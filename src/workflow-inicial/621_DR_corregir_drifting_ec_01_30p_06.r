# Experimentos Colaborativos Default
# Workflow  Data Drifting repair

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")


# Parametros del script
PARAM <- list()
PARAM$experimento <- "DR6210_EC_06"

PARAM$exp_input <- "CA6110_EC_06"

PARAM$variables_intrames <- TRUE # atencion esto esta en TRUE

# valores posibles
#  "ninguno", "rank_simple", "rank_cero_fijo", "deflacion", "estandarizar"
PARAM$metodo <- "deflacion"

PARAM$home <- "~/buckets/b1/"
# FIN Parametros del script

OUTPUT <- list()

#------------------------------------------------------------------------------

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})
#------------------------------------------------------------------------------

GrabarOutput <- function() {
  write_yaml(OUTPUT, file = "output.yml") # grabo output
}
#------------------------------------------------------------------------------
# Esta es la parte que los alumnos deben desplegar todo su ingenio
# Agregar aqui sus PROPIAS VARIABLES manuales

AgregarVariables_IntraMes <- function(dataset) {
  gc()
  # INICIO de la seccion donde se deben hacer cambios con variables nuevas

  # creo un ctr_quarter que tenga en cuenta cuando
  # los clientes hace 3 menos meses que estan
  dataset[, ctrx_quarter_normalizado := ctrx_quarter]
  dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5]
  dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2]
  dataset[
    cliente_antiguedad == 3,
    ctrx_quarter_normalizado := ctrx_quarter * 1.2
  ]

  # variable extraida de una tesis de maestria de Irlanda
  dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]

  # se crean los nuevos campos para MasterCard  y Visa,
  #  teniendo en cuenta los NA's
  # varias formas de combinar Visa_status y Master_status
  dataset[, vm_status01 := pmax(Master_status, Visa_status, na.rm = TRUE)]
  dataset[, vm_status02 := Master_status + Visa_status]

  dataset[, vm_status03 := pmax(
    ifelse(is.na(Master_status), 10, Master_status),
    ifelse(is.na(Visa_status), 10, Visa_status)
  )]

  dataset[, vm_status04 := ifelse(is.na(Master_status), 10, Master_status)
    + ifelse(is.na(Visa_status), 10, Visa_status)]

  dataset[, vm_status05 := ifelse(is.na(Master_status), 10, Master_status)
    + 100 * ifelse(is.na(Visa_status), 10, Visa_status)]

  dataset[, vm_status06 := ifelse(is.na(Visa_status),
    ifelse(is.na(Master_status), 10, Master_status),
    Visa_status
  )]

  dataset[, mv_status07 := ifelse(is.na(Master_status),
    ifelse(is.na(Visa_status), 10, Visa_status),
    Master_status
  )]


  # combino MasterCard y Visa
  dataset[, vm_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]

  dataset[, vm_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]
  dataset[, vm_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]
  dataset[, vm_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]
  dataset[, vm_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]
  dataset[, vm_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]
  dataset[, vm_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]
  dataset[, vm_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]
  dataset[, vm_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]
  dataset[, vm_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]
  dataset[, vm_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]
  dataset[, vm_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]
  dataset[, vm_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]
  dataset[, vm_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]
  dataset[, vm_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]
  dataset[, vm_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]
  dataset[, vm_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]
  dataset[, vm_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]
  dataset[, vm_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]
  dataset[, vm_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]

  # a partir de aqui juego con la suma de Mastercard y Visa
  dataset[, vmr_Master_mlimitecompra := Master_mlimitecompra / vm_mlimitecompra]
  dataset[, vmr_Visa_mlimitecompra := Visa_mlimitecompra / vm_mlimitecompra]
  dataset[, vmr_msaldototal := vm_msaldototal / vm_mlimitecompra]
  dataset[, vmr_msaldopesos := vm_msaldopesos / vm_mlimitecompra]
  dataset[, vmr_msaldopesos2 := vm_msaldopesos / vm_msaldototal]
  dataset[, vmr_msaldodolares := vm_msaldodolares / vm_mlimitecompra]
  dataset[, vmr_msaldodolares2 := vm_msaldodolares / vm_msaldototal]
  dataset[, vmr_mconsumospesos := vm_mconsumospesos / vm_mlimitecompra]
  dataset[, vmr_mconsumosdolares := vm_mconsumosdolares / vm_mlimitecompra]
  dataset[, vmr_madelantopesos := vm_madelantopesos / vm_mlimitecompra]
  dataset[, vmr_madelantodolares := vm_madelantodolares / vm_mlimitecompra]
  dataset[, vmr_mpagado := vm_mpagado / vm_mlimitecompra]
  dataset[, vmr_mpagospesos := vm_mpagospesos / vm_mlimitecompra]
  dataset[, vmr_mpagosdolares := vm_mpagosdolares / vm_mlimitecompra]
  dataset[, vmr_mconsumototal := vm_mconsumototal / vm_mlimitecompra]
  dataset[, vmr_mpagominimo := vm_mpagominimo / vm_mlimitecompra]

#   # Aqui debe usted agregar sus propias nuevas variables
#   dataset[, mtotal_income := rowSums(.SD, na.rm = TRUE), .SDcols = c("mpayroll", "mpayroll2")] #1

#   dataset[, mtotal_expense := rowSums(.SD, na.rm = TRUE), .SDcols = c("Master_mconsumototal","Visa_mconsumototal",
#                                                                     "mcuenta_debitos_automaticos", "mpagodeservicios",
#                                                                     "mcomisiones_mantenimiento","ccomisiones_otras",
#                                                                     "mcomisiones_otras")] #2

#   dataset[, msavings := ifelse(!is.na(mtotal_income) | !is.na(mtotal_expense), mtotal_income - mtotal_expense, NA)] #3

#   dataset[, saving_rate := msavings / mtotal_income] #4

#   dataset[, mvm_consumo_tc_total := rowSums(.SD, na.rm = TRUE), .SDcols = c("Master_mconsumospesos", "Master_mconsumosdolares",
#                                                                             "Visa_mconsumospesos","Visa_mconsumosdolares")] #5
#   dataset[, vm_consumo_ingreso := mvm_consumo_tc_total  / mtotal_income]  #6

#   dataset[, atm_ext_rate := ccajas_extracciones / ctrx_quarter] #7
    
#   dataset[, mtotal_investment := rowSums(.SD, na.rm = TRUE), .SDcols = c("minversion1_pesos", "minversion1_dolares",
#                                                                         "minversion2")] #8

#   dataset[, inv_o_income := mtotal_investment / mtotal_income] #9

#   dataset[, income_type := ifelse(!is.na(cpayroll_trx), ifelse(cpayroll_trx > 1, 10, 90), NA)] #10 - se mod a 10,90


#   dataset[, mtotal_descuentos_tc := rowSums(.SD, na.rm = TRUE), .SDcols = c("mtarjeta_visa_descuentos",
#                                                                             "mtarjeta_master_descuentos")] #11

#   dataset[, mtotal_consumo := rowSums(.SD, na.rm = TRUE), .SDcols = c("Visa_mconsumototal",
#                                                                     "Master_mconsumototal")] #12

#   dataset[, discount_usage_total := mtotal_descuentos_tc/mtotal_consumo] #13
#   dataset[, mobile_transactions := cmobile_app_trx/ctrx_quarter] #14
#   dataset[, extraction_rate_own := catm_trx/cextraccion_autoservicio] #15
#   dataset[, extraction_rate_other := catm_trx_other/cextraccion_autoservicio] #16
    
#   dataset[, mtotal_debt := rowSums(.SD, na.rm = TRUE), .SDcols = c("mprestamos_hipotecarios",
#                                                                     "mprestamos_prendarios",
#                                                                     "mprestamos_personales")] #17

#   dataset[, acid_rate := mcuentas_saldo/mtotal_debt] #18

#   dataset[, rentabilidad_prom_prod := mrentabilidad_annual / cproductos] #19

#   dataset[, mtotal_prestamos := rowSums(.SD, na.rm = TRUE), .SDcols = c("cprestamos_personales",
#                                                                       "cprestamos_prendarios",
#                                                                       "cprestamos_hipotecarios")] #20

#   dataset[, comision_sobre_producto := mcomisiones/ cproductos] #21
#   dataset[, ganancia_activa := mactivos_margen/mrentabilidad_annual] #22
#   dataset[, ganancia_pasiva := mpasivos_margen/mrentabilidad_annual] #23
#   dataset[, dolares_rate := mcaja_ahorro_dolares/mcuentas_saldo] #24
#   dataset[, prom_trans_debito := mautoservicio/ctarjeta_debito_transacciones] #25

#   dataset[, mtotal_trans_tc := rowSums(.SD, na.rm = TRUE), .SDcols = c("ctarjeta_visa_transacciones",
#                                                                       "ctarjeta_master_transacciones")] #26

#   dataset[, mtotal_trans_tc_moneda := rowSums(.SD, na.rm = TRUE), .SDcols = c("mtarjeta_master_consumo",
#                                                                             "mtarjeta_visa_consumo")] #27

#   dataset[, rate_trans_tc := mtotal_trans_tc_moneda/mtotal_trans_tc] #28
#   dataset[, in_trans_prom := mtransferencias_recibidas/ctransferencias_recibidas] #29
#   dataset[, out_trans_prom := mtransferencias_emitidas/ctransferencias_emitidas] #30
#   #dataset[, cheques_dep_prom := mcheques_depositados/ccheques_depositados] #31
#   #dataset[, cheques_emit_prom := mcheques_emitidos/mcheques_emitidos] #32
#   #dataset[, cheq_rech_emit_rate := ccheques_emitidos_rechazados/ccheques_emitidos_rechazados] #33
#   #dataset[, cheq_rech_dep_rate := ccheques_depositados_rechazados/ccheques_depositados] #34

#   dataset[, mtotal_assets := rowSums(.SD, na.rm = TRUE), .SDcols = c("mtotal_investment",
#                                                                     "mcuentas_saldo")]  #35

#   dataset[, liquidity_Rate := mtotal_assets/mtotal_debt] #36

# #Datos canasta basica
#   foto_mes <- c(
#     201901, 201902, 201903, 201904, 201905, 201906,
#     201907, 201908, 201909, 201910, 201911, 201912, 202001, 202002, 202003, 202004,
#     202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102,
#     202103, 202104, 202105, 202106, 202107, 202108, 202109
#   )
#   Hogar_tipo_1b <- c(
#     21051.65, 21949.28, 22889.09, 23480.38, 24152.45, 24797.76,
#     25423.53, 26282.37, 27692.71, 28379.69, 29930.8, 31016.96, 32141.62, 32473.33, 33432.8, 33909.77,
#     34297, 34878.42, 35444.1, 36205.52, 37589.42, 39735.45, 41219.42, 43155.51, 44947.82, 46172.75,
#     48462.54, 50121.54, 51305.74, 52932.46, 53798.9, 54421.74, 56152.06
#   )
#   datos_canasta <- data.table(foto_mes = foto_mes, Hogar_tipo_1b = Hogar_tipo_1b)

#   setkey(dataset, foto_mes)
#   setkey(datos_canasta, foto_mes)
#   dataset[datos_canasta, `:=`(Hogar_tipo_1b = i.Hogar_tipo_1b)] 


#   sum_mrentabilidad <- dataset[, .(suma_mrentabilidad = sum(mrentabilidad, na.rm = TRUE)), by = foto_mes]
#   setkey(dataset, foto_mes)
#   setkey(sum_mrentabilidad, foto_mes)
#   dataset[sum_mrentabilidad, `:=`(sum_mrentabilidad = i.suma_mrentabilidad)]
#   dataset[, custumer_contribution := mrentabilidad / sum_mrentabilidad * 1e5] #37

#   dataset[, investment_over_debt := mtotal_investment / mtotal_debt] #38


#   total_income_mes <- dataset[, .(total_income_m = sum(mtotal_income, na.rm = TRUE)), by = foto_mes]
#   setkey(dataset, foto_mes)
#   setkey(total_income_mes, foto_mes)
#   dataset[total_income_mes, `:=`(total_income_mes = i.total_income_m)]
#   dataset[, Income_rate := mtotal_income / total_income_mes *1e6] #39 Flag

#   #FUNCIÓN DE PERCENTILES FIJOS
#   # calculate_percentiles <- function(x) {
#   #   quantiles <- quantile(x, probs = c(0.45, 0.65, 0.85), na.rm = TRUE)
#   #   cut_points <- c(-Inf, quantiles[1], quantiles[2], quantiles[3], Inf)
#   #   labels <- c("P0-P45", "P45-P65", "P65-P85", ">P85")
#   #   cut(x, breaks = cut_points, labels = labels, include.lowest = TRUE)
#   # }

#   #Variable de ratio de ingreso
#   # dataset[, Income_rate_ := calculate_percentiles(Income_rate), by = foto_mes]
#   # Income_rate_p <- dcast(dataset[, list(V1=1, numero_de_cliente, foto_mes , Income_rate_), ],
#   #                       numero_de_cliente+foto_mes  ~ paste("Income_rate_",Income_rate_, sep=""),
#   #                       fun=sum,
#   #                       value.var="V1",
#   #                       drop=c(TRUE, FALSE))
#   # Income_rate_p <- as.data.table(Income_rate_p)                       

#   # merged_dataset <- merge(dataset, Income_rate_p, by = c("foto_mes", "numero_de_cliente"), all.x = TRUE)
#   # merged_dataset[, Income_rate_:=NULL]
#   # dataset<<-merged_dataset  #Incluye #40 a 43
#   # rm(merged_dataset)

#   #Variable de ratio de entorno economico
#   #dataset[, environmental_rate := mtotal_income / Hogar_tipo_1b ]  #40
#   #dataset[, Hogar_tipo_1b:=NULL]

#   # dataset[, environmental_rate_ := calculate_percentiles(environmental_rate), by = foto_mes]
#   # environmental_rate_p <- dcast(dataset[, list(V1=1, numero_de_cliente, foto_mes , environmental_rate_), ],
#   #                               numero_de_cliente+foto_mes  ~ paste("environmental_rate_",environmental_rate_, sep=""),
#   #                               fun=sum,
#   #                               value.var="V1",
#   #                               drop=c(TRUE, FALSE))
#   # environmental_rate_p <- as.data.table(environmental_rate_p)                       
#   # merged_dataset_ <- merge(dataset, environmental_rate_p, by = c("foto_mes", "numero_de_cliente"), all.x = TRUE)
#   # merged_dataset_[, environmental_rate_:=NULL]
#   # merged_dataset_[, Hogar_tipo_1b:=NULL]
#   # dataset<<- merged_dataset_  #Incluye #45 a 48
#   # rm(merged_dataset_)

#   #Indicador de uso de descubierto
#   dataset[, descubierto_usage := rowSums(!is.na(.SD) & .SD < 0, na.rm = TRUE),
#    .SDcols = c("mcuentas_saldo", "mcuenta_corriente", "mcuenta_corriente_adicional")] #41
#    #se podria colocar un % del income osea < -0.1*mtotal_income

#   #Indicador de pago aguinaldo
#   #fechas_aguinal <- c(201807, 201812, 201907, 201912, 202007, 202012, 
#   #                    202107, 202112, 202207, 202212, 202307)
#   #dataset[, bonus_aguinaldo_detec := ifelse((foto_mes %in% fechas_aguinal), 0, 1)] #42

#   dataset[, ratio_comi_incom := mcomisiones / mtotal_income] #43

#   dataset[, ratio_deuda_capital  := mtotal_debt / ( mtotal_investment + mtotal_income)] #44

#   #dataset[, ratio_monto_atm_atm_other  := matm/(matm + matm_other)] #45

#   #dataset[, ratio_mov_atm_atm_other  := catm_trx/(catm_trx+catm_trx_other)] #46



  # valvula de seguridad para evitar valores infinitos
  # paso los infinitos a NULOS
  infinitos <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.infinite(get(.name)))]
  )

  infinitos_qty <- sum(unlist(infinitos))
  if (infinitos_qty > 0) {
    cat(
      "ATENCION, hay", infinitos_qty,
      "valores infinitos en tu dataset. Seran pasados a NA\n"
    )
    dataset[mapply(is.infinite, dataset)] <<- NA # Se cambia asignación global a local
  }


  # valvula de seguridad para evitar valores NaN  que es 0/0
  # paso los NaN a 0 , decision polemica si las hay
  # se invita a asignar un valor razonable segun la semantica del campo creado
  nans <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.nan(get(.name)))]
  )

  nans_qty <- sum(unlist(nans))
  if (nans_qty > 0) {
    cat(
      "ATENCION, hay", nans_qty,
      "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n"
    )

    cat("Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0 # Se cambia asignación global a local
  }
}
#------------------------------------------------------------------------------
# deflaciona por IPC
# momento 1.0  31-dic-2020 a las 23:59

drift_deflacion <- function(campos_monetarios) {
  vfoto_mes <- c(
    201901, 201902, 201903, 201904, 201905, 201906,
    201907, 201908, 201909, 201910, 201911, 201912,
    202001, 202002, 202003, 202004, 202005, 202006,
    202007, 202008, 202009, 202010, 202011, 202012,
    202101, 202102, 202103, 202104, 202105, 202106,
    202107, 202108, 202109
  )

  vIPC <- c(
    1.9903030878, 1.9174403544, 1.8296186587,
    1.7728862972, 1.7212488323, 1.6776304408,
    1.6431248196, 1.5814483345, 1.4947526791,
    1.4484037589, 1.3913580777, 1.3404220402,
    1.3154288912, 1.2921698342, 1.2472681797,
    1.2300475145, 1.2118694724, 1.1881073259,
    1.1693969743, 1.1375456949, 1.1065619600,
    1.0681100000, 1.0370000000, 1.0000000000,
    0.9680542110, 0.9344152616, 0.8882274350,
    0.8532444140, 0.8251880213, 0.8003763543,
    0.7763107219, 0.7566381305, 0.7289384687
  )

  tb_IPC <- data.table(
    "foto_mes" = vfoto_mes,
    "IPC" = vIPC
  )

  dataset[tb_IPC,
    on = c("foto_mes"),
    (campos_monetarios) := .SD * i.IPC,
    .SDcols = campos_monetarios
  ]
}

#------------------------------------------------------------------------------

drift_rank_simple <- function(campos_drift) {
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_rank") :=
      (frank(get(campo), ties.method = "random") - 1) / (.N - 1), by = foto_mes]
    dataset[, (campo) := NULL]
  }
}
#------------------------------------------------------------------------------
# El cero se transforma en cero
# los positivos se rankean por su lado
# los negativos se rankean por su lado

drift_rank_cero_fijo <- function(campos_drift) {
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[get(campo) == 0, paste0(campo, "_rank") := 0]
    dataset[get(campo) > 0, paste0(campo, "_rank") :=
      frank(get(campo), ties.method = "random") / .N, by = foto_mes]

    dataset[get(campo) < 0, paste0(campo, "_rank") :=
      -frank(-get(campo), ties.method = "random") / .N, by = foto_mes]
    dataset[, (campo) := NULL]
  }
}
#------------------------------------------------------------------------------

drift_estandarizar <- function(campos_drift) {
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_normal") := 
      (get(campo) -mean(campo, na.rm=TRUE)) / sd(get(campo), na.rm=TRUE),
      by = foto_mes]

    dataset[, (campo) := NULL]
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
OUTPUT$PARAM <- PARAM
OUTPUT$time$start <- format(Sys.time(), "%Y%m%d %H%M%S")

setwd(PARAM$home)

# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input <- paste0("./exp/", PARAM$exp_input, "/dataset.csv.gz")
dataset <- fread(dataset_input)

# creo la carpeta donde va el experimento
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

GrabarOutput()
write_yaml(PARAM, file = "parametros.yml") # escribo parametros utilizados

# primero agrego las variables manuales
if (PARAM$variables_intrames) AgregarVariables_IntraMes(dataset)

# ordeno de esta forma por el ranking
setorder(dataset, foto_mes, numero_de_cliente)

# por como armé los nombres de campos,
#  estos son los campos que expresan variables monetarias
campos_monetarios <- colnames(dataset)
campos_monetarios <- campos_monetarios[campos_monetarios %like%
  "^(m|Visa_m|Master_m|vm_m)"]

# aqui aplico un metodo para atacar el data drifting
# hay que probar experimentalmente cual funciona mejor
switch(PARAM$metodo,
  "ninguno"        = cat("No hay correccion del data drifting"),
  "rank_simple"    = drift_rank_simple(campos_monetarios),
  "rank_cero_fijo" = drift_rank_cero_fijo(campos_monetarios),
  "deflacion"      = drift_deflacion(campos_monetarios),
  "estandarizar"   = drift_estandarizar(campos_monetarios)
)



fwrite(dataset,
  file = "dataset.csv.gz",
  logical01 = TRUE,
  sep = ","
)

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
  file = "dataset.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------
OUTPUT$dataset$ncol <- ncol(dataset)
OUTPUT$dataset$nrow <- nrow(dataset)
OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

# dejo la marca final
cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
  file = "zRend.txt",
  append = TRUE
)
