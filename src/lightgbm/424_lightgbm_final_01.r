# para correr el Google Cloud
#   8 vCPU
#  16 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "KA4240"

PARAM$input$dataset <- "./datasets/dataset_pequeno.csv"
PARAM$input$training <- c(202107) # meses donde se entrena el modelo
PARAM$input$future <- c(202109) # meses donde se aplica el modelo

PARAM$finalmodel$semilla <- 420551

PARAM$finalmodel$num_iterations <- 559
PARAM$finalmodel$learning_rate <- 0.0100746999
PARAM$finalmodel$feature_fraction <- 0.5144127527
PARAM$finalmodel$min_data_in_leaf <- 505
PARAM$finalmodel$num_leaves <- 44


PARAM$finalmodel$max_bin <- 31

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))
#En esta parte, se crea un vector llamado campos_buenos que contiene
# los nombres de todas las columnas en dataset, excepto las columnas "clase_ternaria" y "clase01"
#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L] #PARAM$input$training <- c(202107) # meses donde se entrena el modelo

#se crea una nueva columna llamada train en el conjunto de datos dataset y se inicializa con el
# valor 0 (0L) para todas las filas. Luego, se actualizan ciertas filas en la columna train a 1 (1L) basándose en una condición

## foto_mes %in% PARAM$input$training: Esta condición verifica si los valores en la columna foto_mes están presentes
# en un conjunto específico de valores almacenados en PARAM$input$training. Si esta condición es verdadera para una
# fila particular, se asigna el valor 1 (1L) a la columna train en esa fila, lo que indica que esa fila se utilizará
# para entrenar un modelo (por ejemplo, en un conjunto de entrenamiento).




#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

#Cuando with = FALSE, R no buscará las columnas en el entorno global y
# considerará solamente las columnas dentro del objeto de datos. 
# Esto es útil cuando deseas asegurarte de que las columnas se tomen 
#exclusivamente del objeto de datos y no se vean afectadas por variables
# con el mismo nombre en el entorno global.



# genero el modelo
# estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo <- lgb.train(
  data = dtrain,
  param = list(
    objective = "binary",
    max_bin = PARAM$finalmodel$max_bin,
    learning_rate = PARAM$finalmodel$learning_rate,
    num_iterations = PARAM$finalmodel$num_iterations,
    num_leaves = PARAM$finalmodel$num_leaves,
    min_data_in_leaf = PARAM$finalmodel$min_data_in_leaf,
    feature_fraction = PARAM$finalmodel$feature_fraction,
    seed = PARAM$finalmodel$semilla
  )
)

#--------------------------------------
# ahora imprimo la importancia de variables
tb_importancia <- as.data.table(lgb.importance(modelo))
archivo_importancia <- "impo.txt"

fwrite(tb_importancia,
  file = archivo_importancia,
  sep = "\t"
)

#--------------------------------------


# aplico el modelo a los datos sin clase
dapply <- dataset[foto_mes == PARAM$input$future]

# aplico el modelo a los datos nuevos
prediccion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE])
)

# genero la tabla de entrega
tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
tb_entrega[, prob := prediccion]

# grabo las probabilidad del modelo
fwrite(tb_entrega,
  file = "prediccion.txt",
  sep = "\t"
)

# ordeno por probabilidad descendente
setorder(tb_entrega, -prob)


# genero archivos con los  "envios" mejores
# deben subirse "inteligentemente" a Kaggle para no malgastar submits
# si la palabra inteligentemente no le significa nada aun
# suba TODOS los archivos a Kaggle
# espera a la siguiente clase sincronica en donde el tema sera explicado

cortes <- seq(8000, 12000, by = 500)
for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]

  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
    file = paste0(PARAM$experimento, "_", envios, ".csv"),
    sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
