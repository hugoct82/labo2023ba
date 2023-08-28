rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")

PARAM <- list()
PARAM$semilla <- 102191

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa

# particionar( data=dataset, division=c(70,30),
#  agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30

particionar <- function(
    data, division, agrupa = "",
    campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

# Aqui se debe poner la carpeta de la computadora local
# Establezco el Working Directory
setwd("C:\\Users\\lenov\\Documents\\Maestria_CD_2023\\Laboratorio_1\\")

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]


# particiono estratificadamente el dataset
# Cambiar por la primer semilla de cada uno !
particionar(dataset, division = c(7, 3), 
  agrupa = "clase_ternaria", seed = PARAM$semilla) # aqui se usa SU semilla


param_basicos <- list(
  "cp" = -1, # complejidad minima
  "minsplit" = 600, # minima cantidad de regs en un nodo para hacer el split
  "minbucket" = 13, # minima cantidad de regs en una hoja
  "maxdepth" = 9 # profundidad máxima del arbol
)

# genero el modelo
# quiero predecir clase_ternaria a partir del resto
# fold==1  es training,  el 70% de los datos
modelo <- rpart("clase_ternaria ~ .",
  data = dataset[fold == 1],
  xval = 0,
  control = param_basicos # aqui van los parametros
)


# aplico el modelo a los datos de testing
prediccion <- predict(modelo, # el modelo que genere recien
  dataset[fold == 2], # fold==2  es testing, el 30% de los datos
  type = "prob"
) # type= "prob"  es que devuelva la probabilidad

# prediccion es una matriz con TRES columnas,
#  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego una columna que es la de las ganancias
dataset[, ganancia := ifelse(clase_ternaria == "BAJA+2", 117000, -3000)]

# para testing agrego la probabilidad
dataset[fold == 2, prob_baja2 := prediccion[, "BAJA+2"]]

# calculo la ganancia en testing  qu es fold==2
ganancia_test <- dataset[fold == 2 & prob_baja2 > 0.025, sum(ganancia)]

# escalo la ganancia como si fuera todo el dataset
ganancia_test_normalizada <- ganancia_test / 0.3

estimulos <- dataset[fold == 2 & prob_baja2 > 0.025, .N]
aciertos <- dataset[fold == 2 & prob_baja2 > 0.025 & clase_ternaria == "BAJA+2", .N]


cat("Testing total: ", dataset[fold == 2, .N], "\n")
cat("Testing BAJA+2: ", dataset[fold == 2 & clase_ternaria == "BAJA+2", .N], "\n")

cat("Estimulos: ", estimulos, "\n")
cat("Aciertos (BAJA+2): ", aciertos, "\n")

cat("Ganancia en testing (normalizada): ", ganancia_test_normalizada, "\n")



#NUEVA CORRIDA
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]
#Este dataset solo tiene los valores de clase ternaria
#trabajo solo con los datos con clase, es decir 202107
dataset_1 <- dataset

#aux<- skimr::skim(dataset)

#genero nuevo codigo para asignar variable categoricas
variables_factor <- c("active_quarter",	"cliente_vip",	"internet",	"tcuentas",	"cdescubierto_preacordado",
                      "tcallcenter",	"thomebanking",	"tmobile_app",	"Master_delinquency",
                      "Master_status",	"Visa_delinquency",	"Visa_status")

# Reemplazar valores NA por 99 en las columnas especificadas en "variables_factor"
dataset_1[, (variables_factor) := lapply(.SD, as.factor), .SDcols = variables_factor]

dataset_1[, (variables_factor) := lapply(.SD, function(x) ifelse(is.na(x), 99, x)), .SDcols = variables_factor]





#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# particiono estratificadamente el dataset_1
# Cambiar por la primer semilla de cada uno !
particionar(dataset_1, division = c(7, 3), 
            agrupa = "clase_ternaria", seed = PARAM$semilla) # aqui se usa SU semilla


param_basicos <- list(
  "cp" = -1, # complejidad minima
  "minsplit" = 600, # minima cantidad de regs en un nodo para hacer el split
  "minbucket" = 13, # minima cantidad de regs en una hoja
  "maxdepth" = 9 # profundidad máxima del arbol
)

# genero el modelo
# quiero predecir clase_ternaria a partir del resto
# fold==1  es training,  el 70% de los datos
modelo <- rpart("clase_ternaria ~ .",
                data = dataset_1[fold == 1],
                xval = 0,
                control = param_basicos # aqui van los parametros
)


# aplico el modelo a los datos de testing
prediccion <- predict(modelo, # el modelo que genere recien
                      dataset_1[fold == 2], # fold==2  es testing, el 30% de los datos
                      type = "prob"
) # type= "prob"  es que devuelva la probabilidad

# prediccion es una matriz con TRES columnas,
#  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego una columna que es la de las ganancias
dataset_1[, ganancia := ifelse(clase_ternaria == "BAJA+2", 117000, -3000)]

# para testing agrego la probabilidad
dataset_1[fold == 2, prob_baja2 := prediccion[, "BAJA+2"]]

# calculo la ganancia en testing  que es fold==2
ganancia_test <- dataset_1[fold == 2 & prob_baja2 > 0.025, sum(ganancia)]

# escalo la ganancia como si fuera todo el dataset
ganancia_test_normalizada <- ganancia_test / 0.3 
##-------------
##-------------
#(Consultar si es porque tome 70-30 y porque se puede hacer así?)
##-------------
##-------------

##-------------
#--------------
##el .N se usa para contar
##-------------
#--------------
estimulos <- dataset_1[fold == 2 & prob_baja2 > 0.025, .N]
aciertos <- dataset_1[fold == 2 & prob_baja2 > 0.025 & clase_ternaria == "BAJA+2", .N]


cat("Testing total: ", dataset_1[fold == 2, .N], "\n")
cat("Testing BAJA+2: ", dataset_1[fold == 2 & clase_ternaria == "BAJA+2", .N], "\n")

cat("Estimulos: ", estimulos, "\n")
cat("Aciertos (BAJA+2): ", aciertos, "\n")

cat("Ganancia en testing (normalizada): ", ganancia_test_normalizada, "\n")


# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
       file = "./exp/KA2001/K101_001.csv",
       sep = ","
)
