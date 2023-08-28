# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros
install.packages("doParallel")  # Si no tienes el paquete instalado

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection
require("doParallel")
require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(420551, 589753, 632363, 770437, 81040)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "Clase_binaria1", seed = semilla)

  # genero el modelo
  # quiero predecir clase_ternaria a partir del resto
  modelo <- rpart("Clase_binaria1 ~ .",
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con TRES columnas,
  #  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades
  # se agrega "pos" (BAJA+2) y "neg" (BAJA+1 / CONTINUA)


  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "pos"] > 0.025,
      ifelse(Clase_binaria1 == "pos", 117000, -3000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3

  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------



ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  ksemillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
    semillas, # paso el vector de semillas
    MoreArgs = list(param_basicos), # aqui paso el segundo parametro
    SIMPLIFY = FALSE#,
    #mc.cores = 1
  ) # se puede subir a 5 si posee Linux o Mac OS

  ganancia_promedio <- mean(unlist(ganancias))

  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Users\\lenov\\Documents\\Maestria_CD_2023\\Laboratorio_1\\")  # Establezco el Working Directory
# cargo los datos

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]

#generación de clave binaria

dataset[, nueva_columna := ifelse(
  clase_ternaria %in% c("BAJA+2"), "pos", "neg"
)]
setnames(dataset, "nueva_columna", "Clase_binaria1")
head(dataset)
names(dataset)


dataset[clase_ternaria == "BAJA+1", .N]+dataset[clase_ternaria == "CONTINUA", .N]

dataset[Clase_binaria1 == "neg", .N]
dataset[Clase_binaria1 == "pos", .N]

# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch_04.txt"

# Escribo los titulos al archivo donde van a quedar los resultados
# atencion que si ya existe el archivo, esta instruccion LO SOBREESCRIBE,
#  y lo que estaba antes se pierde
# la forma que no suceda lo anterior es con append=TRUE
cat(
  file = archivo_salida,
  sep = "",
  "max_depth", "\t",
  "min_split", "\t",
  "cp_test", "\t",
  "minbucket", "\t",
  "ganancia_promedio", "\n"
)


# itero por los loops anidados para cada hiperparametro
##-----PRUEBA DE PARALELIZAR


num_cores <- detectCores()
cl <- makeCluster(num_cores)

registerDoParallel(cl)

# Ejemplo de uso con paquete doParallel y foreach
for (vmax_depth in c(7,9,11)) {
  for (vmin_split in c(400,500, 600,800)) {
    for (minbucket in c(10, 30,50,80,110,200,300,400)){
      for (cp_test in c(-1,-0.5,-0.25)) {
        if(vmin_split / minbucket > 2) {
          param_basicos <- list(
            "cp" = cp_test, # complejidad minima
            "minsplit" = vmin_split,
            "minbucket" = minbucket, # minima cantidad de registros en una hoja
            "maxdepth" = vmax_depth
          ) # profundidad máxima del arbol
          
          # Un solo llamado, con la semilla 17
          ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
          
          # escribo los resultados al archivo de salida
          cat(
            file = archivo_salida,
            append = TRUE,
            sep = "",
            vmax_depth, "\t",
            vmin_split, "\t",
            cp_test, "\t",
            minbucket, "\t",
            ganancia_promedio, "\n"
          )
          # notar como se agrega
          
          # vminsplit  minima cantidad de registros en un nodo para hacer el split
        }
      }
    }
  }
}

stopCluster(cl)



##------------FIN DE PRUEBA PARALERIZAR


#-----------NUEVA PRUEBA DE PARALELIZACIÖN- no ver aun, 
#falta corregir nuevo uso del CP dentro de los parametrosq
library(doParallel)
library(foreach)

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(420551, 589753, 632363, 770437, 81040)

# Definir los valores posibles para los parámetros
vmax_depth_values <- c(6, 8, 10,14)
vmin_split_values <- c(1000, 800)
minbucket_values <- c(5, 7)

# Crear todas las combinaciones posibles de parámetros
param_combinations <- expand.grid(
  vmax_depth = vmax_depth_values,
  vmin_split = vmin_split_values,
  minbucket = minbucket_values
)

# Configurar el cluster para el procesamiento paralelo
num_cores <- detectCores()
cl <- makeCluster(num_cores)

registerDoParallel(cl)

# Función para calcular la ganancia promedio
calculate_ganancia_promedio <- function(params) {
  param_basicos <- list(
    "cp" = -0.5,
    "minsplit" = params$vmin_split,
    "minbucket" = params$minbucket,
    "maxdepth" = params$vmax_depth
  )
  
  ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)  # Corrección aquí
  
  return(ganancia_promedio)
}

# Aplicar la función en paralelo a las combinaciones de parámetros
results <- foreach(params = param_combinations, .combine = rbind) %dopar% {
  calculate_ganancia_promedio(params)
}

# Detener el cluster
stopCluster(cl)

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch.txt"

# Agregar los resultados al archivo de salida
output_file <- archivo_salida
write.table(results, file = output_file, sep = "\t", col.names = FALSE, row.names = FALSE, append = TRUE)


##FUNCIONES--------------------

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  
  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_ternaria", seed = semilla)
  
  # genero el modelo
  # quiero predecir clase_ternaria a partir del resto
  modelo <- rpart("clase_ternaria ~ .",
                  data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
                  xval = 0,
                  control = param_basicos
  ) # aqui van los parametros del arbol
  
  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
                        dataset[fold == 2], # fold==2  es testing, el 30% de los datos
                        type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad
  
  # prediccion es una matriz con TRES columnas,
  #  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades
  
  
  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
               ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
               0
    ))
  ]
  
  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3
  
  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------



ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  ksemillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
                        semillas, # paso el vector de semillas
                        MoreArgs = list(param_basicos), # aqui paso el segundo parametro
                        SIMPLIFY = FALSE#,
                        #mc.cores = 1
  ) # se puede subir a 5 si posee Linux o Mac OS
  
  ganancia_promedio <- mean(unlist(ganancias))
  
  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------