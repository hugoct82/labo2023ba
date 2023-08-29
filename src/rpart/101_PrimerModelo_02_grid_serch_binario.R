# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
#file.choose()
# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\lenov\\Documents\\Maestria_CD_2023\\Laboratorio_1\\") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

# Modifico la columna clase_ternaria para que sea binaria y le cambio el nombre: 
setnames(dataset, old = "clase_ternaria", new = "Clase_binaria1")
dataset[Clase_binaria1 == "BAJA+1", Clase_binaria1 := "neg"]
dataset[Clase_binaria1 == "CONTINUA", Clase_binaria1 := "neg"]
dataset[Clase_binaria1 == "BAJA+2", Clase_binaria1 := "pos"]

#Observaciones de clase positiva
dataset[foto_mes == 202107, sum(Clase_binaria1 == "pos") / .N]
#Observaciones de clase negativa
dataset[foto_mes == 202107, sum(Clase_binaria1 == "neg") / .N]

dataset[foto_mes == 202107 , .N]


dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "Clase_binaria1 ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -.25, # esto significa no limitar la complejidad de los splits
        minsplit = 400, # minima cantidad de registros para que se haga el split
        minbucket = 10, # tamaño minimo de una hoja
        maxdepth = 6
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)





# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de "pos"
dapply[, prob_baja2 := prediccion[, "pos"]]


# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

unique(dapply$Predicted)

#Observaciones de clase positiva
dapply[ ,sum(Predicted == 1) / .N]
#Observaciones de clase negativa
dapply[ ,sum(Predicted == 0) / .N]

prediccion[foto_mes == 202107 , .N]




# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_grid_serch_bin_2_m.csv",
        sep = ","
)
