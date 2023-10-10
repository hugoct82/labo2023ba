# limpio la memoria

rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection



require("yaml")

leer_documentos_csv <- function(documentos) {
  lista_dt <- list()
   for (documento in documentos) {
    dt <- fread(documento)
    nombre_archivo <- tools::file_path_sans_ext(basename(documento))
    lista_dt[[nombre_archivo]] <- dt
  }
  return(lista_dt)
}



generar_csv_por_cantidad_registros <- function(data, cantidades) {
  # Iterar a través de las cantidades y generar CSV para cada una
  for (cantidad in cantidades) {
    
    # Crear un nuevo data.table con 1 asignado a los primeros "cantidad" registros
    data[, Predicted := ifelse(1:nrow(data) <= cantidad, 1, 0)]
    data[, promedio_prob:=NULL]
    
    # Generar el nombre del archivo CSV
    nombre_archivo <- paste("resultado_FINAL_07_", cantidad, "_envios.csv", sep = "")
    
    # Escribir el data.table con etiquetas en un archivo CSV
    write.csv(data, nombre_archivo, row.names = FALSE)
  }
}


#Defino mi espacio de trabajo

setwd("~/buckets/b1/exp/ZZ6610_EC_07")

archivos_csv <- list.files(pattern = "^pred.*\\.csv$", full.names = TRUE)

dts <- leer_documentos_csv(archivos_csv)

resultado <- rbindlist(dts, use.names = TRUE, fill = TRUE)

promedio_por_cliente <- resultado[, .(promedio_prob = mean(prob, na.rm = TRUE)), 
                                  by = numero_de_cliente][order(-promedio_prob)]

#elimino la columna promedio_prob

promedio_por_cliente[, promedio_prob:=NULL]

# Defino las cantidades que quiero

cantidades <- c(9500, 9750, 10000,10250, 10500, 10750,11000)

# Llamo a la función con el data.table promedio_por_cliente

generar_csv_por_cantidad_registros(promedio_por_cliente, cantidades)

# limpio la memoria

rm(list = ls(all.names = TRUE)) # remove all objects




###NUVEA VERSION
rm(list = ls(all.names = TRUE)) # remove all objects
require("data.table")

setwd("~/buckets/b1/exp/")

# Obtén la lista de carpetas que comienzan con "zz" en el directorio base
todas_carpetas <- list.dirs(full.names = TRUE, recursive = FALSE)

# Filtra las carpetas que comienzan con "./ZZ"
carpetas_zz <- todas_carpetas[grepl("^\\.\\/ZZ", todas_carpetas)]

carpeta_a_excluir <- "./ZZ6610_ep_01"

# Utiliza setdiff para obtener las carpetas que no contienen la carpeta a excluir
carpetas_zz <- setdiff(carpetas_zz, carpeta_a_excluir)


archivos_csv <- character(0)
for (carpeta in carpetas_zz) {
  archivos_en_carpeta <- list.files(path = carpeta, pattern = "^pred.*\\.csv$", full.names = TRUE)
  archivos_csv <- c(archivos_csv, archivos_en_carpeta)
}


leer_documentos_csv <- function(documentos) {
  lista_dt <- list()
  for (documento in documentos) {
    dt <- fread(documento)
    nombre_archivo <- tools::file_path_sans_ext(basename(documento))
    lista_dt[[nombre_archivo]] <- dt
  }
  return(lista_dt)
}

dts <- leer_documentos_csv(archivos_csv)

resultado <- rbindlist(dts, use.names = TRUE, fill = TRUE)

promedio_por_cliente <- resultado[, .(promedio_prob = mean(prob, na.rm = TRUE)), 
                                  by = numero_de_cliente][order(-promedio_prob)]

promedio_por_cliente[, promedio_prob:=NULL]


#genero nueva carperta sobre la raiz de exp
nueva_carpeta <- "combinado"

# Utilizo dir.create para crear la carpeta
dir.create(nueva_carpeta)

setwd("~/buckets/b1/exp/combinado")


# Defino las cantidades que quiero

cantidades <- c(9500, 9750, 10000,10250, 10500, 10750,11000)
generar_csv_por_cantidad_registros(promedio_por_cliente, cantidades)