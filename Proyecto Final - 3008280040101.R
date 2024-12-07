# Instalar las bibliotecas necesarias
install.packages("haven")  # Para leer archivos .sav de SPSS
install.packages("dplyr")  # Para manipulación y transformación de datos
install.packages("arules") # Para aplicar el algoritmo Apriori
install.packages("https://mhahsler.github.io/arules/docs/fim4r/fim4r_latest.tar.gz", repos = NULL, type = "source")  # Para el algoritmo FP-Growth
install.packages("ggplot2") # Para graficar
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")


# Cargar las bibliotecas
library(haven)    # Carga la librería haven para trabajar con archivos .sav
library(dplyr)    # Carga dplyr para manipulación de datos
library(arules)   # Carga arules para aplicar el algoritmo Apriori
library(ggplot2)  # Carga ggplot2 para gráficos de KMeans
library(fim4r)    # Carga fim4r para aplicar el algoritmo FP-Growth
library(rpart)
library(rpart.plot)
library(randomForest)




# 1. Cargar Matrimonios:

# Ruta a la carpeta que contiene los archivos .sav
carpeta <- "D:/Datos Matrimonios y Divorcios/Matrimonios"

# Obtener una lista de todos los archivos .sav en la carpeta
archivos_sav <- list.files(path = carpeta, pattern = "\\.sav$", full.names = TRUE)

# Leer cada archivo .sav y almacenarlo en una lista
lista_datos <- lapply(archivos_sav, read_sav)

# Convertir las columnas CIUOHOM y CIUOMUJ a tipo character para evitar problemas de conversión
lista_datos <- lapply(lista_datos, function(df) {
  df <- mutate(df,
               CIUOHOM = as.character(CIUOHOM),
               CIUOMUJ = as.character(CIUOMUJ))  # Convertimos CIUOHOM y CIUOMUJ a character
  return(df)
})

# Combinar todos los dataframes en un solo dataframe
combined_df <- bind_rows(lista_datos)




# 2. Cargar Divorcios:

# Ruta a la carpeta que contiene los archivos .sav para los divorcios
carpeta <- "D:/Datos Matrimonios y Divorcios/Divorcios"

# Obtener una lista de todos los archivos .sav en la carpeta
archivos_sav2 <- list.files(path = carpeta, pattern = "\\.sav$", full.names = TRUE)

# Leer cada archivo .sav y almacenarlo en una lista
lista_datos2 <- lapply(archivos_sav2, read_sav)

# Convertir las columnas CIUOHOM y CIUOMUJ a tipo character
lista_datos2 <- lapply(lista_datos2, function(df2) {
  df2 <- mutate(df2,
                CIUOHOM = as.character(CIUOHOM),
                CIUOMUJ = as.character(CIUOMUJ))  # Convertimos CIUOHOM y CIUOMUJ a character
  return(df2)
})

# Combinar todos los dataframes de divorcios en un solo dataframe
combined_df2 <- bind_rows(lista_datos2)