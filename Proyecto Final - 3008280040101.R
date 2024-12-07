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


# 3. Predicción por medio de árboles de decisión



# 3.1. Árbol de Decisión: Predicción de Clase Unión
arbol1 <- rpart(ESCHOM ~ PUEHOM + PUEMUJ + ESCMUJ, 
                data = combined_df, 
                method = "class")

# Visualizar el árbol de decisión
rpart.plot(arbol1, 
           type = 2, 
           extra = 102,  # Muestra porcentajes en los nodos
           fallen.leaves = TRUE, 
           box.palette = "BuGn", 
           main = "Árbol de Decisión: Escolaridad Hombre")

# Escenarios para prueba
escenario1 <- data.frame(PUEHOM = 4, PUEMUJ = 1, ESCMUJ = 2)
pred1 <- predict(arbol1, escenario1, type = "class")
print(pred1)
print(as.character(pred1))

escenario2 <- data.frame(PUEHOM = 4, PUEMUJ = 4, ESCMUJ = 4)
pred2 <- predict(arbol1, escenario2, type = "class")
print(pred2)
print(as.character(pred2))

escenario3 <- data.frame(PUEHOM = 1, PUEMUJ = 1, ESCMUJ = 5)
pred3 <- predict(arbol1, escenario3, type = "class")
print(pred3)
print(as.character(pred3))

escenario4 <- data.frame(PUEHOM = 2, PUEMUJ = 4, ESCMUJ = 1)
pred4 <- predict(arbol1, escenario4, type = "class")
print(pred4)
print(as.character(pred4))




# 3.2. Árbol de Decisión: Departamento de Ocupación
arbol2 <- rpart(DEPOCU ~ PUEHOM + PUEMUJ + ESCHOM + ESCMUJ, 
                data = combined_df, 
                method = "class")

# Visualizar el árbol de decisión
rpart.plot(arbol2, 
           type = 2, 
           extra = 102,  # Muestra porcentajes en los nodos
           fallen.leaves = TRUE, 
           box.palette = "BuGn", 
           main = "Árbol de Decisión: Departamento de Ocupación")

# Escenarios para prueba
escenario1a <- data.frame(PUEHOM = 4, PUEMUJ = 1, ESCHOM = 2, ESCMUJ = 1)
pred1a <- predict(arbol2, escenario1a, type = "class")
print(pred1a)
print(as.character(pred1a))

escenario2a <- data.frame(PUEHOM = 4, PUEMUJ = 4, ESCHOM = 5, ESCMUJ = 4)
pred2a <- predict(arbol2, escenario2a, type = "class")
print(pred2a)
print(as.character(pred2a))

escenario3a <- data.frame(PUEHOM = 1, PUEMUJ = 1, ESCHOM = 3, ESCMUJ = 4)
pred3a <- predict(arbol2, escenario3a, type = "class")
print(pred3a)
print(as.character(pred3a))

escenario4a <- data.frame(PUEHOM = 1, PUEMUJ = 4, ESCHOM = 4, ESCMUJ = 2)
pred4a <- predict(arbol2, escenario4a, type = "class")
print(pred4a)
print(as.character(pred4a))




# 3.3. Árbol de Decisión: Predicción de Edad Hombre
arbol3 <- rpart(EDADHOM ~ PUEMUJ + PUEHOM + ESCHOM + ESCMUJ + EDADMUJ, 
                data = combined_df, 
                method = "class")

# Visualizar el árbol de decisión
rpart.plot(arbol3, 
           type = 2, 
           extra = 102,  # Muestra porcentajes en los nodos
           fallen.leaves = TRUE, 
           box.palette = "BuGn", 
           main = "Árbol de Decisión: Edad Hombre")

# Escenarios para prueba
escenario1b <- data.frame(PUEMUJ = 1, PUEHOM = 4, ESCHOM = 4, ESCMUJ = 4, EDADMUJ = 45)
pred1b <- predict(arbol3, escenario1b, type = "class")
print(pred1b)
print(as.character(pred1b))

escenario2b <- data.frame(PUEMUJ = 4, PUEHOM = 4, ESCHOM = 3, ESCMUJ = 4, EDADMUJ = 16)
pred2b <- predict(arbol3, escenario2b, type = "class")
print(pred2b)
print(as.character(pred2b))

escenario3b <- data.frame(PUEMUJ = 1, PUEHOM = 1, ESCHOM = 2, ESCMUJ = 2, EDADMUJ = 23)
pred3b <- predict(arbol3, escenario3b, type = "class")
print(pred3b)
print(as.character(pred3b))

escenario4b <- data.frame(PUEMUJ = 4, PUEHOM = 2, ESCHOM = 3, ESCMUJ = 3, EDADMUJ = 25)
pred4b <- predict(arbol3, escenario4b, type = "class")
print(pred4b)
print(as.character(pred4b))




# 3.4. Árbol de Decisión: EDADMUJ
arbol4 <- rpart(EDADMUJ ~ PUEMUJ + PUEHOM + ESCHOM + ESCMUJ + EDADHOM, 
                data = combined_df, 
                method = "class")

# Visualizar el árbol de decisión
rpart.plot(arbol4, 
           type = 2, 
           extra = 102,  # Muestra porcentajes en los nodos
           fallen.leaves = TRUE, 
           box.palette = "BuGn", 
           main = "Árbol de Decisión:Edad de la Mujer")

# Escenarios para prueba
escenario1c <- data.frame(PUEHOM = 4, PUEMUJ = 1, ESCHOM = 2, ESCMUJ = 1, EDADHOM = 41)
pred1c <- predict(arbol4, escenario1c, type = "class")
print(pred1c)
print(as.character(pred1c))

escenario2c <- data.frame(PUEHOM = 4, PUEMUJ = 4, ESCHOM = 5, ESCMUJ = 4, EDADHOM = 19)
pred2c <- predict(arbol4, escenario2c, type = "class")
print(pred2c)
print(as.character(pred2c))

escenario3c <- data.frame(PUEHOM = 1, PUEMUJ = 1, ESCHOM = 3, ESCMUJ = 4, EDADHOM = 26)
pred3c <- predict(arbol4, escenario3c, type = "class")
print(pred3c)
print(as.character(pred3c))

escenario4c <- data.frame(PUEHOM = 1, PUEMUJ = 4, ESCHOM = 4, ESCMUJ = 2, EDADHOM = 31)
pred4c <- predict(arbol4, escenario4c, type = "class")
print(pred4c)
print(as.character(pred4c))