#Adriana Kareny García Ledezma
# 29/08/2022
# Tarea 2


# Importar datos de trabajo -----------------------------------------------


rm(list = ls())
datos <- read.csv("conjunto.csv",header=T)
head(datos)

H.media <- subset(datos$Altura,datos$Altura <=mean(datos$Altura))
H.16 <- subset(datos$Altura,datos$Altura <16.5)


Vecinos_3 <- subset(datos$Vecinos,datos$Vecinos <=3)
Vecinos_4 <- subset(datos$Vecinos,datos$Vecinos >4)

DBH_media <- subset(datos$Diametro,datos$Diametro <mean(datos$Diametro))
DBH_16 <- subset(datos$Diametro,datos$Diametro >16)


Especie_C <- subset(datos$Especie,datos$Especie == "C")
Especie_HF <- subset(datos$Especie,datos$Especie == datos$Especie[c(1, 4)])

DBH_16.9 <-  subset(datos$Diametro,datos$Diametro <=16.9)
H.18 <-  subset(datos$Altura,datos$Altura >18.5)


# Visualización de datos --------------------------------------------------

x11()
hist(datos$Altura, main = "Altura", xlab="Datos Altura",ylab = "Frecuencia", las = 1,col='pink') 
x11()
hist(H.media, main = "H.media", xlab="Datos Altura",ylab = "Frecuencia", las = 1,col='aquamarine') 
x11()
hist(H.16, main = "H.16", xlab="Datos Altura",ylab = "Frecuencia", las = 1,col='cadetblue1') 


x11()
hist(datos$Vecinos, main = "Vecinos", xlab="Datos Vecinos",ylab = "Frecuencia", las = 1,col='green2') 
x11()
hist(Vecinos_3, main = "Vecinos_3", xlab="Datos Vecinos",ylab = "Frecuencia", las = 1,col='goldenrod1') 
x11()
hist(Vecinos_4, main = "Vecinos_4", xlab="Datos Vecinos",ylab = "Frecuencia", las = 1,col='hotpink3') 


x11()
hist(datos$Vecinos, main = "Vecinos", xlab="Datos Vecinos",ylab = "Frecuencia", las = 1,col='green2') 
x11()
hist(Vecinos_3, main = "Vecinos_3", xlab="Datos Vecinos",ylab = "Frecuencia", las = 1,col='goldenrod1') 
x11()
hist(Vecinos_4, main = "Vecinos_4", xlab="Datos Vecinos",ylab = "Frecuencia", las = 1,col='hotpink3') 


x11()
hist(datos$Diametro, main = "Diametro", xlab="Datos Diametro",ylab = "Frecuencia", las = 1,col='honeydew1') 
x11()
hist(DBH_media, main = "DBH_media", xlab="Datos Diametro",ylab = "Frecuencia", las = 1,col='lightblue1') 
x11()
hist(DBH_16, main = "DBH_16", xlab="Datos Diametro",ylab = "Frecuencia", las = 1,col='lightsalmon2') 

# Estadísticas básicas ----------------------------------------------------


mean(datos$Altura);mean(H.media);mean(H.16)
sd(datos$Altura);sd(H.media);sd(H.16)

mean(datos$Vecinos);mean(Vecinos_3);mean(Vecinos_4)
sd(datos$Vecinos);sd(Vecinos_3);sd(Vecinos_4)

mean(datos$Diametro);mean(DBH_media);mean(DBH_16)
sd(datos$Diametro);sd(DBH_media);sd(DBH_16)
