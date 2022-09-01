#Adriana Kareny Garcia Ledezma
# 29/08/2022
# Tarea 2


# Importar datos de trabajo -----------------------------------------------


rm(list = ls())
conjunto<- read.csv("cuadro1.csv",header=T)
head(conjunto)


# Selección de datos ------------------------------------------------------


H.media <- subset(conjunto$Altura,conjunto$Altura <=mean(conjunto$Altura))
H.media
H.16 <- subset(conjunto$Altura,conjunto$Altura <16.5)
H.16 

Vecinos_3 <- subset(conjunto$Vecinos,conjunto$Vecinos <=3)
Vecinos_3
Vecinos_4 <- subset(conjunto$Vecinos,conjunto$Vecinos >4)
Vecinos_4

DBH_media <- subset(conjunto$Diametro,conjunto$Diametro <mean(conjunto$Diametro))
DBH_media
DBH_16 <- subset(conjunto$Diametro,conjunto$Diametro >16)
DBH_16

Especie_C <- subset(conjunto$Especie,conjunto$Especie == "C")
Especie_C
Especie_HF <- subset(conjunto$Especie,conjunto$Especie == conjunto$Especie[c(1, 4)])
Especie_HF

DBH_16.9 <-  subset(conjunto$Diametro,conjunto$Diametro <=16.9)
H.18 <-  subset(conjunto$Altura,conjunto$Altura >18.5)


# Visualización de datos --------------------------------------------------


hist(conjunto$Altura, main = "Altura", xlab="Datos Altura",ylab = "Frecuencia", las = 1,col='#97FFFF') 
hist(H.media, main = "H.media", xlab="Datos Altura <= Media",ylab = "Frecuencia", las = 1,col='#8DEEEE') 
hist(H.16, main = "H.16", xlab="Datos Altura < 16.5",ylab = "Frecuencia", las = 1,col='#528B8B') 


hist(conjunto$Vecinos, main = "Vecinos", xlab="Datos Vecinos",ylab = "Frecuencia", las = 1,col='#CAFF70') 
hist(Vecinos_3, main = "Vecinos_3", xlab="Datos Vecinos <= 3",ylab = "Frecuencia", las = 1,col='#CAFF70') 
hist(Vecinos_4, main = "Vecinos_4", xlab="Datos Vecinos > 4",ylab = "Frecuencia", las = 1,col='#A2CD5A') 


hist(conjunto$Diametro, main = "Diametro", xlab="Datos Diametro",ylab = "Frecuencia", las = 1,col='#FFAEB9') 
hist(DBH_media, main = "DBH_media", xlab="Datos Diametro < Media",ylab = "Frecuencia", las = 1,col='#CD8C95') 
hist(DBH_16, main = "DBH_16", xlab="Datos Diametro >16",ylab = "Frecuencia", las = 1,col='#8B5F65') 

# Estadadisticas basicas ----------------------------------------------------


mean(conjunto$Altura);mean(H.media);mean(H.16)
sd(conjunto$Altura);sd(H.media);sd(H.16)

mean(conjunto$Vecinos);mean(Vecinos_3);mean(Vecinos_4)
sd(conjunto$Vecinos);sd(Vecinos_3);sd(Vecinos_4)

mean(conjunto$Diametro);mean(DBH_media);mean(DBH_16)
sd(conjunto$Diametro);sd(DBH_media);sd(DBH_16)


# Líneas de comando en R --------------------------------------------------

library(repmis)
conjunto1 <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")
head(conjunto1)