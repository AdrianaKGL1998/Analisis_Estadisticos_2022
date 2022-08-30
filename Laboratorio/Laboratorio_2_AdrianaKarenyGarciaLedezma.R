#Adriana Kareny Garcia Ledezma
# 29/08/2022
# Laboratorio 2


# Parte 1 ------------------------------------

##Ingresar datos directo en la consola
trees <- read.csv("DBH_1.csv",header=T)
head(trees)
getwd()

dbh <- c(16.5, 25.3, 22.1, 17.2, 16.1, 8.1, 34.3, 5.4, 5.7, 11.2, 24.1,
         14.5, 7.7, 15.6, 15.9, 10, 17.5, 20.5, 7.8, 27.3,
         9.7, 6.5, 23.4, 8.2, 28.5, 10.4, 11.5, 14.3, 17.2, 16.8)

##Accesar datos de internet

## Datos de URL no seguras (http)
prof_url <- "http://www.profepa.gob.mx/innovaportal/file/7635/1/accionesInspeccionfoanp.csv"
profepa <- read.csv(prof_url)
head(profepa)

prof_url_2 <- paste0("http://www.profepa.gob.mx/innovaportal/",
                     "file/7635/1/accionesInspeccionfoanp.csv")
profepa2 <- read.csv(prof_url_2)
head(profepa2)

## Datos de URL seguras (https): Dropbox y Github
library(repmis) 
conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")
head(conjunto) # muestra las primeras seis filas de la BD


library(readr)
file <- paste0("https://raw.githubusercontent.com/mgtagle/",
               "202_Analisis_Estadistico_2020/master/cuadro1.csv")
inventario <- read_csv(file)
head(inventario)


# Parte 2: Operaciones con la base de datos -------------------------------

mean(trees$dbh)
sd(trees$dbh)

# Indica la sumatoria de los individuos en el objeto tree con un dbh < a 10
sum(trees$dbh < 10)

which(trees$dbh < 10)

trees.13 <- trees[!(trees$parcela=="2"),]
trees.13

trees.1 <- subset(trees, dbh <= 10)
head(trees.1)

mean(trees$dbh)
mean(trees.1$dbh)

v1 <- trees[2:30,2]
hist(v1, main = "Muestra original trees", xlab = "DBH",
     ylab = "Frecuencia")

v2 <- trees.1[2:9,2]
hist(v2, main = "dbh < 10 cm. trees.1", xlab = "DBH",
     ylab = "Frecuencia",ylim = c(0,2))


# Parte 3 Representacion grafica ------------------------------------------

mamifero <- read.csv("https://www.openintro.org/data/csv/mammals.csv")

hist(mamifero$total_sleep)

hist(mamifero$total_sleep, # Datos
     xlim = c(0,20), ylim = c(0,14), # Cambiar los limites de x & y
     main = "Total de horas sueño de las 39 especies", # Cambiar el titulo
     xlab = "Horas sueño", # Cambiar eje de las x
     ylab = "Frecuencia", # Cambiar eje de las y
     las = 1, # Cambiar orientacion de y
     col = "#996600") # Cambiar color de las barras


data("chickwts")#base de datos de r anteriormente guardada
head(chickwts[c(1:2,42:43, 62:64), ])

feeds <- table(chickwts$feed)#hagrupar en frecuencias es una tabla de frecuencia
feeds

barplot(feeds)
barplot(feeds[order(feeds, decreasing = TRUE)])

barplot(feeds[order(feeds, decreasing = F)],horiz=T, las=1,col = colorRampPalette(c('goldenrod2','gold','khaki1'))(6),xlab="Número de Pollos",main = "Frecuencias por tipos de alimentación")
