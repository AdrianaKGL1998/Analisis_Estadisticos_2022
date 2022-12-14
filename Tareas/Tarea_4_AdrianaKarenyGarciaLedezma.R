#Adriana Kareny Garc?a Ledezma
# 01/09/2022
# Tarea 4



# Problema 1 --------------------------------------------------------------

set.seed(9875)
size <- 1000
x2 <- round(runif(n = size, min = 0, max = 10), 2)


hx <- hist(x2, las = 1, col = 'pink')
hx
hx$breaks

hx <- hist(x2, xaxt = "n",
          breaks = c(0,2,4,6,8,10),
          col = "cyan",)
axis(1, hx$mids)


hx2 <- hist(x2, xaxt = "n",
           breaks = c(0,1,2,4,7,10),
           col = "orange",)
axis(1, hx2$mids)


# Problema 2 --------------------------------------------------------------

#a. ?Cu?l distribuci?n parece estar sesgada a la derecha? A
#b. ?Cu?l distribuci?n parece estar sesgada a la izquierda? D
#c. ?Cu?l distribuci?n parece ser sim?trica o en forma de "campana"?C
#d. ?Cu?l distribuci?n parece ser bimodal?B
#e. ?Cu?l distribuci?n parece mostrar una falta de intervalos? A,B,D


# Problema 3 --------------------------------------------------------------


data(quakes)
tabla <- table(quakes$mag)

mags <-  hist(quakes$mag, xaxt = "n",
              # breaks = c(en caso de necesitar aqui se puede especificar),
              col = "#e6ac00", 
              xlab="Magnitud de los terremotos",
              ylab= "Frecuencias",main = "",
              las = 1,
              ylim = c(0,260))
axis(1, mags$mids)

#a. ?C?mo describir?a la forma de esta distribuci?n de las magnitudes de los terremotos?
#Respuesta= sesgada a la derecha
#b. Mencione un intervalo donde ocurren tipicamente las magnitudes.
#Respuesta =4.2-4.4
#c. Determine el rango de las magnitudes (Range = Max - Min).
Ran <- max(range(quakes))-min(range(quakes))
Ran
#d. ?Qu? porcentaje de los terremotos ocurren con magnitud en la clase 5.3 (5.1 : 5.4)?

library(dplyr)

magnitu <- quakes%>%
filter(mag =="5.3")%>%
select(mag,stations)

porcent_5.3 <- (length(magnitu$mag)/length(quakes$mag))*100
porcent_5.3

#e. ?Qu? porcentaje de los terremotos tiene una magnitud igual o mayor a 5.0?
magnitu2 <- quakes%>%
  filter(mag >="5")%>%
  select(mag,stations)
magnitu2

porcent_5 <- (length(magnitu2$mag)/length(quakes$mag))*100
porcent_5
#f. ?Qu? porcentaje de los terremotos tienen una magnitud menor o igual a 4.6?
magnitu3 <- quakes%>%
  filter(mag <="4.6")%>%
  select(mag,stations)
magnitu3

porcent_5 <- (length(magnitu3$mag)/length(quakes$mag))*100
porcent_5


# Problema 4 --------------------------------------------------------------

#B


# Problema 5 --------------------------------------------------------------

#a) ?Cu?l especie tiene el di?metro m?s peque?o? F  
#b) ?Cu?l especie tiene el di?metro m?s grande? H  
#c) ?Cu?l especie tiene el di?metro m?nimo m?s alto? F  
#d) ?Cu?l especie tiene la mediana de di?metro m?s peque?a? C 
#e) ?Cu?l especie tiene la mediana de di?metro mas grande? H 
#f) ?Cu?l especie tiene el menor rango de di?metro? F 
#g) ?Cu?l especie tiene el rango intercuantil (Q3-Q1) mas grande? F
#h) ?Cu?l especie tiene el rango intercuantil (Q3-Q1) mas peque?o? C
#i) Cu?l especie tiene una distribuci?n sim?trica? Ninguna
#j) ?Cu?l especie tiene el sesgo positivo (ver Fig. 2) m?s marcado ? Ninguno ya que son simetricos


# Problema 6 --------------------------------------------------------------

fires <- c(78, 44, 47, 105, 126, 181, 277, 210, 155)
fires
#Valor minimo
minimo <- min(fires)
#Valor m?ximo
maximo <- max(fires)
#Rango
rango <- max(range(fires))-min(range(fires))
rango
#Q1 (25 %)
quantile(fires,0.25)
#Q2 (50 %)
quantile(fires,0.50)
#Q3 (75 %)
quantile(fires,0.75)
#Media
mean(fires)
#Varianza
var(fires)
#Desviaci?n est?ndar
sd(fires)
#Realice un boxplot 
boxplot(fires, main ="Incendios Forestales")
