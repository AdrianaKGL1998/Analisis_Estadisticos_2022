#Adriana Kareny García Ledezma
# 31/08/2022
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

#a. ¿Cuál distribución parece estar sesgada a la derecha? A
#b. ¿Cuál distribución parece estar sesgada a la izquierda? D
#c. ¿Cuál distribución parece ser simétrica o en forma de "campana"?C
#d. ¿Cuál distribución parece ser bimodal?B
#e. ¿Cuál distribución parece mostrar una falta de intervalos? A,B,D


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

#a. ¿Cómo describiría la forma de esta distribución de las magnitudes de los terremotos?
#Respuesta= sesgada a la derecha
#b. Mencione un intervalo donde ocurren tipicamente las magnitudes.
#Respuesta =4.2-4.4
#c. Determine el rango de las magnitudes (Range = Max - Min).
Ran = max(mags$counts)-min(mags$counts)
Ran2 = max(mags$breaks) - min(mags$breaks)
#d. ¿Qué porcentaje de los terremotos ocurren con magnitud en la clase 5.3 (5.1 : 5.4)?
subset(mags$breaks,mags$breaks == 5.3)
#e. ¿Qué porcentaje de los terremotos tiene una magnitud igual o mayor a 5.0?

#f. ¿Qué porcentaje de los terremotos tienen una magnitud menor o igual a 4.6?



# Problema 4 --------------------------------------------------------------

#B


# Problema 5 --------------------------------------------------------------

# a ) F
#b) H


# Problema 6 --------------------------------------------------------------

fires <- c(78, 44, 47, 105, 126, 181, 277, 210, 155)
fires
#Valor minimo
minimo <- min(fires)
#Valor máximo
maximo <- max(fires)
#Rango
rango <- maximo - minimo
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
#Desviación estándar
sd(fires)
#Realice un boxplot 
boxplot(fires, main ="Incendios Forestales")
