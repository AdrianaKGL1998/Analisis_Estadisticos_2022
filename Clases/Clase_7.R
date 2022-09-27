#Adriana Kareny Garcia Ledezma
# 27/09/2022
# Clase 7
#Analisis de Varianza (ANOVA):
#SE OCUPA: 3 tratamiento o variables, homogenidad, normalidad


# Datos -------------------------------------------------------------------

#Hnula: No exite diferencia significativa en la avena en los 3 distintos usos de suelos
#Halternativa:exite diferencia al menos en una de las medias de usos de suelos

arena <- c(6,10,8,6,14,17,9,11,7,11)
arcilla <- c(17,15,3,11,14,12,12,8,10,13)
limo <- c(13,16,9,12,15,16,17,13,18,14)

prod<- c(arena,arcilla,limo)

suelo <- gl(3,10,30,labels = c("arena","arcilla","limo"))#gl: generar niveles (cuantas etiquetas,cuantos son cada etiqueta, total de todas las etiquetas)

avena <- data.frame(suelo,prod)#data.frama: junta dos variables independientes 


# Funcion tapply ----------------------------------------------------------

#que ha que calularle a cDA CURBA = LA MEDIA

tapply(avena$prod, avena$suelo, length)
tapply(avena$prod, avena$suelo, mean)
tapply(avena$prod, avena$suelo, sd)
tapply(avena$prod, avena$suelo, var)

#comprobar homogenidad de varianza 0 Revisar la homogenidad de la varianza
#hay dos pruebas que s epueden realizar:
#primera prueba
bartlett.test(avena$prod,avena$suelo)#p=0.5283, NO hay diferencia significativa
#segunda prueba
fligner.test(avena$prod,avena$suelo)#p = 0.8332 No hay diferencia significativa

#Revisar datos de forma grafica
boxplot(avena$prod ~ avena$suelo)
#on layer es el punto que sale fuera de las graficas, el dato fue mal tomado o es una variacion
#hay diferencia en un boxplot si las cajas no se empalman entre ellas es decir en el eje de las y no en el de las x

#si la varianza del error es mas grande que la del tratamiento NO habra diferencias significativas 
#si la varianza del error es maspequeña que la del tratamiento si habra diferencias significativas 

#Suma de cuadrados
SST <- sum((avena$prod - mean(avena$prod))^2)

arena - mean(arena)
arcilla - mean(arcilla)
limo - mean(limo)

arena.sum <- sum((arena - mean(arena))^2)
arcilla.sum <- sum((arcilla - mean(arcilla))^2)
limo.sum <- sum((limo - mean(limo))^2) 
  
#suma de cuandrados del error
SSe <- arena.sum +arcilla.sum + limo.sum
SSe

#suma toral del tratamiento
SStr <- SST -SSe
SStr

#Cuadrado Medio= me representa, es la varianza promedio que tiene el experiment
tapply(avena$prod,avena$suelo,var)
CME <- mean(tapply(avena$prod,avena$suelo,var))
#cuadrado medio del tratamiento
CMtr <- SStr/2

# Fcalulada
Fcal <- CMtr/CME

# Ftabulada
Ftab <- qf(0.95,2,27)#2 grados de libertat del tratamiento,  el 27 grados de libertat del error 
Ftab

#Fcalulado mayor que el tabulado Hay diferencias significativas
#Fcalulado pequeño que el tabulado No hay diferencias significativas

#probabilidad de F 
probF <- 1- pf(4.24,2,27)#pf probabilidad de f es una funcion 
#otra forma de escribir probF
probF <- 1- pf(Fcal,2,27)
probF
# si hay diferenciassignificativas en este probelma

#que se debe de hacer ahora que se sabe que hay diferencias 
#Prueba de Tukey

# ANOVA procedimiento aimplificado 

anova.aov <- aov(avena$prod ~ avena$suelo )
summary(anova.aov)

par(mfrow=c(2,2))
plot(aov(avena$prod ~ avena$suelo ))
par(mfrow=c(1,1))


## Prueba de Tukey
TukeyHSD(anova.aov,conf.level=0.95)
#determinamos con padj es con la que determianmos si hay diferencias 

plot(TukeyHSD(anova.aov))
#mientras que en la grafica toca el 0 no hay diferencia si no toca la linea de cero hay diferencia dependiendo dle lado es positiva o negativa



#Homogenidad y Normailidad
#Varianza
#si hay varianza parametrica hacemos tukey 