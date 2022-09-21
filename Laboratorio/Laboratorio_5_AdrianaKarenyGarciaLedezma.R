#Adriana Kareny Garc√≠a Ledezma
# 21/09/2022
# Laboratorio 5

# Ejercicio 1: El cuarteto de Anscombe ------------------------------------

# Graficar en un cuadro de 2x2
op = par(mfrow = c(2, 2), mar = c(4.5, 4, 1, 1))
plot(anscombe$x1, anscombe$y1, pch = 20)
plot(anscombe$x2, anscombe$y2, pch = 20)
plot(anscombe$x3, anscombe$y3, pch = 20)
plot(anscombe$x4, anscombe$y4, pch = 20)
par(op)


# Coeficiente de correlacion ----------------------------------------------

## Conjunto de Datos 1
Datos1 <- matrix(0,length(anscombe$x1),2)
colnames(Datos1) <- (c("x","y"))
Datos1[,1] <- anscombe$x1
Datos1[,2] <- anscombe$y1
Datos1

## Funcion de lineal modem (lm)
cd1.lm <- lm(anscombe$y1~anscombe$x1)
#Grafica de dispersion del conjunto de datos 1
plot(anscombe$x1, anscombe$y1, pch = 20)
abline(cd1.lm,col="red")


## Examinar la relacion que existe entre dos muestras mediante una correlacion.
### Respuesta: Es una relacion lineal simple.

## Explore los datos graficamente y explique.
### Respuesta: Es una correlacion positiva.

##Establezca la Hipotesis nula y la Hipotesis alternativa,
### Respuesta: Hipotesis nula = No hay una corelacion significatuva; Hipotesis alternativa = hay una corelacion significatuva

## Aplique la prueba correspondiente.

###Prueba de zafiro
shapiro.test(anscombe$x1) #Los datos son normales.

###Coeficientes de correlacion (r).
c.d1 <- cor.test(anscombe$x1, anscombe$y1)
c.d1

summary(cd1.lm)
##Reporte los datos (indicar valor de r, grados de libertad y probabilidad, asi como la significancia
##de la correlacion).
### Respuesta: r=0.8164205; df=9; p-value=0.00217.
### Hipotesis alterna=La correlacion si es significativa 


############################################################################################################
## Conjunto de Datos 2
Datos2 <- matrix(0,length(anscombe$x2),2)
colnames(Datos2) <- (c("x","y"))
Datos2[,1] <- anscombe$x2
Datos2[,2] <- anscombe$y2
Datos2

## Funcion de lineal modem (lm)
cd2.lm <- lm(anscombe$y2~anscombe$x2)
#Grafica de dispersion del conjunto de datos 1
plot(anscombe$x2, anscombe$y2, pch = 20)
abline(cd2.lm,col="red")


## Examinar la relacion que existe entre dos muestras mediante una correlacion.
### Respuesta: Es una relacion no lineal.

## Explore los datos graficamente y explique.
### Respuesta: Es una relacion no lineal.

##Establezca la Hipotesis nula y la Hipotesis alternativa,
### Respuesta: Hipotesis nula = No hay una corelacion significatuva; Hipotesis alternativa = hay una corelacion significatuva

## Aplique la prueba correspondiente.

###Prueba de zafiro
shapiro.test(anscombe$x2) #Los datos son normales.

###Coeficientes de correlacion (r).
c.d2 <- cor.test(anscombe$x2, anscombe$y2)
c.d2

summary(cd2.lm)
##Reporte los datos (indicar valor de r, grados de libertad y probabilidad, asi como la significancia
##de la correlacion).
### Respuesta: r=0.8162365; df=9; p-value=0.002179.
### Hipotesis alterna=La correlacion si es significativa 


#######################################################################################################3
## Conjunto de Datos 3
Datos3 <- matrix(0,length(anscombe$x3),2)
colnames(Datos3) <- (c("x","y"))
Datos3[,1] <- anscombe$x3
Datos3[,2] <- anscombe$y3
Datos3

## Funcion de lineal modem (lm)
cd3.lm <- lm(anscombe$y3~anscombe$x3)
#Grafica de dispersion del conjunto de datos 1
plot(anscombe$x3, anscombe$y3, pch = 20)
abline(cd3.lm,col="red")


## Examinar la relacion que existe entre dos muestras mediante una correlacion.
### Respuesta: Es una relacion lineal simple.

## Explore los datos graficamente y explique.
### Respuesta: Es una correlacion positiva.

##Establezca la Hipotesis nula y la Hipotesis alternativa,
### Respuesta: Hipotesis nula = No hay una corelacion significatuva; Hipotesis alternativa = hay una corelacion significatuva

## Aplique la prueba correspondiente.

###Prueba de zafiro
shapiro.test(anscombe$x3) #Los datos son normales.

###Coeficientes de correlacion (r).
c.d1 <- cor.test(anscombe$x3, anscombe$y3)
c.d1

summary(cd3.lm)
##Reporte los datos (indicar valor de r, grados de libertad y probabilidad, asi como la significancia
##de la correlacion).
### Respuesta: r=0.8164205; df=9; p-value=0.00217.
### Hipotesis alterna=La correlacion si es significativa 


##############################################################################################################3
## Conjunto de Datos 4
Datos4 <- matrix(0,length(anscombe$x4),2)
colnames(Datos4) <- (c("x","y"))
Datos4[,1] <- anscombe$x4
Datos4[,2] <- anscombe$y4
Datos4

## Funcion de lineal modem (lm)
cd4.lm <- lm(anscombe$y4~anscombe$x4)
#Grafica de dispersion del conjunto de datos 1
plot(anscombe$x4, anscombe$y4, pch = 20)
abline(cd4.lm,col="red")


## Examinar la relacion que existe entre dos muestras mediante una correlacion.
### Respuesta: Es una relacion no lineal.

## Explore los datos graficamente y explique.
### Respuesta:Es una relacion no lineal.
##Establezca la Hipotesis nula y la Hipotesis alternativa,
### Respuesta: Hipotesis nula = No hay una corelacion significatuva; Hipotesis alternativa = hay una corelacion significatuva

## Aplique la prueba correspondiente.

###Prueba de zafiro
shapiro.test(anscombe$x4) #Los datos no son normales.

###Coeficientes de correlacion (r).
c.d4 <- cor.test(anscombe$x4, anscombe$y4)
c.d4

summary(cd4.lm)
##Reporte los datos (indicar valor de r, grados de libertad y probabilidad, asi como la significancia
##de la correlacion).
### Respuesta: tau=0.8164205; df=9; p-value=0.00217.
### Hipotesis alterna=La correlacion si es significativa 

#øAlguna sorpresa? Como puedes ver, los cuatro pares de las variables xy tienen basicamente la misma
#correlacion de 0.816. Pero no todos tienen diagramas de dispersion en los que los puntos se agrupan
#alrededor de una linea.
