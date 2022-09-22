#Adriana Kareny Garcia Ledezma
# 21/09/2022
# Tarea 5


# Ejercicio 1 -------------------------------------------------------------


Speed <- c(2,3,5,9,14,24,29,34)
Abundance <- c(6,3,5,23,16,12,48,43)

efimeras <- matrix(0,2,8)
efimeras[1,] <- Speed
efimeras[2,] <- Abundance

plot(Speed,Abundance,pch=19,
     xlab = "variable independiente",
     ylab = "variable dependiente")

##¿Es estadisticamente significativa la correlacion?
###Respuesta: si es significativo la correlacion
cor.ef <- cor.test(efimeras[1,],efimeras[2,])
cor.ef

# funcion de lm lineal modem
ef.lm <- lm(efimeras[2,]~efimeras[1,])
ef.lm

#grafica de dispercion
plot(Speed,Abundance,pch=19,
     xlab = "variable independiente",
     ylab = "variable dependiente")
abline(ef.lm,col="red")

## Examinar la relacion que existe entre dos muestras mediante una correlacion.
### Respuesta: es una relacion lineal.

## Explore los datos graficamente y explique.
### Respuesta: es una relacion lineal, con una correlacion positiva

##Establezca la Hipotesis nula y la Hipotesis alternativa,
### Respuesta: 
### hipotesis alternativa:Existe una correlación positiva entre la velocidad de los arroyos y la abundancia de efemeras (Ecdyonurus dispar).
### hipotesis nula:No existe una correlacion entre la velocidad del arroyo y la abundancia de efemeras.

## Aplique la prueba correspondiente.

###Prueba de zafiro
shapiro.test(efimeras[1,]) #Los datos son normales.

###Coeficientes de correlacion (r).
cor.ef <- cor.test(efimeras[1,],efimeras[2,])
cor.ef

summary(ef.lm)
##Reporte los datos (indicar valor de r, grados de libertad y probabilidad, asi como la significancia
##de la correlacion).
### Respuesta: r=0.8441408 ; df=6; p-value=0.008393.
### Hipotesis aceptada es la Hipotesis alterna=Existe una correlación positiva entre la velocidad de los arroyos y la abundancia de efímeras.



# Ejercicio 2 -------------------------------------------------------------

#Conjunto de datos: Composiciones del suelo, caracteristicas fasicas y quimicas.
c.suelos <- read.csv("Tareas/suelo.csv",header=T)

N <- cor.test(c.suelos$pH,c.suelos$N)
plot(c.suelos$pH,c.suelos$N)
abline(lm(c.suelos$N~c.suelos$pH),col="red")
De <- cor.test(c.suelos$pH,c.suelos$Dens)
plot(c.suelos$pH,c.suelos$Dens)
abline(lm(c.suelos$Dens~c.suelos$pH),col="red")
P <- cor.test(c.suelos$pH,c.suelos$P)
plot(c.suelos$pH,c.suelos$P)
abline(lm(c.suelos$P~c.suelos$pH),col="red")
Ca <- cor.test(c.suelos$pH,c.suelos$Ca)
plot(c.suelos$pH,c.suelos$Ca)
abline(lm(c.suelos$Ca~c.suelos$pH),col="red")
Mg <- cor.test(c.suelos$pH,c.suelos$Mg)
plot(c.suelos$pH,c.suelos$Mg)
abline(lm(c.suelos$Mg~c.suelos$pH),col="red")
K <- cor.test(c.suelos$pH,c.suelos$K)
plot(c.suelos$pH,c.suelos$K)
abline(lm(c.suelos$K~c.suelos$pH),col="red")
Na <- cor.test(c.suelos$pH,c.suelos$Na)
plot(c.suelos$pH,c.suelos$Na)
abline(lm(c.suelos$Na~c.suelos$pH),col="red")
Con <- cor.test(c.suelos$pH, c.suelos$Conduc)
plot(c.suelos$pH, c.suelos$Conduc)
abline(lm(c.suelos$Conduc~c.suelos$pH),col="red")

r <- c(N$estimate,De$estimate,P$estimate,Ca$estimate,
       Mg$estimate,K$estimate,Na$estimate,Con$estimate)

v_p <- c(N$p.value,De$p.value,P$p.value,Ca$p.value,
         Mg$p.value,K$p.value,Na$p.value,Con$p.value)

conjuntos <- c("pH - N","pH - Dens","pH - P","pH - Ca","pH -Mg","pH -K","pH -Na","pH - Conductividad")

##Cuadro de datos con los estadisticos de interes.
M <- matrix(0,8,3)
M[,1] <- conjuntos
M[,2] <- r
M[,3] <- v_p
colnames(M) <- c("Conjunto", "r","valor de P")
M
