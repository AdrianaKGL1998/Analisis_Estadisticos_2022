#Adriana Kareny Garcia Ledezma
# 20/09/2022
# Clase 7
#correlacion (r): relacionar dos variables con distintas unidades, esta mide haber si hay una asociasion lineal 
#x = independiente (diametro) y= dependiente(altura) 
#las corelaciones, hay de dos tiepos 
#-correlacion positiva: aumenta x  aumenta y
#-correlacion negativa: aumenta x disminuye y (ej: mayor altitud menor diversidad)
#los valores osilan entre -1 a +1
#esto te dice que tan relacionadas estan mis variables, dando pie a otro analisis "regresion"
#regresion: tendencia central formula = y =alfa +beta(x)
#alfa = intersepto beta= pendiente 


# Importar datos ----------------------------------------------------------

ebano <- read.csv("ebanos.csv",header=T)
head(ebano)

shapiro.test(cnpy$Cnpy)
#son normales los datos
shapiro.test(cnpy$LAI4)
# no son normales los datos
shapiro.test(cnpy$GLI)
#son normales

plot(ebano$ï..diametro,ebano$altura,
     pch=18,col="red",
     xlab = "Diametro cm",
     ylab= "Altura cm")#pch cambia simbolos
#valore de p es mayor a 0.05 No hay correlacion h.nula
# valor de p menor a 0.05 Si hay una correlacion positiva h.alterna 

#correlacion de pearson (r)
cor.eb <- cor.test(ebano$ï..diametro,ebano$altura)
cor.eb
#coeficiente de correlacion .82 esta dentro de los intervalos de confianza 
#Si hay una correlacion positiva, diametro me va ayudar a predecir la variable altura 
#coeficiente de determinacion es r^2, 

C.determinacion <- 0.8217^2#% diametro explica a un 67%o 68% la variacion de la altura 
#asociacion lineal positiva


# Canopy ------------------------------------------------------------------


cnpy <- read.csv("canopy.csv",header=T)


cor.test(cnpy$Cnpy,cnpy$LAI4)


# Regresión  --------------------------------------------
#Regresión entre cnpy vs LAI4

#SOCIACION LINEAL (SE CHECA CON LA CORRELACION)
#DATOS DEBEN SER INDEPENDIENTES

# funcion de lm lineal modem
cp.lm <- lm(cnpy$LAI4~cnpy$Cnpy)
cp.lm

plot(cnpy$Cnpy,cnpy$LAI4,
     xlab = "apertura del dosel (%)",
     ylab = "IAF",
     pch = 19,col="blue",)
abline(cp.lm,col="red")
text(37,1.5,"Y=2.67-0.04*x",pos = 2)
#h nula No hay una corelacion significatuva
#h alternativa Si hay una correlacion significativa 
#formula y =2.66+(-0.04)*x

#EXTRAER DATOS DE CANOPY
summary(cp.lm)
#Los residuales diferencia entre cada valor observado y valor predicho 
#la suma de todos los residuales elevados al cuadrado es 0 so no es 0 le falta ajuste 
#r cuadrado =0.69
#r ajustado =0.68 explica un 68% de indice de foliat

#ALFA ES INTERCEPT ES SIGNIFICATIVO SI ES MENOR A 0.05
#BETA ES cnpy$Cnpy ES SIGNIFICATIVO SI ES MENOR A 0.05

## SI ES SIGNIFICATIVA LA REGRESION 
## ALFA Y BETA SON ALTAMENTE SIGNIFICATIVOS

#esto es la varianza 
sum(cnpy$residual**2)/38# 38 son los grados de libertad del experimento
#suma de cuadrado del error 
sum(cnpy$residual**2)


# Suma de Residuales ------------------------------------------------------

sum(cp.lm$residuals)
cnpy$Yprima <- cp.lm$fitted.values
cnpy$dif <- cnpy$LAI4 -cnpy$Yprima
cnpy$residual <- cp.lm$residuals

plot(cnpy$Cnpy,cnpy$GLI,
     xlab = "apertura del dosel (%)",
     ylab= "GLI",
     pch = 19,col="red")

cor.test(cnpy$Cnpy, cnpy$GLI)
# si es significativa 
# NO es una buelna trayectoria dado que solo explica el 30%

plot(cnpy$LAI4,cnpy$GLI,
     xlab = "LAI4",
     ylab= "GLI",
     pch = 19,col="green")
cor.test(cnpy$LAI4,cnpy$GLI)
# si es significativa la correlacion

cor.test(cnpy$Cnpy,cnpy$LAI4)

#si los datos no vienen de una distribucion normal se aplica "KENDALL"
#cor.test(cnpy$Cnpy,cnpy$LAI4,method = "kendall")# solo es ejemplo los datos aqui si son normales 

