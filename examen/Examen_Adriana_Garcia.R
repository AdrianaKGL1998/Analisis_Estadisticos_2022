
suelo<- read.csv("examen/obs.csv",header=T)
suelo$zone <- as.factor(suelo$zone)
suelo$wrb1 <- as.factor(suelo$wrb1)


# Actividad 1: ------------------------------------------------------------

summary(suelo$Clay1)
summary(suelo$Clay2)
summary(suelo$Clay5)

#P1 ¿Cuál es la tendencia del contenido de Arcilla (Clay) con respecto a la profundidad?
#Respuesta: Los datos tienden a ir de forma creciente conforme aumenta la profundidad

# Actividad 2 -------------------------------------------------------------

stem(suelo$Clay1,scale = 1)

#P2 ¿Los datos de contenido de arcilla siguen una distribución simétrica o con sesgo?
#Respuesta:Sesgo hacia la izquierda 

# Actividad 3: ------------------------------------------------------------

boxplot(suelo$Clay1)

#P3 ¿Existe evidencia de outliers? (2 Puntos) En caso de existir
#Respuesta: Si 
#P4 ¿Puede identificar cuáles observaciones son mediante una simple restricción de datos? 
#Pista: observe los valores mediante el boxplot y haga la restricción (2 puntos)
#Respuesta:Si se pueden indentificar mediante la funcion subset, dando como resultados que los outliers son 72 71 67

out <- subset(suelo$Clay1,suelo$Clay1>65)
out
restringido <- subset(suelo$Clay1,suelo$Clay1<65)
restringido
boxplot(restringido )
# Actividad 4 -------------------------------------------------------------
mean(suelo$Clay1)

#P5 ¿Estime si el contenido de Arcilla promedio en los suelos tropicales de 30 % es significativamente diferente
#que la media observada en el campo experimental Tropenbos Cameroon Programme (TCP)? (5 puntos)
#Respuesta:Si, es significativamente diferente

# Actividad 5: ------------------------------------------------------------
cor.test(suelo$Clay1,suelo$Clay5)
#P6 ¿Existe una relación positiva, negativa o para nada relacionados, entre los perfiles superior (Clay1 ) e
#inferior (Clay5 ) con el contenido de Arcilla? (2 puntos)

#Respuesta: Existe una relacion Positiva

#P7 ¿La correlación es estadísticamente significativa? (3 puntos)
#Respuesta: Sí, hay una diferencia significativa 


# Actividad 6: ------------------------------------------------------------
#P8 ¿Es posible determinar una ecuación significativa para predecir el comportamiento 
#del contenido de arcilla en el perfil inferior Clay5 ? (2 puntos)
#Respuesta:Si

#P9 ¿Cuál es la ecuación final para predecir el comportamiento del contenido de arcilla en el perfil más
#profundo (30-50 cm)? (5 puntos)
#Respuesta: y = Alfa+ Beta(x) o y =18.75856 + 0.82891(x)
suelo.lm <- lm(suelo$Clay5 ~ suelo$Clay1)
suelo.lm
summary(suelo.lm)
#P10 ¿Son ambos parámetros alfa y beta significativos? (4 puntos)
#Respuesta: Alfa y Beta ambos son altamente significativos.

#P11 ¿Cuál es el porcentaje de varianza explicado por el método aplicado? (3 puntos)
sum(suelo.lm$residuals**2)/145
#Respuesta: 32.34%


# Actividad 7: ------------------------------------------------------------

#P12 ¿Existe una forma de identificar la variación entre las cuatro zonas que se encuentran en el estudio? (2
#puntos)
#Si hay una forma de indentificar


#P13 Realice una inspección visual del contenido de arcilla en el perfil 30-50 cm (Clay5 ) y las cuatro zonas
#(zone) presentes en el área de estudio. ¿Existen indicios de que las cuatro zonas son diferentes en cuanto al
#contenido de arcilla en el perfil de 30 a 50 cm.? (2 puntos)
#Respuesta: En las 4 zonas se observa que tiene una tendencia decreciente en arcillas confome se va habanzando en las zonas 

plot(suelo$Clay5 ~ suelo$zone,pch = 19)

#P14 ¿Observa alguna tendencia en los datos en las diferentes zonas?
#Resupuesta: No, se observa ninguna tendencia en los datos de las distintas zonas

by(suelo$Clay5, suelo$zone, summary)


# Actividad 8: ------------------------------------------------------------

#P15 ¿Existen diferencias significativas entre el contenido de arcilla del perfil 30-50 cm y las zonas del estudio?
#(5 puntos)
#Respuesta: Sí, hay diferencia significativa 

suelo.aov <- aov(suelo$Clay5 ~ suelo$zone )
summary(suelo.aov)

par(mfrow=c(2,2))
plot(aov(suelo$Clay5 ~ suelo$zone  ))
par(mfrow=c(1,1))

#P16 En caso de existir diferencias ¿Cuáles zonas son diferentes estadísticamente entre si en el contenido de
#arcilla en el perfil de 30-50 cm? (5 puntos)
#Respuesta: Solamente en la zona 1-2 no hay una diferencia, mientras que las demas zona(1-3, 1-4,3-2,2-4,4-3) si existe una diferencia.

TukeyHSD(suelo.aov,conf.level=0.95)
#determinamos con padj es con la que determianmos si hay diferencias 

plot(TukeyHSD(suelo.aov))
#mientras que en la grafica toca el 0 no hay diferencia si no toca la linea de cero
#hay diferencia dependiendo dle lado es positiva o negativa

