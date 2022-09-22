#Adriana Kareny Garcia Ledezma
# 21/09/2022
# Tarea 6

eru <- read.csv("Tareas/erupciones.csv",header = T)
head(eru)

plot(eru$eruptions,eru$waiting,pch=19,
     xlab = "Tiempo de espera entre erupciones (min)",
     ylab = "Duración de las erupciones (min)")
abline(lm(eru$waiting~eru$eruptions),col="red")

# Correlacion: ------------------------------------------------------------

##Realizar las estadísticas descriptivas de ambas variables (media, desviación estándar y varianza)
mean(eru$eruptions)
sd(eru$eruptions)
var(eru$eruptions)

mean(eru$waiting)
sd(eru$waiting)
var(eru$waiting)

##¿Cuál es el coeficiente de correlación (r)?
c.eru <- cor.test(eru$eruptions,eru$waiting)
c.eru

##¿Es significativa la correlación?
### Respuesta: si es significativo la correlacion

# Regresion lineal: -------------------------------------------------------

##Establecer la Hipotesis nula
### H0: No hay diferencia significativa entre las variables de Tiempo de espera entre erupciones y Duración de las erupciones

##Establecer la Hipotesis alternativa
### H1: Hay diferencia significativa entre las variables de Tiempo de espera entre erupciones y Duración de las erupciones

lm(eru$eruptions~eru$waiting)
##¿Cual es el valor del intercepto Alfa?
### Respuesta:-1.874016

##¿Cuál es el valor de la pendiente Beta?
### Respuesta: 0.075628

##Realizar un analisis de regresion (Mencione siempre el valor P-value para determinar si es significativa o no)
summary(lm(eru$eruptions~eru$waiting))
###Respuesta p_value= 2.2e-16

##Son significativas las regresoras: intercepto Alfa y la pendiente Beta
### Respuesta: Dado que Alfa y Beta tienen un valor P-value menor a 0.05, ambas variables son significativas

##Es significativa la regresion
### Respuesta: Si, es significativo, dado que es menor a 0.05

##Finalmente conteste: ¿Cual sera la duracion en minutos de la proxima erupcion, si los tiempo de espera son los dados en el siguiente cuadro?
#Alfa+Beta*(x)
-1.874016 + (0.075628*80)
-1.874016 + (0.075628*40)
-1.874016 + (0.075628*45)
-1.874016 + (0.075628*53)
-1.874016 + (0.075628*61)