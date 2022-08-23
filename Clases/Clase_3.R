#Clase 3
#Adriana Kareny Garcia Ledezma
#23/08/2022

# Importar un csv ---------------------------------------------------------

est <- read.csv("Clases/cumbres.csv",header=T)#header primera fila es texto
head(est)#revisar los primeros seis datos
tail(est)#revisar los ultimos seis datos

viv <- read.csv("Clases/vivero.csv",header=T)

boxplot(viv$IE ~ viv$Tratamiento )
viv$Tratamiento <- as.factor(viv$Tratamiento)
summary(viv)
