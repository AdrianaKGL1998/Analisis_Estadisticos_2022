#Adriana Kareny García Ledezma
# 06/09/2022
# Tarea Clases 5

#Pruebas de T sirven para hacer comparaciones de medias 
#existen 3 tipos:(una muestra, dependiente, independiente)

viv <- read.csv("Clases/vivero.csv",header=T)
summary(viv)#resumen de la base exportada

#indice de esbeltes mas alto mayor exito tiene

#valor de referencia a tomar Media_t =0.9375

#hipotrsis nula: No exite diferencia entre nuyestro IE(indice de esbeltes)(0.8371) con el del conafort(0.9375)

boxplot(viv$IE)
hist(viv$IE)
#los datos son de distribucion normla= prueba de safiro y conboro


# Normalidad de datos -----------------------------------------------------

#prueba de safiro= nos dira si son normales o no los son
#para que sean normales hay que revisar el valor de P-value
shapiro.test(viv$IE)
#rechazo la hipotesis alternativa donde dice que no hay distribucion normal 
# entponces nosotros decimos que si hay una distribcion normal

ks.test(viv$IE,"pnorm",mean=mean(viv$IE),sd=sd(viv$IE))
#hacepto la nula 

t.test(viv$IE,mu=0.9375)
#mu=media teuretica
#df = grados de libertad n-1

#no sirven las pltantas para conafort

t.test(viv$IE,mu=0.77)
#hipotesis alternativa




data("chickwts")
summary(chickwts)
hist(chickwts$weight)
length(chickwts$weight)
shapiro.test(chickwts$weight)
t.test(chickwts$weight,mu=250)



# Dos muestras Independiente ----------------------------------------------

boxplot(viv$IE ~viv$Tratamiento)

shapiro.test(viv$IE)

var.test(viv$IE~ viv$Tratamiento)

t.test(viv$IE ~viv$Tratamiento,var.equal=T)
t.test(viv$IE~ viv$Tratamiento)


invent <- read.csv("Clases/inventario.csv",header=T)
invent$Tratamiento <- as.factor(invent$Tratamiento)
invent$Fecha <- as.factor(invent$Fecha)
#hipoteses no exite diferencias significativa entre la varaible diametro entre los dos rodales

boxplot(invent$Diametro ~ invent$Tratamiento)

shapiro.test(invent$Diametro)
var.test(invent$Diametro~invent$Tratamiento)

t.test(invent$Diametro ~ invent$Tratamiento,var.equal=T)


#diametro de copa
boxplot(invent$Dcopa ~ invent$Tratamiento)

shapiro.test(invent$Dcopa)
var.test(invent$Dcopa~invent$Tratamiento)

t.test(invent$Dcopa ~ invent$Tratamiento,var.equal=T)




boxplot(invent$Kilogramo ~ invent$Fecha)
shapiro.test(invent$Kilogramo)
var.test(invent$Kilogramo~invent$Fecha)

t.test(invent$Kilogramo ~ invent$Fecha,paired=T)#paried variables dependientes
#son similares la producion de semillas 
