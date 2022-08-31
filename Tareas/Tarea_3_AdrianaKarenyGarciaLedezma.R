#Adriana Kareny Garcia Ledezma
# 29/08/2022
# Tarea 3


# Problema 1 --------------------------------------------------------------

M1 <- matrix(c(1,6,1,2,4,3,3,1,4,4,3,2),3,4)
rownames(M1) <- c("i","xi","yi")
M1

i <- seq(1,4,1)
x <- c(6,4,1,3)
y <- c(1,3,4,2)

xi <- sum(x)
xiyi <- sum(x)*sum(y)


Pxi <- prod(x)
Pxiyi <- prod(x)*prod(y)
Pxiyi2 <- (prod(x)^2)*(prod(y)^0.5)


# Problema 2 --------------------------------------------------------------

Grupo_A <- c(80,90,90,100)
Grupo_A
Grupo_B <- c(60, 65, 65, 70, 70, 70, 75, 75, 80, 80, 80, 80, 80, 85, 100)
A_M <- mean(Grupo_A)
A_M
B_M <- mean(Grupo_B)
B_M

# a) Grupo A,debido a que es un grupo pequeÃ±o con altas alturas
# b) Grupo A, si coincide con mi primera impresiÃ³n 


# Problema 3 --------------------------------------------------------------

examenes_presentados <- c(87,72,85)
S_Ex <- sum(examenes_presentados)# suma de los examenes presentados
puntos_80 <- 4*80#puntos necesarios para tener una media 80
Calificacion_4 <- puntos_80 - S_Ex#calificacion que debe de obtener para el 4 ex.
Calificacion_4

#Jose debera obtener un de 76 calificacion parea tener una media de 80

Ex4 <- c(87,72,85,76)
mean(Ex4)


# Problema 4 --------------------------------------------------------------
#Respuesta correcta b)Hay un total de 110 niños en la ciudad.

# Problema 5 --------------------------------------------------------------

M2 <- matrix(c(5,6,7,8,9,1,3,5,3,1),5,2)
colnames(M2) <- c("Germinaciones","Cajas petri")
M2

#a) ¿Que tipo de gráfico podrías usar para visualizar estos datos?

boxplot(M2)# grafica de Caja y bigotes

hist(M2)#histograma

stem(M2)#grafico de tallo y hoja

#b) ¿Cual es la media? Muestre su trabajo o código, o explique cómo obtuvo su respuesta.
mean(M2)
#c) ¿Cual es la mediana? Muestre su trabajo o código, o explique cómo obtuvo su respuesta
median(M2)

# Problema 6 --------------------------------------------------------------

set <- c(2, 2, 3, 6, 10)

#a) Calcule la moda, la mediana y la media.
mean(set) #media
median(set)#mediana
#moda
  uniqv1 <- unique(set)
  uniqv1[which.max(tabulate(match(set, uniqv1)))]
  
#b) Suma 5 a cada uno de los valores de los datos. Calcule la moda, la mediana y la media.
set5 <- set+5
mean(set5) #media
median(set5)#mediana
#moda
uniqv2 <- unique(set5)
uniqv2[which.max(tabulate(match(set5, uniqv2)))]

#c) Compare los resultados de las partes (a) y (b). En general, ¿cómo crees quela moda, la
#mediana y la media se ven afectadas cuando se agrega la misma constante a cada valor
#de datos en un conjunto?
M3 <- matrix(NaN,3,2)
M3[,1] <- c(mean(set),median(set),uniqv1[which.max(tabulate(match(set, uniqv1)))])
M3[,2] <- c(mean(set5),median(set5),uniqv2[which.max(tabulate(match(set5, uniqv2)))])
M3
#RESPUESTA =Al hagregar una constante la cual interactua en todas, muestra un cambio equivalente en la media,mediana y moda.

#d) Multiplique cada valor de los datos por 5. Calcule la moda, la mediana y la media.
set_M5 <- set*5
uniqv3 <- unique(set_M5)
#moda
uniqv3[which.max(tabulate(match(set_M5, uniqv3)))]
median(set_M5)#mediana
mean(set_M5) #media

#e) Compare los resultados de las partes (a) y (d). En general, ¿cómo crees que la moda,
#La mediana y la media se ven afectadas cuando cada valor de datos en un conjunto se
#multiplica por la misma constante?

Me <- matrix(NaN,3,2)
Me[,1] <- c(mean(set),median(set),uniqv1[which.max(tabulate(match(set, uniqv1)))])
Me[,2] <- c(mean(set_M5 ),median(set_M5 ),uniqv3[which.max(tabulate(match(set_M5, uniqv3)))])
Me
#RESPUESTA= Las variables muestran un comportamiento igualitario a la modificacion que se le aplico.

# problema 7 --------------------------------------------------------------

digitos <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

#a) Enumere cinco dígitos que tengan una mediana de 7 y una media de 7 (se permiten repeticiones).
#Encontrar un conjunto diferente de 5 dígitos que tambien funcionen.

D1 <-c(9,4,7,8,7) #Conjunto de digitos 1
median(D1)#mediana
mean(D1) #media

D2 <-c(4,9,9,6,7) #Conjunto de digitos 2
median(D2)#mediana
mean(D2) #media

#b) Enumere cinco dígitos que tengan una mediana de 7 y una media inferior a 7 (se
#permiten repeticiones). Da la media de tus 5 dígitos. Encuentra un conjunto diferentede 5 dígitos que funcione.

d1 <-c(1,1,7,7,9) #Conjunto de digito 1
median(d1)#mediana
mean(d1) #media


d2 <-c(1,8,9,5,7) #Conjunto de digito 2
median(d2)#mediana
mean(d2) #media

