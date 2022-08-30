#Adriana Kareny García Ledezma
# 29/08/2022
# Tarea 3


# Problema 1 --------------------------------------------------------------

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

# a) Grupo A,debido a que es un grupo pequeño con altas alturas
# b) Grupo A, si coincide con mi primera impresión 


# Problema 3 --------------------------------------------------------------

examenes_presentados <- c(87,72,85)
S_Ex <- sum(examenes_presentados)# suma de los examenes presentados
puntos_80 <- 4*80#puntos necesarios para tener una media 80
Calificacion_4 <- puntos_80 - S_Ex#calificacion que debe de obtener para el 4 ex.
Calificacion_4

#Jose debera obtener un de 76 calificacion parea tener una media de 80

Ex4 <- c(87,72,85,76)
mean(Ex4)
