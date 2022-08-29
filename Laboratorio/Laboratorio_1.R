#Adriana Kareny García Ledezma
# 28/08/2022
# Laboratorio 1

rm(list = ls())
celular <- 300;transporte <-240; comestibles <- 1527;
gimnasio <- 400;alquiler <- 1500; otros <- 183;

gastos <- c(celular , transporte, comestibles , gimnasio , alquiler ,otros)
total <- sum(gastos)
cinco.meses <- 5*total
diez.meses <- 10*total
x11()
barplot(gastos,main=" Gastos", names.arg = c("celular" , "transporte", "comestibles" , "gimnasio" , "alquiler" ,"otros"),col = rainbow(6))

G_decreciente <- sort(gastos,decreasing = T)
x11()
barplot(G_decreciente,main=" Gastos Decreciente", names.arg = c("comestibles" , "alquiler", "gimnasio" , "celular" , "transporte" ,"otros"),col = 'cyan')