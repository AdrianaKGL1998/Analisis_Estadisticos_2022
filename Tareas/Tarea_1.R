#Adriana Kareny García Ledezma
# 28/08/2022
# Tarea 1


# Problema 1 --------------------------------------------------------------


rm(list = ls())

Pinus <- 3140;Mezquite <- 1453;Encinos <- 450;
Teka <- 1200;Juiperos <- 720;

superficie <- c(Pinus,Mezquite,Encinos,Teka,Juiperos)

#x11()
barplot(superficie,main=" Superficies Reforestada"
        ,ylab="Frecuencia", names.arg = c("Pinus","Mezquite","Encinos","Teka","Juiperos"),col = rainbow(5))

s_creciente <- sort(superficie)

#x11()
barplot(s_creciente,main=" sort_Creciente"
        ,ylab="Frecuencia",names.arg = c("Encinos","Juiperos","Teka","Mezquite","Pinus"),col = 'pink' )


s_decreciente <- sort(superficie,decreasing = T)

#x11()
barplot(s_decreciente,main=" sort_Decreciente"
        ,ylab="Frecuencia",names.arg = c("Pinus","Mezquite","Teka","Juiperos","Encinos"),col = 'blue' )

mean(superficie)


# Problema 2 --------------------------------------------------------------
rm(list = ls())
germinacion <- c(4, 1, 6, 2, 4, 2, 4, 2, 4, 6, 3, 5, 3, 2, 5, 4, 0, 5, 4,
                 2, 4, 5, 3, 5, 3, 5, 4, 3, 6, 2)
mean(germinacion)
sd(germinacion)



# Problema 3 --------------------------------------------------------------

altura <- c(38, 14, 44, 11, 9, 21, 39, 28, 41, 4, 35, 24, 36, 12,
            20, 31, 24, 25, 10, 21, 11, 36, 37, 20, 26)
mean(altura)
sd(altura)
