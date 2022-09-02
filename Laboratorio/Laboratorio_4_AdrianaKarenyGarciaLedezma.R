#Adriana Kareny Garcia Ledezma
# 31/08/2022
# Laboratorio 4



# Importar datos csv ------------------------------------------------------


esp.url <- paste0("https://raw.githubusercontent.com/mgtagle/",
                  "PrincipiosEstadistica2021/main/cuadro1.csv")
inventario <- read.csv(esp.url)
head(inventario)

# dimensiones (num filas y columnas)
dim(inventario)

# nombre de las primeras cinco columnas
names(inventario[ ,1:5])

# Resumen estad?stico b?sico de las columnas 3 a 5 columnas
summary(inventario[ ,3:5])

is.factor(inventario$Posicion)

inventario$Posicion <- factor(inventario$Posicion)
is.factor(inventario$Posicion)

summary(inventario[ ,3:5])



# Tablas de frecuencia ----------------------------------------------------

freq_position <- table(inventario$Posicion)
freq_position

prop_position <- freq_position / sum(freq_position)
prop_position

perc_position = 100 * prop_position
perc_position


# Gr?ficas barplot y pie --------------------------------------------------

barplot(freq_position, las = 1, border = NA, cex.names = 0.7)

#Gr?fico circular o pie
pie(freq_position, col=topo.colors(4))
# topo.colors es una paleta de colores pre establecidas en R y
# el par?ntesis indica el # de colores a usar

pie(freq_position, col = topo.colors(4),
    labels = paste(levels(inventario$Posicion), round(perc_position, 2), " %"))


# Autoestudio -------------------------------------------------------------

freq_Especie <- table(inventario$Especie)
freq_Especie

prop_Especie <- freq_Especie / sum(freq_Especie)
prop_Especie

perc_Especie = 100 * prop_Especie
perc_Especie

barplot(freq_Especie, las = 1, border = NA, cex.names = 0.7,main = "Especie")

pie(freq_Especie, col = topo.colors(4),
    labels = paste(levels(inventario$Especie), round(perc_Especie, 2), " %"))

# Histogramas -------------------------------------------------------------

diam_hist <- hist(inventario$Diametros, las = 1, col = '#ffe0b3')
diam_hist
diam_hist$breaks

h1 <- hist(inventario$Diametros, xaxt = "n",
           breaks = c(6, 8, 10, 12, 14, 16, 18, 20, 22,24),
           col = "#00cc99", xlab="Di?metros (cm)",
           ylab= "Frecuencias",
           main = "",
           las = 1,
           ylim = c(0,14))
axis(1, h1$mids)



# Autoestudio -------------------------------------------------------------

alt_hist <- hist(inventario$Altura, las = 1, col = 'orange')
alt_hist
alt_hist$breaks

h <- hist(inventario$Altura, xaxt = "n",
           breaks = c(8,10,12,14,16,18,20,22),
           col = "cyan", xlab="Altura (cm)",
           ylab= "Frecuencias",
           main = "",
           las = 1,
           ylim = c(0,14))
axis(1, h$mids)

