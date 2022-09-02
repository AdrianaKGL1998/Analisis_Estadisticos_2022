#Adriana Kareny García Ledezma
# 31/08/2022
# Tarea 4



# Importar datos csv ---------------------------------------------- --------


esp.url  <- paste0( " https://raw.githubusercontent.com/mgtagle/ " ,
                    " Principios Estadistica2021/main/cuadro1.csv " )
inventario  <- read.csv( esp.url )
cabeza ( inventario )

# dimensiones (num filas y columnas)
tenue ( inventario )

# nombre de las primeras cinco columnas
nombres( inventario [ , 1 : 5 ])

# Resumen estadístico básico de las columnas 3 a 5 columnas
resumen( inventario [ , 3 : 5 ])

es.factor( inventario $ Posición )

inventario $ Posición  <-  factor ( inventario $ Posición )
es.factor( inventario $ Posición )

resumen( inventario [ , 3 : 5 ])


# Tablas de frecuencia ---------------------------------------------- ------

freq_position  <- table( inventario $ Posicion )
freq_position

prop_position  <-  freq_position  / sum( freq_position )
prop_position

posición_perc  =  100  *  posición_prop
posición_perc


# Gráficas barplot y pie --------------------------------------------- -----

barplot( freq_position , las  =  1 , border  =  NA , cex.names  =  0.7 )

# Gráfico circular o pie
tarta ( freq_position , col = topo.colors ( 4 ))
# topo.colors es una paleta de colores preestablecida en R y
# el paréntesis indica el # de colores a usar

pastel ( freq_position , col  = topo.colors ( 4 ),
         etiquetas  = pegar(niveles( inventario $ Posicion ), ronda( perc_posicion , 2 ), " % " ))


#Autoestudio ------------------------------------------------ -------------

freq_Especie  <- table( inventario $ Especie )
freq_especie

prop_Especie  <-  freq_Especie  / sum( freq_Especie )
prop_Especie

perc_Especie  =  100  *  prop_Especie
perc_Especie

barplot( freq_Especie , las  =  1 , border  =  NA , cex.names  =  0.7 , main  =  " Especie " )

pie( freq_Especie , col  = topo.colors( 4 ),
     etiquetas  = pegar(niveles( inventario $ Especie ), round( perc_Especie , 2 ), " % " ))

# Histogramas ------------------------------------------------ -------------

diam_hist  <- hist( inventario $ Diametros , las  =  1 , col  =  ' #ffe0b3 ' )
diam_hist
diam_hist $ descansos

h1  <- hist( inventario $ Diametros , xaxt  =  " n " ,
             descansos  = c( 6 , 8 , 10 , 12 , 14 , 16 , 18 , 20 , 22 , 24 ),
             col  =  " #00cc99 " , xlab = " Diámetros (cm) " ,
             ylab =  " Frecuencias " ,
             principal  =  " " ,
             las  =  1 ,
             ylim  = c( 0 , 14 ))
eje( 1 , h1 $ medios )



#Autoestudio ------------------------------------------------ -------------

alt_hist  <- hist( inventario $ Altura , las  =  1 , col  =  ' naranja ' )
alt_hist
alt_hist $ descansos

h  <- hist( inventario $ Altura , xaxt  =  " n " ,
            descansos  = c( 8 , 10 , 12 , 14 , 16 , 18 , 20 , 22 ),
            col  =  " cian " , xlab = " Altura (cm) " ,
            ylab =  " Frecuencias " ,
            principal  =  " " ,
            las  =  1 ,
            ylim  = c( 0 , 14 ))
eje( 1 , h $ medios )