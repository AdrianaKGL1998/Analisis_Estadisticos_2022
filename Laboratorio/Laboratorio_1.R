#Adriana Kareny García Ledezma
# 28/08/2022
# Laboratorio 1


# Parte 1 R y Rstudio -----------------------------------------------------
celular <- 300;transporte <-240; comestibles <- 1527;
gimnasio <- 400;alquiler <- 1500; otros <- 183;


#Ahora que tiene todas las variables, cree un objeto total con la suma de los gastos:
gastos <- c(celular , transporte, comestibles , gimnasio , alquiler ,otros)
total <- sum(gastos)
total

#Suponiendo que la estudiante tiene los mismos gastos todos los meses, ¿cuánto gastaría durante
#un semestre escolar? (suponga que el semestre implica cinco meses).
cinco.meses <- 5*total
cinco.meses
#Manteniendo la misma suposición sobre los gastos mensuales, ¿cuánto gastaría la estudiante
#durante un año escolar? (suponga que el año académico es de 10 meses).

diez.meses <- 10*total
diez.meses

# Autoevaluación ----------------------------------------------------------
gastos <- c(celular , transporte, comestibles , gimnasio , alquiler ,otros)

barplot(gastos,main=" Gastos", names.arg = c("celular" , "transporte", "comestibles" , "gimnasio" , "alquiler" ,"otros"),col = rainbow(6))

G_decreciente <- sort(gastos,decreasing = T)
barplot(G_decreciente,main=" Gastos Decreciente", names.arg = c("comestibles" , "alquiler", "gimnasio" , "celular" , "transporte" ,"otros"),col = 'cyan')


# Parte II Variables ------------------------------------------------------

##Problema 1:

Variable <- c("Nombre de estudiante"
              ,"Fecha de nacimiento (p. Ej., 21/10/1995)"
              ,"Edad (en años)"
              ,"Dirección de casa (por ejemplo, 1234 Ave. Alamo)"
              ,"Número de teléfono (por ejemplo, 510-123-4567)"
              ,"Área principal de estudio"
              ,"Grado de año universitario: primer año, segundo año, tercer año, último año"
              ,"Calificación general: A, B, C, D, F"
              ,"Tiempo (en minutos) para completar la prueba final de MCF 202"
              ,"Numero de hermanos")

tipo <- c("V.Cualitativa",
          "V,Cuantitativa",
          "V,Cuantitativa",
          "V.Cualitativa",
          "V.Cualitativa",
          "V.Cualitativa",
          "V,Cuantitativa",
          "V,Cuantitativa",
          "V,Cuantitativa",
          "V,Cuantitativa")

Mp1 <- matrix(NaN,10,2)
Mp1[,1] <- Variable[1:10]
Mp1[,2] <- tipo[1:10]
colnames(Mp1) <- c("Variable","Tipo")
Mp1

##Problema 2:
v_cualitativa <- c("Edad","Escritorio por oficina","Cantidad de trabajadores","Altura de los trabajadores",
                   "peso de los trabajadores","Salario Base","Trabajador por oficina")
v_cualitativa

v_cuantitativa <- c("Nombre","Color de oficina","Sexo del Trabajador","Estado Civil","Puesto de Trabajo","Grupo Sanguineo","Raza")
v_cuantitativa

Mp2 <- matrix(NaN,7,2)
Mp2[,1] <- v_cualitativa[1:7]
Mp2[,2] <- v_cuantitativa [1:7]
colnames(Mp2) <- c("V_Cualitativa","V_Cuantitativa")
Mp2
##Problema 3:

#RESPUESTA= Es "Culitativa" dado a que estan solamente nombrando el tipo de formato electronico
#utilizando un numero en lugar del nombre del formato.

##Problema 4:

#Cual es la cantidad promedio de horas que los estudiantes de universidades publicas
#trabajan cada semana?

pregunta <- c("Individuo de interes",
              "Variable",
              "Tipo de Variable")

R1 <- c("Estudiante",
        "Horas de Trabajo",
        "V.Cuantitativa")

Mp41 <- matrix(NaN,3,2)
Mp41[,1] <- pregunta[1:3]
Mp41[,2] <- R1[1:3]
colnames(Mp41) <- c("Pregunta","Respuesta")
Mp41

#Que proporcion de todos los estudiantes universitarios de Mexico estan inscritos en una
#universidad publica?

R2 <- c("Estudiante Universitario",
        "Inscripciones en universidad publica",
        "V.Cualitativa")

Mp42 <- matrix(NaN,3,2)
Mp42[,1] <- pregunta[1:3]
Mp42[,2] <- R2[1:3]
colnames(Mp42) <- c("Pregunta","Respuesta")
Mp42

#En los universidades publicas, ¿las estudiantes femeninas tienen un promedio de CENEVAL
#mas alto que los estudiantes varones?

R3 <- c("Estudiantes",
        "Promedio de CENEVAL segun sexo",
        "V. Cuantitativa")

Mp43 <- matrix(NaN,3,2)
Mp43[,1] <- pregunta[1:3]
Mp43[,2] <- R3[1:3]
colnames(Mp43) <- c("Pregunta","Respuesta")
Mp43

#¿Es mas probable que los atletas universitarios reciban asesoramiento academico que los
#atletas no universitarios?

R4 <- c("Atletas",
        "asesoramiento academico",
        "V. Cualitativo")

Mp44 <- matrix(NaN,3,2)
Mp44[,1] <- pregunta[1:3]
Mp44[,2] <- R4[1:3]
colnames(Mp44) <- c("Pregunta","Respuesta")
Mp44

#Si reunieramos datos para responder a las preguntas de la investigacion anterior, ¿que
#datos podrian analizarse mediante un histograma? ¿Como lo sabes?

#RESPUESTA = Todos los datos pueden ser medidos por un histograma ya que en los casos anteriormente
#mostrados solo se esta trabajando con una variable a la vez.
