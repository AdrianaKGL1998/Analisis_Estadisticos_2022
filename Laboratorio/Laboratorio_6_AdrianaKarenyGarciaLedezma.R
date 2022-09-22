#Adriana Kareny Garcia Ledezma
# 21/09/2022
# Laboratorio 6


# Base de los vectores ----------------------------------------------------

#la forma mas comun de crear vectores es con la funcion c()
wins = c(52, 51, 47, 47, 42)

#vector de losses
losses = c(20, 21, 25, 25, 30)

#se pueden usar los anteriores vectores para crear otro vector
win_loss_perc = wins / (wins + losses)
win_loss_perc

teams = c('UtJ', 'PhS', 'DnN', 'LAC', 'DlM')


# Manipulación de vectores: subconjuntos ----------------------------------

# primer elemento de 'wins'
wins[1]
# tercer elemento de 'losses'
losses[3]
# último nombre en 'teams'
teams[5]

#Algunas funciones comunes que puede usar en vectores son:
length(teams)
teams[length(teams)]
sort(wins, decreasing = TRUE)
rev(wins)

# Subconjuntos con indices logicos ----------------------------------------

# victorias de Utah Jazz
wins[teams == 'UtJ']
# equipos con victorias > 40
teams[wins > 40]
# nombre de los equipos con derrotas entre 10 and 29
teams[losses >= 10 & losses <= 29]


# Factores y variables cualitativas ---------------------------------------

num_vector <- c(1, 2, 3, 1, 2, 3, 2)
# crear un factor apartir de num_vector
first_factor <- factor(num_vector)
first_factor

teams = factor(teams)
teams

# Secuencias --------------------------------------------------------------

# operador dos puntos :
1:5
1:10
-3:7
10:1
# función sequencia
seq(from = 1, to = 10)
seq(from = 1, to = 10, by = 1)
seq(from = 1, to = 10, by = 2)
seq(from = -5, to = 5, by = 1)

# Vectores repetidos ------------------------------------------------------
rep(1, times = 5) # repetir 1 cinco veces

rep(c(1, 2), times = 3) # repetir 1 y 2 tres veces

rep(c(1, 2), each = 2)

rep(c(1, 2), length.out = 5)

rep(c(3, 2, 1), times = 3, each = 2)


# De vectores a estructura tabular (data frame) ---------------------------

dat = data.frame(
  Teams = teams,
  Wins = wins,
  Losses = losses,
  WLperc = win_loss_perc)
dat

dat$Teams

dat$Wins[1]
dat$Wins[5]

# Victorias del equipo Utah
dat$Wins[dat$Teams == 'UtJ']
# equipos con victorias > 40
dat$Teams[dat$Wins > 40]
# nombre de los equipos con derrotas entre 10 y 29
dat$Teams[dat$Losses >= 10 & dat$Losses <= 29]


# Tu Turno ----------------------------------------------------------------

Teams = c("UJ","PS","DN","LAC","DM","PTB","LAL","MG","GSW","SAS","NOP","SK","MT","OCT","HR")
Wins =  c(52,51,47,47,42,42,42,38,39,33,31,31,23,22,17)
Losses = c(20,21,25,25,30,30,30,34,33,39,41,41,49,50,55)
Win_loss_perc = c(.722,.708,653,.653,.583,.583,.583,.528,.542,.458,.431,.431,.319,.306,.236)
Games_behind =  c("NaN",1,5,5,10,10,10,14,13,19,21,21,29,30,35)
points_scored =  c(116.4,115.3,115.1,114,112.4,116.1,109.5,113.3,113.7,111.1,114.6,113.7,112.1,105,108.7)
points_against =  c(107.2,109.5,110.1,107.8,110.2,114.3,106.8,112.3,112.7,112.8,114.9,117.4,117.7,115.6,116.7)
rating = c(8.97,5.67,4.82,6.02,2.26,1.81,2.77,1.07,1.10,-1.58,-0.20,-3.45,-5.25,-10.13,-7.50)

games.behind = Wins[1]-Wins

bal.tem = data.frame(TeamsWC = Teams,W = Wins,L = Losses,W_L = Win_loss_perc,
                     GB = Games_behind, PS_G = points_scored,
                     PA_G = points_against,SRS = rating )

sort(points_scored, decreasing = TRUE)
sort(points_scored, decreasing = FALSE)