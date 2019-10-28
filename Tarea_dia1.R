#######################################
##### TAREA DE ENTORNOS Y REPASO    ###
#######################################

# REPASO -------------------------------------------------------------------

#Ejercicio 1: Repetir el ejercicio usando un "for" sin "while" pero con la condicion
c <- 0

while(c <= 10) {
  print(c)
  c <- c + 1
}
c

# Respuesta ejercicio 1:
c=0

t <- c(0:10)
for (c in t){
  print(c)
  c <- c + 1
}
c

#Ejercicio 2: Extraer los números pares del 1 al 100 

seq(0,100, by=2)

#Ejercicio 3: Modifique el siguiente código para que el loop rellene un vector con todos los resultados

v <- vector()
for (i in 1:10){
  v[i] <- paste("Numero", i, "")
}
v

#Ejercicio 4: cargue el data "cars" y cree una nueva columna que rellene con "muy rapido" si la velocidad del modelo es mayor 15 o rellene con "muy lento" si es menor a 15

cars1 <- cars[,1]
cars2 <- vector()
for (i in cars1) {
  cars2[i] <-if(i>=15){paste("muy rápido")} else if (i<15){paste("muy lento")}
  karroz <- cbind(cars[,1],cars2[i])
}

karroz

#Ejercicio 5: Cree un dataframe con 1000 filas y 2 columnas, reste la primer columna a la segunda y cree una nueva columna con el resultado. Haga este ejercicio usando "for" y apply.
DF <- data.frame(rnorm(1000),runif(1000))
for(i in 1: dim(DF[2])){
  newDF <- cbind(DF, (DF$rnorm.1000.[i] - DF$runif.1000.[i]))
}
newDF

##########
# ENTORNOS -------------------------------------------------------------------

# Responda las siguientes preguntas y comente las operaciones y valores de cada objeto en cada función
# ejerc 1 ---------------------------------------

func_1 <- function(u){     # func_1(u) función con argumento u
  
  u <<- 2*u   # es un reasignación local o global? global
  
  return(u)
  
}

u <- 1

func_1(u) # es un reasignación local o global?


# ejerc 2 ---------------------------------------

func_1 <- function(u){
  
  u <- 2*u
  
  func_2 <- function(uu){
    u <<- 3*uu    # es un reasignación local o global?
  }
  
  return(u)
  return(func_2(u))
  
}

u <- 1

func_1(u) # es un reasignación local o global?


# ejerc 3 ---------------------------------------

fun_1 <- function(d, j){
  d <- 8
  j <- "esto es local y por defecto"
  paste(d,j, sep = " ")
}

v1 <- 1
v2 <- "esto es global y un argumento"

fun_1()      #porque ignora las variables locales?


# ejerc 4 ---------------------------------------

fun_1 <- function(d = 8, j = "esto es local y por defecto"){
  paste(d,j, sep = " ")
}

fun_1()      #que valores muestra, explique su respuesta

fun_1(d = v1, j = v2)   #que valores muestra, explique su respuesta