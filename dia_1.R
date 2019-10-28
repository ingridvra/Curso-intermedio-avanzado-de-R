####################################
### Repaso if/while/ for/repeat/apply
### creador: Luis Manuel Calsada-Rodríguez
### comentado por Ingrid Vanessa Rios Anaya

# 1. if/else
# Estructura
# if (condicion) {
# procedimiento si la condicion es verdadera
# } else {
# procedimiento si la condicion es falsa
# }

# ejemplo
c <- (2)

if (c>=2) {
  print('Mayor/igual a dos') # se puede comilla simple o doble
} else {
  print('menor a dos') # corrijo, cuando else ya no existe
  # condición de igual a dos porque está contenida en el if, 
  # por tanto sería "menor a dos"
}

c>=2

# else/if permite usar más de dos condiciones

# ejemplo
c<-(6)

if (c<=2) {
  print("valor numerico menor/igual a dos")
} else if (2<c & c<=5)  {  #if else para condición intermedia
  print("valor numérico entre 3 y 5")
} else if (5<c & c<8) {
  print("valor numérico entre 6 y 7")
} else {                   #finaliza con else
  print("mayor/igual a 8")
}

###########################

# 2. for/while
# for genera un bucle que repite la aciión que le indique 
# las veces que se le indique en el argumento
# Sintaxis
# for ((elemento) in (objeto)) {
# operación
# }


for (i in 1:10){
  cat ("Número", i, " ") #cat, concatena y muestra la palabra con cada uno de los i y un espacio
}  

# ejemplo creando el vector fuera del for

a <- 1
b <- 2
for (i in 1:5) {
  print(a+b+i)
}

# también funciona cuando se asigna el vector dentro del for pero cambia el ambiente en el  
# que se encuentra dicho vector

for (i in 1:5) {
  a <- 0
  b <- 1
  print(a+b+i)
}

# UN RETO MÁS! intento colocar elementos del for en un vector
z<-vector()
for (i in 1:10){
  z[i]<-paste("Número", i, " ") #clave el uso de llaves para indexar elementos en un vector
} 
z

## diferencias entre cat/print/paste/vector
# cat: concatena y muestra
# print: muestra imprime 
# paste: concatena vector luego de convertir a caracter 
# vector: produce un vector de determinada longitud, si no le indica longitud lo crea y luego 
# puedo asignar la longitud
# otro ejemplo

a<-1
b<-2

for (i in 1:5){
  print(a+b+i)
} # 4 5 6 7 8 en renglones separados


for (i in 1:5){
  cat(a+b+i)
} #45678 todo unido

# "while" función de manera similar a "for" ; mientras se cumpla una condición, se ejecuta 

# sintaxis
# while (condición) {
# operación
# }

# esto es diferente muestra c como cero y el resto de números

c=0

while (c<=10){
  print(c)      # ¿Qué ocurre si elimino el print? dá lo que vale el vector
  c <- c + 1
}
c
### diferente a esto que muestra desde 1 hasta el c que ya sería 11
c=0

while (c<=10){
  c <- c + 1
  print(c)
}
c

########### TAREA repetir el ejercicio anterior usando un "for" ######
c=0

t <- c(1:10)
for (c in t){
  print(c+1)
}
c


# 3. repeat/break/next


a<- 0
repeat {
  a<-a + 1
  print(a)
  if (a>=10) ### al agregar el if se convierte en un while
    break
}

### para secar el pc; is this a joke? yes. Si quieres comprobarlo pon el # antes de break
a<- 0
repeat {
  a<-a + 1
  print(a)
  break
}

# "next" es usado para saltarse ciertos valores del bucle

for (i in 1:100){
  if (i>10)
    next
  print(i)
}

###### TAREA Extraer los números pares del 1 al 100
seq(from=0, to=100, by=2 )


# 5. funciones

# sintaxis
# myfunction <- function (arg1, arg2, ...){  #Argumentos o datos de entrada
#  statements       # acción
#  return (object)  # respuesta 
# }

celsius_a_fahrenheit <- function(temp_C) {
  temp_F <- (temp_C * 1.8) + 32  ### duda es un vector temp_F? si, pero en otro entorno
  return(temp_F)
}

celsius_a_fahrenheit(30)


#### funcion de fahrenheit a celsius, se hace con la equivalencia
fahrenheit_a_celsius <- function(temp_F) {
  temp_C <- (temp_F -32) * 5/9  
  return(temp_C)
}

fahrenheit_a_celsius(200)


#### function de celsius a kelvin
celsius_a_kelvin <- function(temp_C) {
  temp_K <- (temp_C +273.15)  
  return(temp_K)
}

celsius_a_kelvin(200)


###############
# Función huevo cocido, para conocer el tiempo en el que está cocido un huevo

huevo_cocido <- function(temperatura){
  tiempo <- 0
  while (temperatura < 100 && tiempo < 100){
    
    tiempo <- tiempo + 1
    temperatura <- temperatura + 1
    
  }
  
  print ("su huevo estará listo en")
  print (paste("En", tiempo/60, "segundos o", tiempo, "minutos", sep = " "))

}

huevo_cocido(20)

# ejercicio función "huevo perfecto" con tres condicionales, cuando está crudo, en proceso y se quemó

huevo_perfecto <-function(tiempo,temperatura){
  if (tiempo<= 0 | temperatura <=0) {
    print("encienda la estufa")  
  } else if (tiempo>7 & temperatura>100){
    print("su huevo se quema")
  } else if (tiempo==7 & temperatura ==100){
    print("su huevo es perfecto")
  } else if (tiempo<7 & temperatura <=100){
      print("a su huevo le falta tiempo y temperatura")
      } else 
        print("se está cocinando")
}

huevo_perfecto (7,20)
##########################
# 4. apply/lapply/sapply
# familia de funciones que ejecuta sobre el objeto especifico

# Sintaxis
# apply(MI MATRIZ, 1/2/c(1,2), Función) ### 1 para filas 2 para columnas
# conprende 3 argumentos: matriz a la que se le aplica, cte 1=filas o 2=columnas,
# función que será aplicada
m<- matrix(data = (1:10), nrow = 5, ncol=2)
apply(m, 1, mean) 
# qué ocurrirá? 
# "lapply" opera con listas, recibe una lista y devuelve una lista
lm <- as.list(m)
str(lm) # una lista compuesta de lo que quiera matrices, nombres, números etc
lapply(lm, sqrt)
# "sapply" recibe una lista y devuelve un vector
vm<- sapply(lm, sqrt)
vm
# Adicionalmente, existen otras funciones de esta familia como=: "mapply", 
# "tapply", "vapply"

# TAREA ADICIONAL

# cargue el data "cars" y cree una nueva columna que rellene con un "muy
# rápido" si la velocidad del modelo es mayor 15 o rellene con "muy lento"
# si es menor a 15

kars <- as.data.frame(cars)
cars1 <- cars[,1]
cars2 <- vector()
for (i in cars1) {
  cars2[i] <-if(i>=15){paste("muy rápido")} else if (i<15){paste("muy lento")}
  
  kars$cars2 <- cbind(kars,cars2[i])
  
}


# cree un dataframe con 1000 filas y 2 columnas, reste la primer columna a la 
# segunda y cree una nueva columna con el resultado. Haga este ejercicio
# usando "for" y "apply"

 DF <- data.frame(rnorm(1000),runif(1000))
 for(i in 1: dim(DF[2])){
   newDF <- cbind(DF, (DF$rnorm.1000.[i] - DF$runif.1000.[i]))
 }
 
 ### R_profile y entornos
 ## R primero carga ambiente o variables de entorno, luego profile del directorio 
 # donde está o busca el de Home, luego R.history y luego R.data
 # .Rprofile está súper para no estar cargando linea por linea con 
 # datos muy grandes
 # que sucede cuando ponemos punto antes del nombre de la funcion? 
 # con esto se indica que están ocultas
 # en el R profile se puede cargar cosas como cargar siempre que abra
 # ciertas librerias como ggplot ó ape
 # para ver que paquetes están cargados uso --> (.packages())
 # hay variables de entorno inicial-> funciones y arranque
 
 ###### entornos 1 ##############
 ################################
 # la zona o ambiente donde funciona
 # para mostrar el ambiente
 environment ()
 # el ambiente cuando uno reasigna pues deja uno solo 
 x<-1
 x
 x<-"esto es x"
 x
 ls()
 
 ## Funciones
 # Entorno de una función es propio de esa función,donde los objetos creados
 # y usados son almacenados
 
 obj_1 <- 1
 obj_2 <- "a"
 
 c <- function(){
   x <- 0
 }
 ls() # Porqué no listan las dos x, porque una pertenece al entorno local de la función que la contiene
 x
 str(c) # la estructura de la función muestra los atributos, uno de ellos es el ambiente donde se encuentra
 
 #### variables globales ## 
 # se encuentran en el ambiente más general, parental es global
 
 global_ambiente <- 10          # objeto en entorno golbal
 
 fun_1 <- function() {          # segundo entorno
   amb_1 <- global_ambiente + 1
   fun_2 <- function() {        # tercer entorno
     amb_2 <- amb_1 + 1
     print(ls())
     print(amb_2)
     print(environment())
   }
   print(ls())
   print(amb_1)
   print(environment())
   fun_2()                    # ejecuta de una vez la fun_2
 }
 
 fun_1() # como resultado, dos ambientes diferentes
 
 
 ### variables locales
 # existen únicamente en el entorno específico en el que fueron creadas
 
 a <- 20            # es variable global
 fun_1 <- function(){
   a <- 10          # es local para fun_1
   print(a)
   
   fun_2 <- function(){
     a <- "hello"  # es local para fun_2
     return(a)
   }
   fun_2()
 }
 fun_1()
 
 a       #la a global 
 
 
 ### Retornar valores y asignar valores a varibales globales desde el ambiente local 
 # y ahora con doble << osea cambiar el esquema de aignacion
 # asigna a la a del entorno anterior osea modifica la variable global
 
 a <- 20          # es variable global
 fun_1 <- function(){
   a <<- 10       # asigna un valor numérico a la variable a del entorno global
   print(a)
   
   fun_2 <<- function(){
     a <- "hello"
     return(a)    #retorna el valor de una variable a su ambiente parental
   }
   fun_2()
 }
 fun_1()
 
 a       #la a global se modificó
 
 
 ### ejercicio en clase
 
 w <- 1
 f <- function(y,fun){
   d <- 8
   print(environment(fun))
   return(fun(d,y))}
 
 h <- function(dee, yyy){
   return(dee*(w+yyy))} #tocó definir a w como 1 en el ambiente global 
 
 d <- 12
 
 # ¿Cuál  es el env?  R_GlobalEnv>
 f(3,h)    # le damos 2 valores a los argumentos, uno es una cte y el otro 
 # la fun que pide, en este caso la funcion h,  los demás valores
 # que necesita los asignamos d <- 8 y el 3 que le dimos en f(3,h)
 
 
 ###### entornos 2 ##############
 ################################
 
 showframe <- function(upn){
   # determinando el ambiente propio
   if (upn < 0){
     env <- .GlobalEnv    # . antes de función, están ocultas
   } else {
     env <- parent.frame(n=upn+1)
   }
   vars <- ls(envir=env)
   for (vr in vars){
     vrg <- get(vr, envir = env)
     if (!is.function(vrg)){
       cat (vr, ":\n",sep="")
       print(vrg)
     }
   }
 }
 
 showframe(-1)
 
 showframe(0)
 
 showframe(2)
 
 ls()
 
 ###
 g <- function(aa){
   b <- 2
   showframe(0)
   showframe(1)
   #  aab <- h(aa+b) ### AQUI HAY UN ERROR porque h necesita 2 argumentos 
   # entonces en vez de un + va una coma
   aab <- h(aa,b)
   return(aab)
 }
 
 g()  # sin valor por omisión
 g(1) # listo
 f()  # sin valor por omisión
 f(1) # listo
 m <- rbind(1:3, 20:22)
 m
 
 ## ojo! cuando uno asigna una funcion a otra copiada, tambien se copian
 # o se crean las variables para cada funcion, lo que nos explicaban 
 # con los counter
 
 # Revisar <<
 
 two <- function(u){
   u <<- 2*u
   z <- 2*z
 }
 
 x <- 1
 z <- 3
 
 u   # no existe, hasta que se ejecute la función
 
 two(x)
 
 #
 
 u   # listo, es = 2
 
 # con este ejercicio comprobamos una vez más el sentido de usar la función
 # return()
 x <- 6699
 x
 
 f <- function(){
   x <- 6
   inc <- function(){ x <<- x + 1; return(x)} 
   inc()
   cat("número", x, "algo pasa")
   return(x)
 }
 
 ls()
 f()
 
 x   # y sigue teniendo el mismo valor 6699
 
 rm(x)
 
 # x <- 1
 
 # revisar assign
 
 z <- 1
 
 two <- function (u){
   assign("u",2*u,pos = .GlobalEnv)
   z <- 2*z
 }
 ls()
 
 # rm(u)
 two(x)
 
 x
 
 u
 
 # bad blood closures
 
 counter <- function(){
   ctr <- 0
   f <- function(){
     ctr <<- ctr + 1
     cat("this count currently has value", ctr, "\n")
   }
   return(f)
 }
 
 c1 <- counter()  # asignamos una funcion a c1
 c2 <- counter()  # clonamos la función
 
 c1
 c2
 
 ctr # objeto no encontrado
 
 c3 <- counter() # clonamos una vez más
 
 c1 () # 1  <environment: 0x555ac9afb0f8>
 
 c2 () # 1  <environment: 0x555ac9b21e48>
 
 c3 () # 1  <environment: 0x555aca097648>
 
 c3 () # cambia a 2 pues se ejecutan independiente pues aunque
 # tengan la misma función el ambiente es diferente, son clones
 
 # donde están ctr (en plural) ? 
 # cada uno en su ambiente


