#######################################
#####     TAREA DE S3, S4 y R6      ###
#######################################

##########
# S3 -------------------------------------------------------------------

# ejercicio 1.1 Cree un objeto de clase "Ropa" cuyos componentes sean precio y prenda.

objeto <- list()
objeto$precio <- c(100, 150, 200, 250, 300)
objeto$prenda <- c("camisa", "pantalón", "corbata", "medias", "tennis" )
class(objeto) <- "ropa"
class(objeto)

# ejercicio 1.2 cree una función generica para la clase "Ropa" y un método que imprima la prenda y su precio.
vale <- function(x, ...){
  UseMethod("vale",x)
}

vale.ropa <- function(x){
  paste(x$prenda, x$precio, sep = " $")
}

vale(objeto)

# ejercicio 1.3 cree un print para la clase "Ropa" que muestre la suma del valor de todas las prendas.
print.ropa <- function(x, ..){
  print(sum(x$precio))
}
print(objeto)

# ejercicio 2.1 Cree un objeto de clase "Baraja" cuyos componentes sean número y simbolo. Según la baraja española de 40 cartas. https://es.wikipedia.org/wiki/Baraja_espa%C3%B1ola#/media/Archivo:Baraja_de_40_cartas.png
naipe <- list()
class(naipe) <- "baraja"
naipe$numero <- c(1:12)
naipe$simbolo <- c("oros", "copas", "espadas", "bastos")

# ejercicio 2.2 Cree un print que para la clase "Baraja" donde se muestre una descripción breve del objeto y 3 reglas de juego de cualquier variante de la baraja española, puede guiarse de http://www.juntadeandalucia.es/averroes/centros-tic/18700441/myscrapbook/bookcontents.php?page=8&section=7&viewis=&username=
print.baraja <- function(x, ..){
  print(summary(naipe))
  paste("Para jugar Conquián")
  paste("Regla 1: 8 a 10 naipes por jugador", "Regla 2: máximo 4 jugadores", "Regla 3: cada jugador debe intercambiar una carta de su mazo con el jugador del lado")
}
print.baraja(naipe)

# ejercicio 2.3 cree una función generica para la clase "Baraja" que sirva de manera al azar (uniforme sin reemplazo) 9 cartas y se las asigne a un objeto jugador.
barajador <- function(x, ...){
  UseMethod("barajador",x)
}

barajador.baraja <- function(x){
  for (i in seq(1,9)) {
    numerov <- as.matrix(naipe$numero)
    simbolov <- as.matrix(naipe$simbolo)
    j <- sample(c(1:12),1)
    k <- sample(c(1:4),1)
    jugador[i] <- paste(numerov[j,1],simbolov[k,1])
  }

}

barajador(naipe)
jugador

# ejercicio 3. Dado lo siguiente, explique y argumente (con evidencia resultante de diferentes comandos) por qué modificar el objeto "bubba" afecta a "louise" y viceversa? Recuerde comentar el código que use como argumento.

NordAmericain <- function(eatsBreakfast = TRUE, myFavorite = "cereal") {
  thisEnv <- environment()
  hasBreakfast <- eatsBreakfast
  favoriteBreakfast <- myFavorite
  me <- list(
    thisEnv = thisEnv,
    getEnv = function() {
      return(get("thisEnv", thisEnv))   # return reporta el valor de la variable a su ambiente parental, en este caso del objeto nombrado "thisEnv" que es el ambiente global
    }
  )
  assign('this', me, envir = thisEnv)
  class(me) <- append(class(me), "NordAmericain")
  return(me)    # lo mismo sucede en esta linea con el return, devuelve el valor del ambiente parental al objeto me que es la clase
}
# tanto la clase como el ambiente están en el ambiente de la función NordAmericain

bubba <- NordAmericain()
bubba
bubba <- NordAmericain(myFavorite="oatmeal")
bubba

get("favoriteBreakfast", bubba$getEnv())

louise <- bubba     # clona el mismo ambiente, mismos atributos
assign("favoriteBreakfast", "toast", louise$getEnv())  # al asignar el valor "toast" al objeto "favoriteBreakfast" que corresponde a myFavorite con un argumento, en louise$getEnv(). es decir es específico para Louise 
get("favoriteBreakfast", louise$getEnv())              # así se muestra el cambio para Louise
get("favoriteBreakfast", bubba$getEnv())               # Pero no para bubba, pues sigue estando 


##########
# S4 ------------------------------------------------------------------- 

#Cargar paquetes

library(maptools)
library(rgeos)
library(nnclust)
library(shapefiles)


# Ejercicios 1. ¿Que hace el objeto graph_mst? comente cada componente de la función

graph_mst <- function (l) {
  distrib<-l@lines[[1]]@Lines[1][[1]]@coords  
  mst<-mst(l@lines[[1]]@Lines[1][[1]]@coords)
  plot(l, col="white");title("MST")
  segments(distrib[mst$from,1], distrib[mst$from,2], distrib[mst$to,1],distrib[mst$to,2],col="red")
}


# Ejercicios 2. Crear un objeto s4 en donde incluya 30 letras y 30 observaciones aleatorias de una distribucion normal. 


# Ejercicios 3. Usando setGeneric() y setMethod(), escribir dos funciones para extraer los datos del objeto creado en el punto 2 y graficarlos.


# Ejercicios 4. Sin ejecutar que pasa en cada una de laa asignaciones y ¿por qué? comente errores de código y la correción que le realizó

rm(list=ls())


setClass("angelito",
         representation(nombre="character",
                        apellido="character",
                        peso=altura="numeric")
)



setClass("angelito",
         representation(nombre="character",
                        apellido="character",
                        peso  <- "numeric",
                        altura="numeric")
)


setClass("angelito",
         representation(nombre="character",
                        apellido="character",
                        peso="numeric",
                        altura="numeric")
)

yo  <- new("angelito",
           nombre  <- "Daniel",
           apellido  <- "Miranda")


yo  <- new("angelito",
           nombre  = "Daniel",
           apellido  = "Miranda")


yo

setClass("estudiante",
         representation(
           identidad="angelito",
           semestre="numeric",
           grado="logical")
)


yoReal  <- new("estudiante")

yoReal

yoReal$angelito

yoReal@estudiante

yo@nombre

yoReal@identidad <- yo

yoReal

yo@nombre

yoReal@nombre

yoReal@identidad@nombre

ls()


##########
# R6 ------------------------------------------------------------------- 


# Ejercicio 1. Cree una clase de cuenta bancaria R6 que almacene un saldo y le permita depositar y retirar dinero. Cree una subclase que arroje un error si intenta entrar en sobregiro. Cree otra subclase que le permita entrar en sobregiro, pero le cobra una tarifa.

# Ejercicio 2. Cree una clase (parquedero) R6 que almacene diferentes vehiculos y le permita  diferencar motocicletas y automoviles. Cree una subclase para diferenciar el cobro por tipo de vehiculo y otra por la cantidad de tiempo. Adicione otra clase "dia" y finalemente presente los ingresos mensueales.
