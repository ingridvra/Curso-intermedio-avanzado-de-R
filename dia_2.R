## Objetos S3

rm(list = ls()) # elimina todos los objetos del ambiente 

install.packages("sloop")#Instala el paquete "sloop"
library(sloop)#Carga el paquete

# Existen tipos de objetos en el  lenguaje R, los tipo S3 son los comúnmente más utilizados, se usan en los paquetes base y paquetes CRAN
# los objetos (00) ya traen un atributo clase predefinido
# Objeto base o "type base"
n <- 1:10
attr(n, "class")  # attr () permite conocer o configurar si el elemento x tiene algún atributo, este no tiene clase

otype(n)  # otype () indica que objeto estoy manejando si es; objeto base, s3, s4, RC o R6


# Objeto OO
attr(mtcars, "class") # tiene clase data.frame

otype(mtcars) # es objeto s3 porque es del paquete base

# Para definir un s3 es como una lista con su atributo "clase" establecido en algún nombre de clase, es un objeto S3. 


# Ejemplo objeto S3

ejemplo_S3 <- list() # creando una lista vacía

ejemplo_S3$datos  <-  c(1,2,3) # incluyendo datos

ejemplo_S3$conteo  <-  5 # incluyendo datos


str(ejemplo_S3) # es una lista de 2

typeof(ejemplo_S3) # typeof () determina el modo de almacenamiento de cualquier objeto, este caso es list

class(ejemplo_S3) # list es el tipo base del objeto, clase es el estilo de programación

# el atributo clases permite darle funciones de esa clase especifica


# Establecemos la clase
class(ejemplo_S3)  <- "myClass"  # asignando clase como "myClass" al objeto ejemplo_S3

class(ejemplo_S3)

otype(ejemplo_S3) # objeto tipo S3


# ejemplo_S3 <- structure(list(), class = "my_class")


# Otro ejemplo  

x <- c(1,2,3)

y <- c(1,3,8)

lmout <- lm(y ~ X) ## error por X mayus no como el vector que era minus
lmout <- lm(y ~ x) # corregido

str(lmout)

typeof(lmout) # este es el tipo base

class(lmout) # su propia clase se llama lm porque toma la clase de la funcion

otype(lmout) # S3 


## Funciones genéricas  

print # sin parentesis me da la str, la funcion, el ambiente
print() # faltan argumentos

summary # entorno base

plot # entorno graphics, plot muestra entorno basica de graphics es latix

...

class # esta por curiosidad que es? es una funcion de clase primitiva

# Creemos una función genérica
hola <- function(x,..){
  UseMethod("hola",x)
}

## Métodos

# Cuando se usa una función genérica R redirige el llamado al método de clase adecuado
# Los objetos de cada clase se imprimen con un método específico a partir de la función genérica print.

class(x)
print(x)

class(lmout) 
print(lmout) # muestra el resultado de la funcion lm

# Revisemos todos los métodos de la función print
# Los asteriscos indican métodos no visibles.
methods(print) # diferente metodos para diferentes objetos con diferentes clases

# Los nombres de los métodos tienen el formato generic_name.class_name (). 

class(ejemplo_S3)

# qué imprime ?
Print(ejemplo_S3)


typeof(ejemplo_S3)


# creemos un método para print de la clase
print.myClass <- function(x){
  cat (x$datos,"\n","estos son mis datos")
}

print.myclass() # x está ausente sin valor por omisión

print(ejemplo_S3) #lista es el tipo base del objeto por eso lo print 
#con el método lista, porque myclass no sabe como tratarlo
str(ejemplo_S3) # es diferente a estructura

# el metodo de las funciones se puede alterar pero las funciones no

# Ahora usemos la función "hola" en este objeto
# ¿qué sucede ?
hola(ejemplo_S3) # no se ha definido el método para la clase myclass

# definiendo un método para la clase myClass 
hola.myClass <- function(x){
  paste(x$datos, "hola", sep = " ")
}

hola(ejemplo_S3) # ahora si muestra= "1 hola" "2 hola" "3 hola"


# Creemos un método de la función para la clase "myClass"
hola.myClass <- function(x,..){ ## definimos un argumento de entrada pero si cambio de opinion puedo luego agregar más
  cat("me amo",x[conteo], "\n") # está mal en lugar de llamar con [] debe ser con $ para lista
}

# Cuál es la salida?
hola(ejemplo_S3) #  Error objeto conteo no encontrado

# corrijo
hola.myClass <- function(x,..){ ## definimos un argumento de entrada pero si cambio de opinion puedo luego agregar más
  cat("me amo",x$conteo, "\n") # está mal en lugar de llamar con [] debe ser con $ para lista
}
# Cuál es la salida?
hola(ejemplo_S3) # sale -> me amo 5

# prueba 
hola(lmout) #no puedo aplicar un método a un objeto de clase diferente este es de clase lm

## Repaso de objeto S3

# Cada componente de la lista es una variable
j <- list(name="Joe", salary=55000, union=T)

# Definimos el atributo clase 
class(j) <- "employee"
class(j)

# Imprimamos el objeto j ¿qué estructura tiene?  # es una lista de clase employee con 3 variables o elementos
print(j)

# Creemos el método "print" para la clase "employee" 
print.employee <- function(wrkr) {
  cat(wrkr$name,"\n")
  cat("salary",wrkr$salary,"\n")
  cat("union member",wrkr$union,"\n")
}

# Ahora, cualquier objeto de la clase "employee" sera impreso con el método print.employee
print(j)


## Herencia 

# La nueva clase hereda los métodos de la clase antigua

# Si la nueva clase no tiene métodos entonces toma los métodos la clase anterior

test01  <- list()

test01$datos  <-  c(1,2,3)

test01$conteo  <-  5

test01$letras  <-  "This is ok"

class(test01)  <- "cosa"

class(test01)

str(test01)   # como no definimos el método, se muestra como una lista

typeof(test01) #efectivamente es una lista


# creamos el metodo para la clase
print.cosa <- function(x, ...){          # ... significa que luego se pueden añadir otros argumentos
  "1_ Si!!!!!!!!!!!!, lo logramos!!"
}

print(test01)

# la clase tambien puede ser un conjunto de caracteres
class(test01) <- c("cosa" ,"cosa1", "cosa2", "CCc") # crea 4 clases diferentes para ese objeto con esa jerarquia especifica

class(test01)

# ¿ Qué debe pasar?
print(test01)  # se mestra con el método configurado para la clase cosa

# creemos un método para la siguiente clase del conjunto
print.cosa1 <- function(x, ...){
  "2_ ni idea si lo logramos!!"
}
# método para cosa2
print.cosa2 <- function(x, ...){
  "3_ ni idea si lo logramos!!"
}

# ¿Qué pasa?
print(test01)

class(test01)

# método para CCc
print.CCc <- function(x, ...){
  "4_ ni idea si esta vez si lo logramos!!"
}

print(test01) # sigue usando el método de cosa pues es el primero


rm(print.cosa) # así va usar el metodo que sigue en la jerarquia


print.cosa1(test01) # usa el método específico que yo le digo

##########################
# objetos s4 y R6

# tambien hay listas 
# objetos orientados a programación de objetos dirigidos a crear herramientas par realizar una función
# ejs: entornos java, bioconductor, carthographic data for spatial analysis
###
# S4: reescritura de objetos s3, mantiene la misma estructura de s3 en R: definidos por una clase, especificado por genericos y tener un metodo
# sistema riguroso y formal (primero la clase y luego el objeto)
# ya no son componentes a los que podiamos añadir con $ sino ahora son slots accedemos con @
# clase, defino con new, asigno metodo para la clase, y asignar genéricos 
# la sintaxis cambia en s4 simbolos diferentes, orden cambia, un método puede ser generico 

##Generalidades de objetos S4
##Con base a:

## Matloff, Norman S.The art of R programming : tour of statistical software design

## http://www.stat.umn.edu/geyer/3701/notes/generic.html

## https://adv-r.hadley.nz/s4.html


#
# un objeto S4
#

#Un objeto s4 es un sistema riguroso y formal, que surge de la reescritura de objetos s3, introducido por Chambers (1998). Este tipo de objeto lleva a pensar cuidadosamente su dise?o, ya que est? particularmente adecuado para construir grandes sistemas que evolucionan con el tiempo y que posiblemente recibir?n contribuciones de muchos programadores. A pesar de que s4 requiere m?s trabajo que S3, este objeto proporciona m?s garant?as y una mayor encapsulaci?n. 


## Un ejemplo de objeto s4
rm(list=ls())

setClass("Car",representation=representation(
  price = "numeric",
  numberDoors="numeric",
  typeEngine="character",
  mileage="numeric"
))

ls() # hasta aquí no hay nada porque solo hemos establecido la clase

aCar <- new("Car",price=20000,numberDoors=4,typeEngine="V6",mileage=143)

ls() # ya aparece un objeto llamado aCar
class(aCar) # tiene clase llamada car de entorno global

## Un objeto s4 trabaja similar que un objeto s3, Los objetos s4 tienen una clase formal definida, la cual describe la representaci?n y herencia de cada clase. Adicionalmente, los objetos s4 tiene funciones auxiliares especiales para definir gen?ricos y m?todos. S4 tambi?n tiene distribuci?n m?ltiple, lo que significa que las funciones gen?ricas pueden elegir m?todos basados en la clase de cualquier n?mero de argumentos.

#### Instalar y cargar el paquete stats4 y pryr 

## S4 se implementa en el paquete de m?todos b?sicos, que siempre se instala con R.

#install.packages("stats4") #Instalar paquete
library(stats4)#Cargar paquete

#install.packages("pryr")#Instalar paquete
library(pryr)#Cargar paquete

#install.packages("sloop")#Instalar paquete
library(sloop)#Cargar paquete

## sloop proporciona ayudas como  sloop :: otype (), esto facilita descubrir el sistema OOP (object-oriented programming) utilizado por un objeto. 

##############################################


### Las ideas subyacentes de los objetos s4 son similares a S3, sin embargo, la implementaci?n es mucho m?s estricta y hace uso de funciones especializadas para crear clases: setClass (), gen?ricos: setGeneric () y m?todos: setMethod ().


### setClass: 
#### Para definir una clase S4 llame a setClass () con el nombre de la clase y una definición de sus slots.

setClass("Person",
         slots = list(name = "character", age = "numeric"))
setClass("Employee",
         slots = list(boss = "Person"),
         contains = "Person")

### para construir objetos dentro de la clase ya definida use new() con el nombre de la clase y valores para los slots.
alice <- new("Person", name = "Alice", age = 40)
john <- new("Employee", name = "John", age = 20, boss = alice)
juan <- new("Employee", name = "Juan", age = 21)

juan


class(juan)
class(alice)


newPerson <- c(alice,  john)
str(newPerson)
ls()
class(newPerson) # es una lista, de S4 pero al fínal sigue siendo una lista tipo base

otype(juan)
otype(newPerson)

## Un importante componente de S4 son los slots @ (ranura en español). 

alice@age

#> [1] 40
newPerson
newPerson@age

slot(john, "boss")

### slot new person age??

#> Un objeto de clase "Person"
#> Slot "name":
#> [1] "Alice"
#> 
#> Slot "age":
#> [1] 40

#### 

john@boss@name
### ???


## Ejemplo 2


setClass("employee",
         representation(
           name="character",
           salary="numeric",
           union="logical")
)

#####

Daniel <- new("employee",name="",salary=) # no se puede llamar el slot y no asignarle nada


#Que hace falta?

## ejercicio 
### Al objeto Daniel, definir un nuevo empleado con nombre Daniel y asignarle un salario de 55000 

ls()
Daniel <- ("employee",name="Daniel",salary= 55000)


############

### EL empleado Joe tiene un salario igual que Daniel 


joe <- new("employee", name= "joe", salary= 55000)

joe
Daniel

#####

joe@salary

slot(joe,"salary")

show(joe)

ls()

##


##Ejemplo 3

setClass("RangedNumeric",
         contains = "numeric",
         slots = list(min = "numeric", max = "numeric"))
rn <- new("RangedNumeric", 1:10, min = 1, max = 10)

rn <- new("RangedNumeric", contains= 10, 1:10, min = 1, max = 10)

class(rn)
otype(rn)
rn@min
#> [1] 1
rn@.Data
juan@.Data


#>  [1]  1  2  3  4  5  6  7  8  9 10
rn

#########################################################################3
############## setGeneric y setMethod
### setGeneric realiza el envío del método 
setGeneric("union")
#> [1] "union"

##setMethod: define el m?todo de lo que pretende hacer (funciones)
setMethod("union",
          c(x = "data.frame", y = "data.frame"),
          function(x, y) {
            unique(rbind(x, y))
          }
)
#> [1] "union"

#####################

#solo setMethod 
ls()

### Crear un empleado con su nonbre y que su salario sea 55000

Vane <- new("employee",name = "vane",salary=55000, union=" ") 

show(vane)

### que hace show?

setMethod("show", "employee",
          function(object) {
            inorout <- ifelse(object@union,"is","is not")
            cat(object@name,"has a salary of",object@salary,
                "and",inorout, "in the union", "\n")
          }
)



show(Vane)
print(Vane) ## curioso muestra lo mismo que print




### Ejemplo 4 

setGeneric("myGeneric", function(x) {
  standardGeneric("myGeneric")
})
#> [1] "myGeneric"
class(myGeneric)
otype(myGeneric)

### probando que hace set generic
setGeneric("show", "employee",
           function(object) {
             inorout <- ifelse(object@union,"is","is not")
             cat(object@name,"has a salary of",object@salary,
                 "and",inorout, "in the union", "\n")
           }
) ## error in setGeneric

################################################
### ejercicio 2
####################################

#parte 1
setClass("Person", 
         slots = c(
           name = "character", 
           age = "numeric"
         )
)

#### crear un persona llamada john Smith, sin embargo no se conoce la edad de jhon
###como asignaria NA a age si es numeric ???

john <- new("Person", name="john Smmith", age="NA")


john@name

slot(john, "age")




## Crear un setter y getter para el slot (age) creando genericos con setGeneric()


setGeneric("age", function(x) standardGeneric("age"))
setGeneric("age<-", function(x, value) standardGeneric("age<-"))

## Despues definir metodos con setMethod():

setMethod("age", "Person", function(x) x@age)
setMethod("age<-", "Person", function(x, value) {
  x@age <- value
  x
})

john

john@age <- 12

john


#parte 2
setClass("Person", 
         slots = c(
           name = "character", 
           age = "numeric"
         ), 
         prototype = list(
           name = NA_character_,
           age = NA_real_
         )
)

me <- new("Person", name = "Hadley")
str(me)


#########################################################
###Ejercicio 2

######################################################
## Evaluates the effect of Ramer - Douglas - Peucker on simpliying tracks  
##
## DRME nov 04 - 2012
## dmiranda(at)uis(dot)edu(dot)co

########Cargar paquetes
## libraries
##

library(maptools)
library(rgeos)
library(nnclust)
library(shapefiles)

##

## functions
##¿Que hace el objeto graph_mst?


graph_mst <- function (l) {
  distrib<-l@lines[[1]]@Lines[1][[1]]@coords
  mst<-mst(l@lines[[1]]@Lines[1][[1]]@coords)
  plot(l, col="white");title("MST")
  segments(distrib[mst$from,1], distrib[mst$from,2], distrib[mst$to,1],distrib[mst$to,2],col="red")
}

# Si quiere llevar los objetos s4 a la práctica, Existen dos retos: No hay referencias que responda a todas sus preguntas sobre S4 y la documentación incorporada de # R en ocasiones es contradictoria.



######################################
############################ Actividades #############################################



# 1.Crear un objeto s4 en donde incluya 30 letras y 30 observaciones aleatorias de una distribucion normal. 
# 2.Usando setGeneric() y setMethod(),escribir dos funciones para extraer los datos del objeto creado en el punto 1 y graficarlos.


######################################
############################# Literatura recomendada #############################################

# 1.Chambers, John M. 1998. Programming with Data: A Guide to the S Language. Springer. 

#2.http://heather.cs.ucdavis.edu/~matloff/132/NSPpart.pdf

#3.https://adv-r.hadley.nz/s4.html

#4.Chambers, John M. 2014. "Object-Oriented Programming, Functional Programming and R." Statistical Science 29 (2). Institute of Mathematical Statistics:167-80. https://projecteuclid.org/download/pdfview_1/euclid.ss/1408368569.


##
## Sin ejecutar que pasa en cada una de laa asignaciones y ¿por qué?
##

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


###
# R6: métodos pertenecen a objetos no a genéricos
# vuelve a aparecer el simbolo $
# encapsulamiento
# control de acceso(privado(invisible).. o publico)
# prerequisitos paquete R6
# se define la clase como R6class dentro de la clase tenemos una lista de argumentos que es sum y add
# para agregar un nuevo objeto es con new() pero de esta forma "x <- Accumulator$new()"
# llamada del argumento que era add, 
# en R6 se puede heredar el comportamiento de una clase ya existente con inherit
# en r6 sintaxis tambien cambia, simbolos, lo primero que se hace es especificar la clase como r6class, el orden 
# se puede hacer todo al tiempo no con el orden de los s4, aparte se puede heredar la clase con super$


#~ #~  it uses the encapsulated OOP paradigm, which means that methods
#~ #~  belong to objects, not generics, and you call them like
#~ #~  object$method().


#~ #~  R6 objects are mutable, which means that they are modified in
#~ #~  place, and hence have reference semantics.

#~  install.packages("R6")

library(R6)

rm(list=ls())
#~ Clases y métodos
#~ Los dos argumentos más importantes para R6Class():

#~  1) classname: no es estrictamente necesario, pero mejora los mensajes de error y permite utilizar objetos R6 con genéricos S3. Por convención, las clases R6 tienen UpperCamelCasenombres.

#~  2)  public: proporciona una lista de métodos (funciones) y campos (cualquier otra cosa) que componen la interfaz pública del objeto. Los métodos pueden acceder a los métodos y campos del objeto actual a través de self$.



Accumulator <- R6Class("Accumulator", list(
  sum = 0,
  add = function(x = 1) {
    self$sum <- self$sum + x  ## lo que está dentro de este metodo en lugar de accumulator$sum 
    invisible(self)
  })
)



Accumulator


#~ construir un nuevo objeto de la clase mediante new(). En R6, los métodos pertenecen a los objetos, por lo que se utiliza $para acceder a new():


x <- Accumulator$new() 

x


#~ #~  mediante $ se accede a los campos.

x$add(0)


#~ en esta clase, los campos y métodos son públicos, lo que significa que puede obtener o establecer el valor de cualquier campo. 

#~  "Método de encadenamiento"

#~ $add()se llama principalmente por su efecto secundario de la actualización $sum

Accumulator <- R6Class("Accumulator", list(
  sum = 0,
  add = function(x = 1) {
    self$sum <- self$sum + x #self$ es metodo publico
    invisible(self) ## invisible es un atributo del método
  })
)



#~ Los métodos de efectos secundarios R6 siempre deben volver self invisiblemente. Esto devuelve el objeto "actual" y permite encadenar varias llamadas a métodos:


x


x$add(10)

x

x$add(10)$add(10) ### invisible permite varias llamadas como aqui

x
x$add(10)$add(10)$sum

x

#~ #~ #~  Hay dos métodos importantes que deben definirse para la mayoría de las clases: $initialize()y $print()

#~  $initialize() anula el comportamiento predeterminado de $new()

Person <- R6Class("Person", list(
  name = NULL,
  age = NA,
  initialize = function(name, age = NA) {
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.numeric(age), length(age) == 1)
    
    self$name <- name
    self$age <- age
  }
))

hadley <- Person$new("Hadley", age = "thirty-eight")
#~ > Error in .subset2(public_bind_env, "initialize")(...): is.numeric(age) is
#~ > not TRUE

hadley2 <- Person$new("Hadley", age = 38)


hadley2$age

#~ #~  $print() permite anular el comportamiento de impresión predeterminado.  $print()debería regresar invisible(self).

Person <- R6Class("Person", list(
  name = NULL,
  age = NA,
  initialize = function(name, age = NA) {
    self$name <- name
    self$age <- age
  },
  print = function(...) {
    cat("Person: \n")
    cat("  Name: ", self$name, "\n", sep = "")
    cat("  Age:  ", self$age, "\n", sep = "")
    invisible(self)
  }
))

hadley2 <- Person$new("Hadley")
hadley2
hadley2$print
hadley2$print()

#~ Agregar métodos después de la creación
#~ En lugar de crear continuamente nuevas clases, también es posible modificar los campos y métodos de una clase existente. Esto es útil cuando explora interactivamente o cuando tiene una clase con muchas funciones que le gustaría dividir en pedazos. 


Accumulator <- R6Class("Accumulator")

Accumulator$set("public", "sum", 0)
Accumulator$set("public", "add", function(x = 1) {
  self$sum <- self$sum + x 
  invisible(self)
})


#~ #~  herencia

#~ Para heredar el comportamiento de una clase existente, proporcione el objeto de clase al argumento inherit 



AccumulatorChatty <- R6Class("AccumulatorChatty", 
                             inherit = Accumulator,
                             public = list(
                               add = function(x = 1) {
                                 cat("Adding ", x, "\n", sep = "")
                                 super$add(x = x)
                               }
                             )
)


x2 <- AccumulatorChatty$new()

x2

x2$add(10)

x2$add(10)$add(1)$sum

x2


#~ Cada objeto R6 tiene una clase S3 que refleja su jerarquía de clases R6. Esto significa que la forma más fácil de determinar la clase (y todas las clases de las que hereda) es usar class():

hadley
class(hadley)

names(hadley)

hadley2
class(hadley2)

names(hadley2)


#~ ejercicios

#1
#Defina a fabrica(factory) para microondas(a microwave oven).
#llame una clase R6Class.
#El nombre de la clase debe ser "MicrowaveOven".
#Determine un elemento que se llame private y que sea una list (asi como puede tener elementos publicos, se pueden tener elementos privados )
#Esta listdebe contener un unico campo, que se llame  power_rating_watts y que tenga un valor de  800.

# Defina microwave_oven_factory
microwave_oven_factory <- r6class(microwave_oven_factory
                                  "___",
                                  ___ = ___(
                                    ___ = ___
                                  )
)

AccumulatorChatty <- R6Class("AccumulatorChatty", 
                             inherit = Accumulator,
                             public = list(
                               add = function(x = 1) {
                                 cat("Adding ", x, "\n", sep = "")
                                 super$add(x = x)
                               }
                             )
)

# Ver su  microwave_oven_factory

# Crear un nuevo objeto MicrowaveOven  y asignelo a la variable microwave_oven

microwave_oven <- 
  
  
  
  #~ Tarea
  
  #~ 1)Cree una clase de cuenta bancaria R6 que almacene un saldo y le permita depositar y retirar dinero. Cree una subclase que arroje un error si intenta entrar en sobregiro. Cree otra subclase que le permita entrar en sobregiro, pero le cobra una tarifa.
  
  #~ 2) Cree una clase (parquedero) R6 que almacene diferentes vehiculos y le permita  diferencar motocicletas y automoviles. Cree una subclase para diferenciar el cobro por tipo de vehiculo y otra por la cantidad de tiempo. Adicione otra clase "dia" y finalemente presente los ingresos mensueales.
  
  
  



