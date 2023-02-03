#Practica 1
#Ejercicio 1
c<--4
b<-2
a<-1

x1 <- (-b + sqrt(b**2-4*a*c))/2*a
x2 <- (-b - sqrt(b**2-4*a*c))/2*a

#Ejercicio 2
roots <- function(a,b,c){
  if(b**2-4*a*c < 0){
    print("Tiene raices imaginarias")
  }
  else{
    x1 <- (-b + sqrt(b**2-4*a*c))/2*a
    x2 <- (-b - sqrt(b**2-4*a*c))/2*a
    return(c(x1,x2))
  }
}
roots(a,b,c)

#Ejercicio 3
set.seed(27)
#a
letraRandom<-function(){
  return(sample(letters,1,replace = FALSE))
}
#b
letrasAlAzar<-function(palabra){
  letra <- sample(letters,1)
  while(letra != "a"){
    palabra <- paste0(palabra,letra)
    letra <- sample(letters,1)
  }
  palabra <- paste0(palabra,letra)
  return (palabra)
}
letrasAlAzar('Jorge')

#c
n<- 5
palabras <- c()
cantidad <- 0
for(i in 1:n){
  palabras <- c(palabras,letrasAlAzar(''))
  cantidad <- cantidad + nchar(palabras[i])
}
promedio <- cantidad / n
palabras

#Ejercicio 4

suma <- 0
iter <- 0
while (suma < 500) {
  x <- runif(1,min =0,max =1)
  suma <- suma + x
  iter<-iter+1
}
suma
iter

#Ejercicio 5

cuadrado <- function(vec){
  vec[which(vec > 0)] <- (vec[vec>0])**2
  return(vec)
}
prueba <- c(1,-8,7,5,-6,16)
cuadrado(prueba)

#Ejercicio 6
mat <- matrix(c(1,3,2,8),2,2,byrow = TRUE)
nulo<- matrix(c(0,0),nrow = 2)
solve(mat,nulo)

#Ejercicio 7
escalar<- function(a,n){
  vec<- rep(c(a,rep(0,times = n)),length.out = n*n)
  return(matrix(vec,n,n))
}
escalar(3,4)

#Ejercicio 8
armarDiagonal<- function(b){
  vec <- c()
  for(i in 1:(length(b)-1)){
    vec <- c(vec,c(b[i],rep(0,times = length(b))))
    print(b[i])
  }
  vec <- c(vec,b[length(b)])
  print(vec)
  return(matrix(vec,length(b),length(b)))
}

armarDiagonal(c(2,4,5))

#Ejercicio 9
astro <- read.csv('/media/clinux01/UBUNTU 22_0/astronauts.csv')
#a
astro$Gender<-factor(astro$Gender)
astro$Status<-factor(astro$Status)

#b
tabla <- table(astro$Space.Flights,astro$Space.Walks)
tabla

#c
maximo <- astro[which.max(astro$Space.Flights),]$Name
minimo <- astro[which.min(astro$Space.Flights),]$Name

#d
plot(astro$Space.Flights,astro$Space.Walks,xlab = 'Vuelos',ylab = 'Caminata')

#e
smoothScatter(astro$Space.Walks..hr.,astro$Space.Flight..hr.,xlab = 'Horas Vuelo',ylab = 'Horas Caminata')

#f

cols <- c()
for(i in 1:length(astro)){
  if(is.numeric(astro[,i])){
    cols <- c(cols,i)
  }
}
cols
dataNuevo <- astro[,cols]
dataNuevo
NAs<-!is.na.data.frame(dataNuevo)
dataNuevo <- dataNuevo[NAs[,1],] #Saco los NA
sumas <- sapply(dataNuevo,MARGIN = 2,sum,na.rm=TRUE)
maximo <- sapply(dataNuevo,Margin = 2, max)
minimo <- sapply(dataNuevo,Margin = 2, min)
promedio <- sapply(dataNuevo,Margin = 2, mean)

datos <- data.frame(Sumas=sumas,Maximo = maximo,Minimo = minimo, Promedio = promedio)
datos
p<-t(datos)
p<-as.data.frame(p)
p
#Preguntar como lo hago al reves, cambio filas por las columnas porque no me salio.
#Preguntar el tipo de dato que queda sumas y porque tiene info de las columnas

help("sapply")
