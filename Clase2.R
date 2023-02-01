#Ejercicio 1a
t <- sum(1:1000)
vector <- (1:10000)
help("seq")

#Ejercicio 1b
acum <- 0
indice <- 0
while (acum < 10000) {
  indice <- indice + 1
  acum <- acum + indice
}
indice

#Ejercicio 1c

vector <- c(1,1,1,1,-1)

sumaPositiva <- function(vec){
  return(sum(vec[vec > 0]))
}

sumaPositiva(vector)

#Ejercicio 2
x <- seq(0,50,length = 100)
plot(x, sin(x), type = "l", col = 2, xlab = "tiempo", ylab = "mA", main = "datos capacitor", asp = 10, xlim = c(0,30))
lines(x,cos(x),col = 3)
lines(x,cos(x**2),col = 4)
help("plot")

#Ejercicio 3
autos <- read.table("/media/clinux01/UBUNTU 22_0/autos.txt", header = TRUE)

autos[3,] 

autos[,2]
autos$calidad
autos[2]

help(which.min)
autos[which.min(autos$precio),2]

sum(autos[(1:4),1])

help(apply)

apply(autos,2,sum)

apply(autos,1,sum)

help(plot)

plot(autos$precio,autos$calidad, type = "p", col = 3)

autosOrdenados <-autos[order(autos$precio),]

#Ejercicio 4

cars <- mtcars

rownames(cars[cars$gear == 4,])

rownames(cars[(cars$am == 0 & cars$gear == 4),])

rownames(cars[(cars$am == 0 | cars$gear == 4),])

cars$am <- factor(cars$am,c(0,1),c("Manual","Automàtico"))



#Pruebas
vec<-c(54,84,45,21,8)
mat <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, ncol = 3)
apply(mat,1,sum)
