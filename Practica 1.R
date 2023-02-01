#Ejercicio 1
x <- c(1,2,3)
y <- c(6,5,4)
x*2
x*y
x[1]*y[1]

#Ejercicio 2
x+x
x<-x+x
y<- x+x
x<-x+1

#Ejercicio 3
vec <- (1:10) ^2
vec <- seq(from = 0, to = 10, length.out = 5)
vec

#ejercicio 4

x <- seq(0,1,0.2)
y <- x*(1-x)
plot(x,y)

#Ejercicio 5
sum((1:100)^2)

#Ejercicio 6
x <- seq(from = 10, to = 30, by = 2)
x[2]
#Cual es la diferencia entre estos
x[1:3*2]
x[1:(3*2)]
x[-1:-3]

#ejercicio 7
hist(rivers)
max(rivers)
min(rivers)
#f 
rivers[which (rivers > 1000)]

#Ejercicio 8
mtcars$disp
mtcars[,3]
mtcars[]
mean(mtcars$carb[mtcars$carb == 2])
