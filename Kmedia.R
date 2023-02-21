distancia <- function(a,b){
  return(sqrt(sum((a-b)**2)))
}

library(ggplot2)
library(gridExtra)

n.iter <- 8
k.clus <- 3
x <-pengu$flipper_length_mm
y <- pengu$bill_length_mm
cat <- rep(NA,344)

#Limpio los NAs
data <- data.frame("X" = x, "Y" = y, "Cluster" = cat)
data <- data[!is.na(data$X) & !is.na(data$Y),]

ggplot(data,aes(X,Y))+geom_jitter()

set.seed(27)
#Creo empty dataframe
columns = c("X","Y","Xviejo","Yviejo") 
centroides = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(centroides) = columns

random <- sample(1:length(x),k.clus,replace = F)

for (i in 1:k.clus){
  centroides <- rbind(centroides,data[random[i],1:2])
}

centroidesAnteriores <- data.frame("X" = rep(0,k.clus),"Y"=rep(0,k.clus))

calculo <- sqrt(apply((centroides-centroidesAnteriores)**2,1,sum))

ggplot(centroides,aes(X,Y))+geom_point(shape=4,color="blue",size=4)

matriz <- matrix(rep(NA,dim(data)[1] * k.clus), nrow=dim(data)[1],ncol = k.clus)

#Bucle
iter <- 0
distanciaFinal <- c()
while(max(calculo) > 0.05){
  for (i in 1:dim(data)[1]){
    for(j in 1:k.clus){
        matriz[i,j] <- distancia(data[i,1:2],centroides[j,])
    }
  }
  cat <- apply(X = matriz,MARGIN= 1,FUN = which.min)
  distanciaFinal[iter+1] <- sum(apply(X = matriz,MARGIN= 1,FUN = min))
  
  data[,"Cluster"] <- factor(cat)
  
  centroidesAnteriores <- centroides
  #Nuevos centros
  for (centro in 1:k.clus) {
    centroides[centro,] <- c(mean(data[data$Cluster==centro,"X"]),mean(data[data$Cluster==centro,"Y"]))
  }
  calculo <- sqrt(apply((centroides-centroidesAnteriores)**2,1,sum))
  print(max(calculo))
  iter <- iter + 1
}

error <- data.frame("X" = 1:length(distanciaFinal),"Y" = distanciaFinal)
g1 <-ggplot(error,aes(X,Y))+geom_jitter()
  
ggplot(data,aes(X,Y,color=Cluster))+geom_jitter() + 
geom_point(data=centroides,aes(X,Y),shape=4,size=4,color="blue")
g1

#----------------------------------------------------------------------
kMeans <- function(a,b,k.clus,n.iter){
  
  #Limpio los NAs
  data <- data.frame("X" = a, "Y" = b, "Cluster" = rep(NA,length(a)))
  data <- data[!is.na(data$X) & !is.na(data$Y),]
  ggplot(data,aes(X,Y))+geom_jitter()
  
  set.seed(27)
  #Creo empty dataframe
  columns = c("X","Y") 
  centroides = data.frame(matrix(nrow = 0, ncol = length(columns))) 
  colnames(centroides) = columns
  
  random <- sample(1:length(x),k.clus,replace = F)
  
  for (i in 1:k.clus){
    centroides <- rbind(centroides,data[random[i],1:2])
  }
  
  ggplot(centroides,aes(X,Y))+geom_point(shape=4,color="blue",size=4)
  
  matriz <- matrix(rep(NA,dim(data)[1] * k.clus), nrow=dim(data)[1],ncol = k.clus)
  
  #Bucle
  iter <- 0
  distanciaFinal <- c()
  while(iter < n.iter){
    for (i in 1:dim(data)[1]){
      for(j in 1:k.clus){
        matriz[i,j] <- distancia(data[i,1:2],centroides[j,])
      }
    }
    cat <- apply(X = matriz,MARGIN= 1,FUN = which.min)
    distanciaFinal[iter+1] <- sum(apply(X = matriz,MARGIN= 1,FUN = min))
    
    data[,"Cluster"] <- factor(cat)

    
    #Nuevos centros
    for (centro in 1:k.clus) {
      centroides[centro,] <- c(mean(data[data$Cluster==centro,"X"]),mean(data[data$Cluster==centro,"Y"]))
    }
    
    iter <- iter + 1
  }
  
  error <- data.frame("X" = 1:length(distanciaFinal),"Y" = distanciaFinal)
  g1 <-ggplot(error,aes(X,Y))+geom_jitter()
  
  g2 <- ggplot(data,aes(X,Y,color=Cluster))+geom_jitter() + 
    geom_point(data=centroides,aes(X,Y),shape=4,size=4,color="blue")+xlab("Flipper")+ylab("Bill length")+
    ggtitle("Cluster de pinguinos")
  
  
  return(list(data$Cluster,centroides,distanciaFinal[length(distanciaFinal)],g2))
}



#-----------------------------------------------------------------------
kMeans2 <- function(a,b,k.clus){
  
  #Limpio los NAs
  data <- data.frame("X" = a, "Y" = b, "Cluster" = rep(NA,length(a)))
  data <- data[!is.na(data$X) & !is.na(data$Y),]
  ggplot(data,aes(X,Y))+geom_jitter()
  
  set.seed(27)
  #Creo empty dataframe
  columns = c("X","Y") 
  centroides = data.frame(matrix(nrow = 0, ncol = length(columns))) 
  colnames(centroides) = columns
  
  random <- sample(1:length(x),k.clus,replace = F)
  
  for (i in 1:k.clus){
    centroides <- rbind(centroides,data[random[i],1:2])
  }
  
  centroidesAnteriores <- data.frame("X" = rep(0,k.clus),"Y"=rep(0,k.clus))
  
  calculo <- sqrt(apply((centroides-centroidesAnteriores)**2,1,sum))
  
  ggplot(centroides,aes(X,Y))+geom_point(shape=4,color="blue",size=4)
  
  matriz <- matrix(rep(NA,dim(data)[1] * k.clus), nrow=dim(data)[1],ncol = k.clus)
  
  #Bucle
  iter <- 0
  distanciaFinal <- c()
  while(max(calculo) > 0.05){
    for (i in 1:dim(data)[1]){
      for(j in 1:k.clus){
        matriz[i,j] <- distancia(data[i,1:2],centroides[j,])
      }
    }
    cat <- apply(X = matriz,MARGIN= 1,FUN = which.min)
    distanciaFinal[iter+1] <- sum(apply(X = matriz,MARGIN= 1,FUN = min))
    
    data[,"Cluster"] <- factor(cat)
    
    centroidesAnteriores <- centroides
    
    #Nuevos centros
    for (centro in 1:k.clus) {
      centroides[centro,] <- c(mean(data[data$Cluster==centro,"X"]),mean(data[data$Cluster==centro,"Y"]))
    }
    calculo <- sqrt(apply((centroides-centroidesAnteriores)**2,1,sum))
    
    iter <- iter + 1
  }
  
  error <- data.frame("X" = 1:length(distanciaFinal),"Y" = distanciaFinal)
  g1 <-ggplot(error,aes(X,Y))+geom_jitter()
  
  g2 <- ggplot(data,aes(X,Y,color=Cluster))+geom_jitter() + 
    geom_point(data=centroides,aes(X,Y),shape=4,size=4,color="blue")+xlab("Flipper")+ylab("Bill length")+
    ggtitle("Cluster de pinguinos")
  
  
  return(list(data$Cluster,centroides,distanciaFinal[length(distanciaFinal)],g2))
}


#-------------------------------------------------------------------------------------
kMeans3 <- function(a,b,k.clus,n.iter){
  
  #Limpio los NAs
  data <- data.frame("X" = a, "Y" = b, "Cluster" = rep(NA,length(a)))
  data <- data[!is.na(data$X) & !is.na(data$Y),]
  ggplot(data,aes(X,Y))+geom_jitter()
  
  set.seed(27)
  #Creo empty dataframe
  columns = c("X","Y") 
  centroides = data.frame(matrix(nrow = 0, ncol = length(columns))) 
  colnames(centroides) = columns
  
  random <- sample(1:length(x),k.clus,replace = F)
  
  for (i in 1:k.clus){
    centroides <- rbind(centroides,data[random[i],1:2])
  }
  
  ggplot(centroides,aes(X,Y))+geom_point(shape=4,color="blue",size=4)
  
  matriz <- matrix(rep(NA,dim(data)[1] * k.clus), nrow=dim(data)[1],ncol = k.clus)
  
  #Bucle
  iter <- 0
  distanciaFinal <- c()
  while(iter < n.iter){
    for (i in 1:dim(data)[1]){
      for(j in 1:k.clus){
        matriz[i,j] <- distancia(data[i,1:2],centroides[j,])
      }
    }
    cat <- apply(X = matriz,MARGIN= 1,FUN = which.min)
    distanciaFinal[iter+1] <- sum(apply(X = matriz,MARGIN= 1,FUN = min))
    
    data[,"Cluster"] <- factor(cat)
    
    centroidesAnteriores <- centroides
    
    #Nuevos centros
    for (centro in 1:k.clus) {
      indice <- filtrado(data[data$Cluster==centro,][,1:2])
      centroides[centro,] <- data[data$Cluster==centro,][indice,1:2] 
    }
    
    iter <- iter + 1
  }
  
  error <- data.frame("X" = 1:length(distanciaFinal),"Y" = distanciaFinal)
  g1 <-ggplot(error,aes(X,Y))+geom_jitter()
  
  g2 <- ggplot(data,aes(X,Y,color=Cluster))+geom_jitter() + 
    geom_point(data=centroides,aes(X,Y),shape=4,size=4,color="blue")+xlab("Flipper")+ylab("Bill length")+
    ggtitle("Cluster de pinguinos")
  
  
  return(list(data$Cluster,centroides,distanciaFinal[length(distanciaFinal)],g2))
}


#-------------------------------------------------------------------------------------
filtrado <- function(datos){
  vectorX <- datos[,1]
  vectorY <- datos[,2]
  medida <- c()
  iterador <- 1
  for(repe in 1:length(vectorX)){
    for(aux in 1:length(vectorX)){
      if(repe != aux){
        medida[iterador] <- distancia(c(vectorX[repe],vectorY[repe]),c(vectorX[aux],vectorY[aux]))
      }
      else{
        medida[iterador] <- NA
      }
    iterador <- iterador + 1
    }
  }
  matriz <- matrix(medida,nrow=length(vectorX),ncol=length(vectorX),byrow=T)
  return(which.min(apply(matriz,1,mean,na.rm = T)))
    
}



kMeans3(x,y,3,15)
pengu <- palmerpenguins::penguins

kMeans2(pengu$flipper_length_mm,pengu$bill_length_mm,3)


#El criterio es que de todas las distancias entre centroides nuevos y viejos, la maxima de estas va a tener que ser menor a 0.05.

#Para el punto 4 lo que hay que hacer es agarrar como centroide aquel observacion dentro del cluster
#que tiene el menor promedio de distancia entre todos los puntos del cluster.

