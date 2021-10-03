##
#Using only Terceira data for a bayesian S~cA^z relationship
##

source("/home/splinter/Documents/BEGC/R scripts/Task1/myFunctions2.R")
setwd("/home/splinter/Documents/R data/Azores")
library(tidyverse)

##
#Zeta diversity
##

########
#AZORES

azoresFull <- as.data.frame(read_excel("azores_arthopoda.xlsx", sheet=1))
azoresVars <- as.data.frame(read_excel("MAC_EnvData.xlsx", sheet=1, col_names = FALSE))
azores <- azoresVars[9:17,]
azoresNames = azores[,1]
azores <- df2matrix(azores)
azores <- as.data.frame(azores)
azores$V1 <- azoresNames
colnames(azores) <- azoresVars[7,]
azores$index <- 1:9 
#Getting species richness
azoresData<-azoresFull[20:length(azoresFull)]
azores$richness = colSums(azoresData, na.rm = TRUE)/2 #/2 removes total from excel
#Distance matrix
dmAzores = distMatrix(df2matrix(azores[2:3]))
dmAzores=dmAzores/1000  #distancia em km
dmAzores=dmAzores/100  #dist em centenas de Km
colnames(dmAzores) <- colnames(azoresData)
round(dmAzores,3)






#preparing data
library(zetadiv)
terData <- read.csv("balaReserves.csv", sep=",")
reserves = c("Ferraria","Caldeira","Galhardo","Serra","Brava")
reservesSize = c(1:length(reserves))
zTer = matrix(ncol = length(reserves), nrow=length(terData[,1]))
colnames(zTer)=reserves
id = c(1:length(terData[1,]))

for(i in 1:length(reserves)){
  selection = grep(reserves[i],colnames(terData))
  zTer[,i] = rowSums(terData[,selection])
  id[selection] = i
  reservesSize[i] = length(selection)
}

zTerCP = t(zTer)/t(zTer)
zTerCP[is.nan(zTerCP)]=0
zTerCP = data.frame(zTerCP)

zTerPP = t(terData)/t(terData)
zTerPP[is.nan(zTerPP)]=0
zTerPP = data.frame(zTerPP)

#Calculating Zeta Diversity components
zDivCP = c(1:length(reserves))
zDivPP = vector("list",length(reserves))
for(i in 1:length(reserves)){
  zDivCP[i] = Zeta.order.mc(zTerCP,order=i,sam=1000)$zeta.val
  selection = grep(reserves[i],colnames(terData))
  for(j in 1:length(selection)){
        zDivPP[[i]] = c(zDivPP[[i]],Zeta.order.mc(zTerPP[selection,],order=j,sam=1000)$zeta.val)
  }
}

##
#Getting distance matrix
##
library(readxl)
balaVars = as.data.frame(read_excel("SITES_BALA_100_UTMs.xlsx", sheet=1))
coords = balaVars[61:100,6:7]
coords = coords[-c(36),]
dist <- matrix(ncol=length(coords[,1]),nrow=length(coords[,1]))

for(i in 1:length(dist[,1])){
  for(j in 1:length(dist[,1])){
    dist[i,j] <- sqrt((coords[i,1]-coords[j,1])^2+(coords[i,2]-coords[j,2])^2)
  }
}


###
#Model with partial pooling:
#
###
library(rethinking)

#Preparing the data

#Zeta diversity: number of simmilar spp per order 
zDiv=log(zDivPP[[1]])
for(i in 2:length(zDivPP)){
  zDiv=c(zDiv,log(zDivPP[[i]]))
}

#A=log(c(1:(length(zDivPP[[1]]))))
#for(i in 2:length(zDivPP)){
#  A=c(A,log(1:length(zDivPP[[i]])))
#}
#A=(A-mean(A))/sd(A) 

stan_data <- list(area=A,
                  zDiv=zDiv,
                  reserve_id=id)

m1<-map2stan(alist(
    zDiv~dnorm(mu,sigma),
    mu<-a_reserve[reserve_id]+b_reserve[reserve_id]*area,
    c(a_reserve,b_reserve)[reserve_id]~dmvnorm2(c(a,b),sigma_reserve,Rho),
    a~dnorm(0,1),
    b~dnorm(0,1),
    sigma_reserve~dcauchy(0,1),
    Rho ~ dlkjcorr(2),
    sigma~dcauchy(0,1))
    ,data=stan_data,warmup=1000,iter=5000,chains=3,cores=3
)


##
#Visualizing the data:
##
layout(matrix(1:6,ncol=3,byrow=T))
counter=1
for(i in 1:length(reserves)){
  plot(zDiv[counter:(counter+reservesSize[i]-1)] ~ A[counter:(counter+reservesSize[i]-1)],
       pch = 20, ylim=c(min(zDiv)*0.90,max(zDiv)*1.1), ylab="Log zDiv",
       xlab="Log Area (no units)", main=paste("Reserve",i))
  for (j in 1:250) {
    abline(extract(fitPP)$logC[j,i], extract(fitPP)$z[j,i],  col = "gray", lty = 1)
  }
  abline(mean(extract(fitPP)$logC[,i]),mean(extract(fitPP)$z[,i]),  col = 6, lw = 2)
  counter = counter+reservesSize[i]
  #points(zDiv,A,pch=20)
}

for(i in 1:length(reserves)){
  plot(S[counter:(counter+reservesSize[i]-1)] ~ A[counter:(counter+reservesSize[i]-1)],
       pch = 20, ylim=c(min(S)*0.90,max(S)*1.1), ylab="Log S (richness)",
       xlab="Log Area (no units)", main=paste("Reserve",i))
  counter = counter+reservesSize[i]
}