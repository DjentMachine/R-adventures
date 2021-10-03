###
#Pooling models:
#Using Terceira data only
###
source("C:/Users/Diogo/Documents/Diogo/The future/BEGC/R scripts/Task1/myFunctions2.R")
setwd("C:/Users/Diogo/Documents/Diogo/R data")
library(vegan)
library(rethinking)

terData <- read.csv("balaReserves.csv", sep=",")
terMatrix = df2matrix(terData[4:length(terData[,1]),])
reserves = c("Ferraria","Caldeira","Galhardo","Serra","Brava")
reservesSize = c(1:5)
terACurves = vector("list",length(reserves))
id = c(1:length(terData[1,]))

##
#preparing data
##

for(i in 1:length(reserves)){
  selection = grep(reserves[i],colnames(terData))
  reservesSize[i] = length(selection)
  id[selection] = i
  iData = t(terData[selection])
  terACurves[[i]]=specaccum(iData, method="random")
}


###
#Complete Pooling
###
S = log(specaccum(t(terMatrix),method="random")$richness)
A = log(c(1:length(terMatrix[1,])))
A = (A-mean(A))/sd(A)
modelCP=stan_model("regressionCP.stan")

fit <- bayesianRegression(S,A,modelCP)
posterior = getPost(fit)   
modelGraphs(fit,S,A)
saveRDS(fit,"terCPModel.rds")

###
#No Pooling
###
terNPModels = vector("list",length(terACurves))
modelNP=stan_model("regressionCP.stan")

for(i in 1:length(terACurves)){
  S =log(terACurves[[i]]$richness)
  A =log(terACurves[[i]]$sites)
  #A =(A-mean(A))/sd(A) 
  terNPModels[i] <- bayesianRegression(S,A,modelCP)
}

posterior = getPost(fit)   
modelGraphs(fit,S,A)
#readRDS("NPModel.rds")

###
#Partial Pooling
###

S=log(terACurves[[1]]$richness)
for(i in 2:length(terACurves)){
  S=c(S,log(terACurves[[i]]$richness))
}
A=log(terACurves[[1]]$sites)
for(i in 2:length(terACurves)){
  A=c(A,log(terACurves[[i]]$sites))
}
A=(A-mean(A))/sd(A) 

stan_data <- list(n=length(S),
                  A=A,
                  S=S,
                  id=id,
                  n_id=length(reserves))

fit <- stan("regressionPP2.stan", data=stan_data,iter=4000,chain=4,
                cores=4,
                control=list(max_treedepth =10,adapt_delta=0.99))

posterior= getPost(fit)

layout(matrix(1:6,ncol=3,byrow=T))
counter=1
for(i in 1:length(reserves)){
  plot(S[counter:(counter+reservesSize[i]-1)] ~ A[counter:(counter+reservesSize[i]-1)],
       pch = 20, ylim=c(min(S)*0.90,max(S)*1.1), ylab="Log S (richness)",
       xlab="Log Area (no units)", main=paste("Reserve",i))
  for (j in 1:1000) {
    abline(posterior$logC[j,i], extract(fit)$z[j,i],  col = "gray", lty = 1)
  }
  abline(mean(posterior$logC[,i]),mean(posterior$z[,i]),  col = 6, lw = 2)
  counter = counter+reservesSize[i]
}

for(i in 1:length(reserves)){
  plot(S[counter:(counter+reservesSize[i]-1)] ~ A[counter:(counter+reservesSize[i]-1)],
       pch = 20, ylim=c(min(S)*0.90,max(S)*1.1), ylab="Log S (richness)",
       xlab="Log Area (no units)", main=paste("Reserve",i))
  counter = counter+reservesSize[i]
}

##
# Partial Pooling using map2stan
##

fit<-map2stan(alist(
  S~dnorm(mu,sigma),
  mu<-logC[id]+z[id]*A,
  c(logC,z)[id]~dmvnorm2(c(a,b),sigma_basin,Rho),
  a~dnorm(0,1),
  b~dnorm(0,1),
  sigma_basin~dcauchy(0,1),
  Rho ~ dlkjcorr(2),
  sigma~dcauchy(0,1)
),data=stan_data,iter=5000,chains=1,cores=3
)

stan_data <- list(dist=A,
                  sorenson=S,
                  basin_id=id)

m3<-map2stan(alist(
  sorenson~dnorm(mu,sigma),
  mu<-a_basin[basin_id]+b_basin[basin_id]*dist,
  c(a_basin,b_basin)[basin_id]~dmvnorm2(c(a,b),sigma_basin,Rho),
  a~dnorm(0,1),
  b~dnorm(0,1),
  sigma_basin~dcauchy(0,1),
  Rho ~ dlkjcorr(2),
  sigma~dcauchy(0,1)
),data=stan_data,warmup=1000,iter=5000,chains=3,cores=3
)
