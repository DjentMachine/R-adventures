###

head(rita)

stan_data <- list(n = nrow(rita),
                  x1= scale(rita$altitude),
                  x2= scale(rita$dist.rio),
                  y= rita$P.A.)
setwd("C:/Users/Diogo/Documents/Diogo/R data/helpingOthers")

fit <- stan("rita.stan", data=stan_data,iter=4000,chain=4,
            cores=4,
            control=list(max_treedepth =10,adapt_delta=0.99))
  
  






####
library(rethinking)

dataset <- read.csv("data_rita.csv")

model4<-map2stan(
  alist(
    poiso~ dbinom(1,p),
    logit(p)<-a+b_dist*driv,
    a~dnorm(0,10),
    b_dist~dnorm(0,1)
    #b_alt~dnorm(-1,1)
  ), data=dataset, warmup=1000,iter=5000,chains=3,cores=5
  
)

plot(model)
precis(model)
plot(precis(model))

stancode