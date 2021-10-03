###
#GG plot charts
###

#Setup
options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)

# Init Ggplot
psize <- island$`A (km2)` / max(island$`A (km2)`)*8
#long vs lat
g1 <- ggplot(island, aes(x=island$Longitude, y=island$Latitude)) +
  geom_point(size=2*psize) +
  labs(x="Longitude", y="Latitude")

for( i in 1:length(dmisland[1,]))
  for ( j in 1:length(dmisland[1,]))
    if ( i < j )
      g1 + geom_line( aes(c( island$Longitude[i],island$Longitude[j] ) ,
             c( island$Latitude[i],island$Latitude[j] ) ,
             lwd=2 , col=col.alpha("black",Rho[i,j]^2 )))


#area vs pop
ggplot(island, aes(x=log(island$`A (km2)`), y=log(island$richness))) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="Area", y="Richness")


model=mA.3.2
post <- extract.samples(model)
# plot the posterior median covariance function
plot( NULL , xlab="distance (thousand km)" , ylab="covariance" , xlim=c(0,10) , ylim=c(0,2))
# compute posterior mean covariance
x_seq <- seq( from=0 , to=10 , length.out=100 )
pmcov <- sapply( x_seq , function(x) post$etasq*exp(-post$rhosq*x^2) )
pmcov_mu <- apply( pmcov , 2 , mean )
lines( x_seq , pmcov_mu , lwd=2 )
# plot 60 functions sampled from posterior
for ( i in 1:50 )
    curve( post$etasq[i]*exp(-post$rhosq[i]*x^2) , add=TRUE ,
           col=col.alpha("black",0.3) )

title("Azores - 1st Edition")
title("Mc Elreath -2nd")
