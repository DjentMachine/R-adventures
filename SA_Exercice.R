###
# Spatial autocorrelation exercice
# https://clubedaciencia.com.br/how-to-identify-and-remove-spatial/
#
# @author Diogo Barros
####

#Preparing data
setwd("C:/Users/DBarros/Documents/PhD/Dados/Paper1/SA_Exercice")
bird.diversity<-read.table("bird.diversity.txt", head=T)

##Load necessary packages
library(nlme)
library(ape)
library(MuMIn)
#My model
model <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , data = bird.diversity )
plot(log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , data = bird.diversity )
abline(model)

#Semivariogram
semivario <- Variogram(model, form = ~Lon_x + Lat_y, resType = "normalized")

semivario2 <- plot.my.semivar(smres = ,br=seq(0,2,.1))
plot(semivario, smooth = TRUE, main = "Semivariogram pre treatment - Bird-Tree data")

#Morran's I
geo<-cbind(bird.diversity$Lon_x, bird.diversity$Lat_y)
samples.dist <- as.matrix( dist(geo) ) #Euclidean distances
samples.dist.inv <- 1/samples.dist #Proximity matrix
diag(samples.dist.inv) <- 0
Moran.I( log(bird.diversity$Bird_diversity+1) , samples.dist.inv ,alternative="greater")

##
#Removing spatialautocorrelation effects from data
##

#Different SA structures: exponential, gaussian, spherical, linear and rational 
exponential.autocor <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , correlation = corExp(form = ~Lon_x + Lat_y, nugget=T), data = bird.diversity )
gaussian.autocor <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , correlation = corGaus(form = ~Lon_x + Lat_y, nugget=T), data = bird.diversity )
spherical.autocor <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , correlation = corSpher(form = ~Lon_x + Lat_y, nugget=T), data = bird.diversity )
linear.autocor <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , correlation = corLin(form = ~Lon_x + Lat_y, nugget=T), data = bird.diversity )
ratio.autocor <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , correlation = corRatio(form = ~Lon_x + Lat_y, nugget=T), data = bird.diversity )

#Comparing all models with different SA structures including "model", the one without SA consideration
model.sel(model, gaussian.autocor, spherical.autocor, linear.autocor, ratio.autocor)

summary(model)
summary(gaussian.autocor)

# Tukey-Anscombe Plot (Residual vs. Fitted) for the best model
plot(fitted(gaussian.autocor), residuals(gaussian.autocor))
abline(h=0,lty=3)

#QQ plot
qqnorm(residuals(gaussian.autocor))
qqline(residuals(gaussian.autocor))

#Cheking if SA effects were removed
semivario <- Variogram( gaussian.autocor, form = ~Lon_x + Lat_y, resType = "normalized")
plot(semivario, smooth = TRUE, main = "Semivariogram post treatment - Bird-Tree data")




