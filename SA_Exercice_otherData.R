###
# Spatial autocorrelation exercice: appliance to McElreath data and BALA data
# https://clubedaciencia.com.br/how-to-identify-and-remove-spatial/
#
# @author Diogo Barros
####

##Load necessary packages
library(readxl)
library(nlme)
library(ape)
library(MuMIn)
library(rethinking)


#Preparing data

#McElreath data 
data(islandsDistMatrix)
Dmat <- islandsDistMatrix
colnames(Dmat) <- c("Ml","Ti","SC","Ya","Fi","Tr","Ch","Mn","To","Ha")
round(Dmat,1)

data(Kline2) # load the ordinary data, now with coordinates
d <- Kline2
d$society <- 1:10 # index observations


#BALA data
setwd("C:/Users/DBarros/Documents/PhD/Package_Diogo/Data")
da <- as.data.frame(read_excel("azores_data.xlsx"))
dc <- as.data.frame(read_excel("canary_data.xlsx"))

#Choosing variables
x = dc$`A (km2)`
y = dc$richness
lon = dc$Longitude
lat = dc$Latitude

#My model
#Model for McElreath might need adjustment. From McElreath, page 360, Tools = (alpha*Population^Beta)/Gamma
#Thats log(Tools) = log(alpha) + Beta * log(Population) - log(Gamma)


model <- gls( log(y) ~ log(x))   
plot(log(y) ~ log(x))
abline(model)
plot(residuals(model, type="normalized"))

#Semivariogram
semivario <- Variogram(model, form = ~lon + lat, resType = "normalized")
plot(semivario, smooth = TRUE, main = "Semivariogram pre treatment - McElreath data")

#Morran's I
geo<-cbind(lon, lat)
samples.dist <- as.matrix( dist(geo) ) #Euclidean distances
samples.dist.inv <- 1/samples.dist #Proximity matrix
diag(samples.dist.inv) <- 0
Moran.I( log(x) , samples.dist.inv ,alternative="greater")

##
#Removing spatialautocorrelation effects from data
##

#Different SA structures: exponential, gaussian, spherical, linear and rational 
exponential.autocor <- gls( log(x) ~ log(y) , correlation = corExp(form = ~lon + lat, nugget=T))
gaussian.autocor <- gls( log(x + 1) ~ log(y + 1) , correlation = corGaus(form = ~lon + lat, nugget=T))
spherical.autocor <- gls( log(x + 1) ~ log(y + 1) , correlation = corSpher(form = ~lon + lat, nugget=T))
linear.autocor <- gls( log(x + 1) ~ log(y + 1) , correlation = corLin(form = ~lon + lat, nugget=T))
ratio.autocor <- gls( log(x + 1) ~ log(y + 1) , correlation = corRatio(form = ~lon + lat, nugget=T))

#Comparing all models with different SA structures including "model", the one without SA consideration
model.sel(model, gaussian.autocor, spherical.autocor, linear.autocor, ratio.autocor, exponential.autocor)

summary(model)
summary(gaussian.autocor)

# Tukey-Anscombe Plot (Residual vs. Fitted) for the best model
plot(fitted(gaussian.autocor), residuals(gaussian.autocor))
abline(h=0,lty=3)

#QQ plot
qqnorm(residuals(gaussian.autocor))
qqline(residuals(gaussian.autocor))

#Cheking if SA effects were removed
semivario <- Variogram( gaussian.autocor, form = ~lon + lat, resType = "normalized")
plot(semivario, smooth = TRUE, main = "Semivariogram post treatment - Bird-Tree data")

