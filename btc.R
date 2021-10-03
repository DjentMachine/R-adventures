###
# Predicting BTC price
# Author: Diogo B
# Verison 1.0
####

library(tidyverse)
library(timeSeries)
library(forecast)

setwd("C:/Users/DBarros/Documents/Pessoal/DataScience/crypto")
data <- read.csv("coinmarketcapBTC2.csv",dec = ".",header = TRUE,stringsAsFactors = FALSE)
head(data)

nData <- data %>% map_df(rev)




#Exploratory analyses
time = (1:length(data[,1]))
plot(nData$Open ~ time, pch=16,ylab = " BTC Open price", main="BTC price over time (days), 2013-2020")

time = (1:length(data[,1]))
plot(log(nData$Open) ~ time, pch=16,ylab = " LOG BTC Open price", main="BTC price (log) over time (days), 2013-2020")

abline(lm(log(nData$Open) ~ time), col="red")

#Basic linear regression
#2849 -> 14/02/2021
m <- lm(log(nData$Open) ~ time)
summary(m)

day0 <- 2849
prediction <- 365
exp(m$coefficients[1] + m$coefficients[2]*(day0+prediction))


#Time series package
m2=ts(data = nData$Open,start = 1,end = 2849)
plot(m2)

m3=Arima(m2)
pred = HoltWinters(m2)



