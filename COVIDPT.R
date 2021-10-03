##COVID19
covidPT=c(1,2,4,6,9,13,21,30,39,41,59,78,112,169,245,331,448,)
days=seq(1,19)
m=lm(log(covidPT)~log(days))
m2=lm(log(days)~log(covidPT))
plot(covidPT,pch=16, main ="Covid19 growth in Portugal",
     xlab="Number of days passed since March 2020")
#lines(days, days^m$coefficients[2],col="red")
m=lm(log(covidPT)~log(days))
summary(m)
y=-0.8866+(2.1853*log(days))
y2=0.5023+(0.4294*log(covidPT))
