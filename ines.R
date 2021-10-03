##
# Ajudando a Inês
##


##
#Loading data
##
setwd("C:/Users/Diogo/Documents/Diogo/R data")  #PÕE A TUA PASTA DE TARABLHO AQUI
data <- read.csv("ines5.csv", sep=",")          #Lê o ficheiro excel
attach(data)
data

##
#Subsetting data - Pula esta parte
##

#myMatrix = data[6,]
#for (i in 1:length(levels(data$Treatment))){
#  rbind(myMatrix,data[18,])
#  print(i)
#}


##
#PCA analyses
##
#pcaData <- names(data) %in% c("NDVI","PRI")

#Full data, no average
pcaData <-as.matrix(data[2:(length(data))])

#Averaged data
pcaData <-as.matrix(data[2:(length(data)-2)])
myPca <- prcomp(pcaData,center = TRUE,scale. = TRUE)
plot(myPca, type ="lines")
summary(myPca)
print(myPca)
biplot(myPca, xlab= "PC1\nVariation exlpained: ",
       ylab="PC2\nVariation exlpained: ")


##
#GLMs
##
library(lme4)
fit1<-glm( data$Biomassa ~ data$NDVI)
fit2<-glm( data$Biomassa ~ data$NDVI + data$PRI)
fit3<-glm( data$Biomassa ~ data$NDVI + data$PRI + data$Lic1+ data$SIPI-data$Ctr2)

##
#Correlações
##
a<-cor.test(data$Biomassa,data$NDVI)
print(a)
summary(a)

##
#TWO WAY ANOVA
##

#Pressupostos

library(car)  #For Levene's test and ANOVA
normality = cbind(rep(0,length(data)-1))
heteroscedasticity = cbind(rep(0,length(data)-1))
assumptions=cbind(normality,heteroscedasticity) #assumption matrix. col1=normality, col2=variance homogenity
normSig=c()
varSig=c()
for (i in 1:length(normality)){
  assumptions[i,1]=shapiro.test(data[,i+1])$p.value
  assumptions[i,2]=leveneTest(data[,i+1],data[,1])$`Pr(>F)`[1]
  if(assumptions[i,1]<0.05){
    normSig=cbind(normSig,i)
  }
  if(assumptions[i,2]<0.05){
    varSig= cbind(varSig,i)    
  }
}
normSig
varSig

#2way anova

#Preparing DA data
Treatments = c()#as.vector(data[,1])
Indexes = c()
Score = c()

for (i in 2:(length(names(data))-2)) {
   Treatments = append(Treatments, as.vector(data[,1]))
   Indexes = append(Indexes, c(rep(names(data)[i],length(data[,1]))))
   Score = append(Score, data[,i])
}

m=cbind(Score,Treatments)
m=cbind(m,Indexes)
m=as.data.frame(m)


#ANOVA
myANOVA = aov(Score~Treatments+Indexes)
summary(myANOVA)

