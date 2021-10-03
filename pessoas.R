#########
#PESSOAS#
#########

setwd("C:/Users/Diogo Barros/Documents/Diogo/R data")


data <- read.csv("people.csv")
data2 <- read.csv("people_anova.csv")

attach(data)
detach(data)



######################
#Exploratory analyses#
######################


shapiro.test(I.12.a)  #SIGNIFICANT
shapiro.test(I.12.p)  #SIGNIFICANT (TO O.1) 
shapiro.test(I.12.v)  #SIGNIFICANT

shapiro.test(I.18.a)  #NOT SIGNIFICANT
shapiro.test(I.18.p)  #SIGNIFICANT (TO O.1)
shapiro.test(I.18.v)  #SIGNIFICANT

shapiro.test(V.12.a)  #SIGNIFICANT
shapiro.test(V.12.p)  #SIGNIFICANT 
shapiro.test(V.12.v)  #SIGNIFICANT

shapiro.test(V.18.a)  #SIGNIFICANT
shapiro.test(V.18.p)  #SIGNIFICANT
shapiro.test(V.18.v)  #SIGNIFICANT


#people
t.test(I.12.p, I.18.p, paires=TRUE)   #NOT SIGNIFICANT
t.test(V.12.p, V.18.p, paires=TRUE)   #NOT SIGNIFICANT
t.test(I.12.p,V.12.p, paires=TRUE)   #NOT SIGNIFICANT
t.test(I.18.p,V.18.p, paires=TRUE)   #NOT SIGNIFICANT

wilcox.test(I.12.p, I.18.p, paires=TRUE)   #NOT SIGNIFICANT
wilcox.test(V.12.p, V.18.p, paires=TRUE)   #NOT SIGNIFICANT
wilcox.test(I.12.p,V.12.p, paires=TRUE)   #NOT SIGNIFICANT
wilcox.test(I.18.p,V.18.p, paires=TRUE)   #NOT SIGNIFICANT

#ANIMALS
t.test(I.12.p, I.18.a, paires=TRUE)   # SIGNIFICANT
t.test(V.12.p, V.18.a, paires=TRUE)   # SIGNIFICANT
t.test(I.12.p,V.12.a, paires=TRUE)   # SIGNIFICANT
t.test(I.18.p,V.18.a, paires=TRUE)   # SIGNIFICANT

wilcox.test(I.12.a, I.18.a, paires=TRUE)   #NOT SIGNIFICANT
wilcox.test(V.12.a, V.18.a, paires=TRUE)   #NOT SIGNIFICANT
wilcox.test(I.12.a,V.12.a, paires=TRUE)   #NOT SIGNIFICANT
wilcox.test(I.18.a,V.18.a, paires=TRUE)   #NOT SIGNIFICANT

#vehicles
t.test(I.12.p, I.18.v, paires=TRUE)   # SIGNIFICANT
t.test(V.12.p, V.18.v, paires=TRUE)   # SIGNIFICANT
t.test(I.12.p,V.12.v, paires=TRUE)   # SIGNIFICANT
t.test(I.18.p,V.18.v, paires=TRUE)   # SIGNIFICANT

wilcox.test(I.12.v, I.18.v, paires=TRUE)   #NOT SIGNIFICANT
wilcox.test(V.12.v, V.18.v, paires=TRUE)   #NOT SIGNIFICANT
wilcox.test(I.12.v,V.12.v, paires=TRUE)   #NOT SIGNIFICANT
wilcox.test(I.18.v,V.18.v, paires=TRUE)   #NOT SIGNIFICANT

#Plotting

#Boxplot people
boxplot(I.12.p,V.12.p,I.18.p,V.18.p,
        las =1,
        at = c(1,2, 4,5),
        col = c("blue", "blue","red", "red"),
        names = c("12:00","19:00","12:00", "19:00")
)
mtext("Nr of people", side = 2, line = 3)
mtext("Winter                                         Summer",
      side = 1, line = 3)
mtext("Nr of people in Green Areas", side = 3, line = 1)


#Boxplot animals and vehicles

boxplot(I.12.a,V.12.a,I.18.a,V.18.a,
        I.12.v,V.12.v,I.18.v,V.18.v,
        las =2,
        at = c(1,2,3,4, 6,7,8,9),
        col = c("blue", "blue","red", "red"),
        names = c("12:00","19:00","12:00",
                  "19:00","12:00","19:00","12:00", "19:00")
)

mtext("Nr of people", side = 2, line = 3)
mtext("Winter                                         Summer",
      side = 1, line = 4)
mtext("Nr of animals/vehicles in Green Areas", side = 3, line = 1)



##########
#Plotting better
##########


library(Rmisc)

people <- data2[which(data_type=="People"),]
animals <- data2[which(data_type=="Animals"),]
vehicles <- data2[which(data_type=="Vehicles"),]

sum1 = summarySE(people,
                 measurevar="score",
                 groupvars=c("season","time"))

sum2 = summarySE(animals,
                 measurevar="score",
                 groupvars=c("season","time"))

sum3 = summarySE(vehicles,
                 measurevar="score",
                 groupvars=c("season","time"))


#Create a plot with error bars, including 

library(ggplot2)

pd = position_dodge(.2)

ggplot(sum1, aes(x=time, y=score, color=season)) +
  geom_errorbar(aes(ymin=score-se, 
                    ymax=score+se), 
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold")) +
  scale_color_manual(values = c("black", "blue"))

ggplot(sum2, aes(x=time, y=score, color=season)) +
  geom_errorbar(aes(ymin=score-se, 
                    ymax=score+se), 
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold")) +
  scale_color_manual(values = c("black", "blue"))

ggplot(sum3, aes(x=time, y=score, color=season)) +
  geom_errorbar(aes(ymin=score-se, 
                    ymax=score+se), 
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold")) +
  scale_color_manual(values = c("black", "blue"))


#Fitting the linear model and conduct ANOVA (2way)       

library(car)
model1 = lm(score ~ season + time + data_type +
            time:season + time:data_type + season:data_type,
            data=data2)
Anova(model1, type="II") 
summary(model1)


