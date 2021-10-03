#####
#SOM#
#####

setwd("C:/Users/Diogo Barros/Documents/Diogo/R data")

data <- read.csv("sound_2way.csv")
data2 <- read.csv("sound.csv")
data3 <- read.csv("sound_habitats.csv")


attach(data)
detach(data)
names(data)


######################
#Exploratory analyses#
######################


shapiro.test(MargemB12)  #NOT SIGNIFICANT
shapiro.test(MargemB19)  #NOT SIGNIFICANT 
shapiro.test(MargemC12)  #NOT SIGNIFICANT
shapiro.test(MargemC19)  #NOT SIGNIFICANT

t.test(MargemB12,MargemB19, paires=TRUE)   #NOT SIGNIFICANT
t.test(MargemC12,MargemC19, paires=TRUE)   #NOT SIGNIFICANT
t.test(MargemB12,MargemC12, paires=TRUE)   #NOT SIGNIFICANT
t.test(MargemB19,MargemC19, paires=TRUE)   #NOT SIGNIFICANT

wilcox.test(MargemB12,MargemB19, paires=TRUE)   #NOT SIGNIFICANT
wilcox.test(MargemC12,MargemC19, paires=TRUE)   #NOT SIGNIFICANT
wilcox.test(MargemB12,MargemC12, paires=TRUE)   #NOT SIGNIFICANT
wilcox.test(MargemB19,MargemC19, paires=TRUE)   #NOT SIGNIFICANT

#Plotting

boxplot(MargemB12,MargemB19,MargemC12, MargemC19,
        las =2,
        col = c("red", "red", "blue", "blue"),
        names = c("12:00","19:00","12:00", "19:00")
        )
  mtext("Sound (dB)", side = 2, line = 3)
  mtext("Floor level                            Breast high",
        side = 1, line = 4)
  mtext("Sound levels in Green Areas", side = 3, line = 1)
  




###############
#SUMMARY STATS#
###############

#Prepare summary estatistics

library(Rmisc)

sum1 = summarySE(data,
                measurevar="ampSound",
                groupvars=c("Time","Place"))

sum2 = summarySE(data,
                 measurevar="dbSound",
                 groupvars=c("Time","Place"))

#Create a plot with error bars, including 

library(ggplot2)

pd = position_dodge(.2)

ggplot(sum1, aes(x=Time, y=ampSound, color=Place)) +
  geom_errorbar(aes(ymin=ampSound-se, 
                    ymax=ampSound+se), 
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold")) +
  scale_color_manual(values = c("black", "blue"))

ggplot(sum2, aes(x=Time, y=dbSound, color=Place)) +
  geom_errorbar(aes(ymin=dbSound-se, 
                    ymax=dbSound+se), 
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
model1 = lm(ampSound ~ Time + Place + Time:Place,
           data=data)
Anova(model1, type="II") 
summary(model1)


model2 = lm(dbSound ~ Time + Place + Time:Place,
            data=data)
Anova(model2, type="II") 
summary(model2)


#POST-HOC Pairwise comparisons (avova 2-way)

TukeyHSD(model1, "")

# Centroid 3 way anova: including habitats

data3 <- read.csv("sound_habitats.csv")

attach(data3)

library(car)
model1 = lm(dbSound ~ Time + Place + Habitat +
              Time:Place + Time:Habitat + Place:Habitat,
            data=data)
Anova(model1, type="II") 
summary(model1)


#Plotting habitats


sum3 = summarySE(data3,
                 measurevar="dbSound",
                 groupvars=c("Habitat", "Time", "Place"))

library(ggplot2)
library(grid)

boxplot(data3$dbSound~Habitat,
        las =2,
        #col = c("red", "red", "blue", "blue"),
#        names = c("12:00","19:00","12:00", "19:00")
)
mtext("Sound (dB)", side = 2, line = 3)
mtext("Habitats",  side = 1, line = 4)
mtext("Sound levels in Green Areas", side = 3, line = 1)

#Quick t test margins vs centorids

#data4 <- read.csv("sound_marginsVsCentr.csv")
data4 <- read.csv("landscape.csv")

attach(data4)
t.test(soundMarg,soundCentr,  pairs=TRUE)


detach(data4)


boxplot(soundCentr,soundMarg,
        las =1,
        names = c("Center","Margin")
)
mtext("Sound (dB)", side = 2, line = 3)
mtext("Sound levels: centroid vs margin (average)", side = 3, line = 1)

                )


library(ggplot2)

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = dbSound + data4$se,
              ymin = dbSound - data4$se)

p <- ggplot(data = data4, aes(x = Place, y = dbSound, fill = Nomes))

p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank()
        )
