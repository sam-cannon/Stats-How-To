# ________________________________T Tests
library(foreign)
setwd("C:/Users/Sam Cannon/Desktop/R/Data Sets/new_datasets_7e/new_datasets_7e")
#import the data
data <- read.spss("Lesson 24 Data File 1.sav", to.data.frame = T)

#recode 
#recoding variables
library(car)
data$STRESS <- recode(data$STRESS, "'Low Stress'=1; 'High Stress'=2")

#convert stress to factor with 2 levels
data$STRESS <- as.factor(data$STRESS)

#check to see if it worked (we didn't have to do this BTW, just cuz)
str(data)

#dropping columns
keeps <- c("STRESS", "TALK")
data <- data[keeps]


#Levene's Test
#--------we dont want a p less than .05, if p is less than .05 then we violated the assumption of equal
# variance, meaning that the groups means to start with were different
library(car)
leveneTest(data$TALK, data$STRESS)

#descriptives
#install.packages("pastecs")
library(pastecs)

by(data$TALK, data$STRESS, stat.desc, basic = FALSE, norm = T)


#run independent samples t test
ind.ttest <- t.test(TALK ~ STRESS, data = data, paired = F)
ind.ttest

#graph t test
library(ggplot2)
boxplot <- ggplot(data = data, aes(x = STRESS, y = TALK)) + geom_boxplot() + labs(x = "Stress", y = "talk")
boxplot
#stress needs to be a factor for this plot? thats what he said in the video but that shouldnt matter...

#__________________________________paired samples T Test aka Dependent T Test
#import the data
data2 <- read.spss("Lesson 23 Data File 1.sav", to.data.frame = T)

#descriptives
stat.desc(data2, basic = F, norm = T)

#paired t test (paired = T)
paired_test <- t.test(data2$PAY, data2$SECURITY, paired = T)
paired_test










