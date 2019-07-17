#-------------------------Linear Regression Intro

library(foreign)
setwd("C:/Users/Sam Cannon/Desktop/R/Data Sets/new_datasets_7e/new_datasets_7e")
#import the data
data <- read.spss("Lesson 33 Data File 1.sav", to.data.frame = T)

summary(data)
library(psych)

#scale our features for predictions, columns 1-5 are features, column 6 is target
scaled_data <- scale(data[, 1:5])

scaled_data <- as.data.frame(scaled_data)

#add target column to scaled data
scaled_data$INJURY <- data$INJURY

#create total composite across scaled columns
scaled_data$TOTAL_STRENGTH <- (scaled_data$QUADS + scaled_data$GLUTS + scaled_data$ABDOMS + scaled_data$ARMS + 
                              scaled_data$GRIP)/5

#run regression
model1 <- lm(data$INJURY~scaled_data$TOTAL_STRENGTH)
summary(model1)
confint(model1)
#model1 <- lm(data$INJURY~scaled_data$TOTAL_STRENGTH, na.action = na.exclude) this excludes NAs if they there

#get the standardized beta slope
# library(QuantPsyc)
lm.beta(model1)

## This information is telling us that if you get 1 increase in SD on Strength you get -.325 decrease in Injury

#------------------plots

plot(model1)

#homoscedacity, residuals are fairly evenly distributed around your regression line

plot(scaled_data$TOTAL_STRENGTH, scaled_data$INJURY)
#fit a regression line over the scatterplot of residuals
abline(lm(scaled_data$INJURY~scaled_data$TOTAL_STRENGTH), col = 'red')

abline(lm(data1$PEER~data1$BOBO), col = "red")




