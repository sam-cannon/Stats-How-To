
library(foreign)
setwd("C:/Users/Sam Cannon/Desktop/R/Data Sets/new_datasets_7e/new_datasets_7e")
#import the data
data <- read.spss("Lesson 25 Data File 1.sav", to.data.frame = T)

#check data types
str(data)

#change the numerics to factor variables
data$GROUP <- factor(data$GROUP)

#name the levels of the groups
levels(data$GROUP) <- c("Placebo", "Low Dose", "High Dose")
str(data)

#descriptives
library(psych)
describe(data)

library(pastecs)
by(data$DIFF, data$GROUP, stat.desc)


#levenes test, we can assume homogeneity of variance
library(car)
leveneTest(data$DIFF, data$GROUP)

#anova
model <- aov(DIFF ~ GROUP, data = data)
summary(model)

#posthoc tests, t tests between groups, need to control for number of tests for p value

#bonferroni is ok but really conservative
pairwise.t.test(data$DIFF, data$GROUP, paired = F, p.adjust.method = "bonferroni")

#Tukey - assumes equal variances
install.packages("multcomp")
library(multcomp)

summary(glht(model, linfct = mcp(GROUP = "Tukey")))

#Welch's anova, if unequal variances
oneway.test(data$DIFF ~ data$GROUP)

#Dunnet 
summary(glht(model, linfct = mcp(GROUP = "Dunnet"), base = 1))

#boxplot
library(ggplot2)
ggplot(data, aes(x = GROUP, y = DIFF)) + geom_boxplot() + labs(x = "Group", y = "DIFF")
















