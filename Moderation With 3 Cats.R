df <- read.csv("ocb.csv")

library(car)
library(psych)
library(ggplot2)
options(scipen=999)

#create labels for department
df$depart <- factor(df$depart,levels = c(1,2,3),labels = c("Finance", "Marketing", "Sales"))


#descriptives by department
describeBy(df$ocb,df$depart)

#create scatterplot for relationship between cse and ocb for each departmnent
scatter<-ggplot(df, aes(cse, ocb,  color = depart))
scatter + geom_point() + geom_smooth(method="lm") + labs(x="cse", y="ocb")

#center cse and add it to the df
csecen <- df$cse - (mean(df$cse))
mean(csecen)  #Check that this is zero, accounting for rounding error
df2 <- data.frame(df, csecen)


#analyze simple slopes
library(devtools)
library(reghelper)

moderation<-(lm(ocb ~ csecen + depart + csecen*depart, data=df2)) 
#using centered data here
simple_slopes(moderation)

#run the MMR
summary(moderation)
anova(moderation)


#test for type 2 heteroscedasticity
library(lmtest)
bptest(moderation)  #Note the p value is .2 Type II heteroscedasticity is likely not happening


