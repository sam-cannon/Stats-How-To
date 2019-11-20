library (compute.es)
library (ggplot2)
library (multcomp)
library(nlme)
library (pastecs)
library (reshape)



#set working directory
setwd("C:/Users/Sam Cannon/Desktop/R/Data Sets/new_datasets_7e")

#upload data
df <- read.csv("L29Ex1_TeacherStress.csv", header=TRUE)

#re-shape the Data in R to get a Long Version
df <-melt(df, id = "ID", measured = c("student", "parent", "admin"))

#name new columns
names(df)<-c("ID", "Role", "Stress")

# name factors in new columns
df$role<-factor(df$Role, labels = c("Student", "Parent", "Administrator"))


df<-df[order(df$ID),]

StressBar <- ggplot(df, aes(Role, Stress))
StressBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + 
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Role of Constituent", y = "Stress Level") 

#look at boxplot
StressBoxplot <- ggplot(df, aes(Role, Stress))
StressBoxplot + geom_boxplot() + labs(x = "Role of Constituent", y = "Stress Level")

#descriptive stats
by(df$Stress, df$Role, function(X) round(stat.desc (X, norm=TRUE, basic = TRUE),3))

#set a priori contrasts
STUDENTvsELSE<-c(-2, 1, 1)
PRTvsADMIN<-c(0, -1, 1)
contrasts(df$Role)<-cbind(STUDENTvsELSE, PRTvsADMIN)

#omnibus ANOVA
tStress_Model<-lme(Stress ~ Role, random = ~1|ID/Role, data = df, method = "ML")
baseline<-lme(Stress ~ 1, random = ~1|ID/Role, data = df, method = "ML")
anova(baseline, tStress_Model)

#summary of model
summary(tStress_Model)

#summary of baseline
summary(baseline)


#effect sizes
rcontrast<-function(t, df)
{r<-sqrt(t^2/(t^2 + df))
print(paste("r = ", r))
}

#rcontrast(t-value, df)
rcontrast(2.289783, 28)
## [1] "r =  0.397139905651822"
rcontrast(2.610116, 28)
## [1] "r =  0.442375351363714"































