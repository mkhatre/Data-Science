# Set Working Directory
setwd("D:\\Others\\XLRI\\Sessions\\Decision Making - SKDe")

#Load the data
lrd<-read.csv("gmat1.csv", header = TRUE)

# convert a numeric data into categorical
lrd$rank<-as.factor(lrd$rank)
summary(lrd)
set.seed(42)

#install.packages("caTools")
library(caTools)

# split data into training and test (70/30)
spl <-sample.split(lrd$admit,SplitRatio = 0.7)
head(spl)

lrdtr<-subset(lrd,spl==TRUE)  # Training data
lrdvl<-subset(lrd,spl==FALSE) # Test Data

# Log Regression Model
mod1<-glm(admit~.,data = lrdtr, family="binomial")

# Prediction
pr<-predict(mod1,lrdvl,type="response")
pr

## converting predicted values to 0 or 1 
cls<- ifelse(pr>0.5,1,0)

# Confusion matrix
cm<-table(lrdvl$admit, cls)
cm
