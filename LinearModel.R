library(car)  # for vif function

setwd("D:\\Others\\XLRI\\Sessions\\Decision Making - SKDe")
adv1 <- read.csv("adv1.csv", header=TRUE)
adv1
head(adv1)
### Linear Regression
mod<-lm(Sales~.,data=adv1)
mod
summary(mod)
# Plot model residuals
plot(mod$residuals, pch = 16, col = "red")

#####################

adv1 <- read.csv("adv-1.csv", header=TRUE)
adv1
head(adv1)
mod<-lm(Sales~. -City,data=adv1)
mod
summary(mod)
plot(mod$residuals, pch = 16, col = "red")

######################
cl15 <- read.csv("cls15.csv", header=TRUE)
cl15
str(cl15)
head(cl15)
mod<-lm(Salary~.,data=cl15)
mod
summary(mod)

#######################
cl15e <- read.csv("cls15-1.csv", header=TRUE)
cl15e
str(cl15e)
head(cl15e)
## Linear model
mod<-lm(Salary~.,data=cl15e)
summary(mod)

## Variance Inflation Factor to find multicolinearity in the model
vif(mod) # Age and Exp has high vif value
mod1<-lm(Salary~.-Age,data=cl15e)  # remove age as Exp is important factor
summary(mod1)
vif(mod1) # Vif of Degree and Exp is reduced significantly and F-stats has increased

####################
## Var: y, x1, x2, x3, x4
cl152 <- read.csv("cls15-2.csv", header=TRUE)
cl152
summary(cl152)
str(cl152)

# generates pairwise plot
pairs(cl152) 

## Linear model
mod1<-lm(y~.,data=cl152)
summary(mod1)  # none of the coefficients are significant
plot(mod1$residuals, pch = 16, col = "red")

vif(mod1) # vif is to high
mod2<-lm(y~. -x4, data=cl152) # remove x4 with highest vif
summary(mod2)  # x3 is not significant yet
vif(mod2) # vif of x1 and x3 are still high
plot(mod2$residuals, pch = 16, col = "red")

mod3<-lm(y~.-x3-x4, data=cl152) # we would remove x3 and x4
summary(mod3) # both x1 and x2 are significant
vif(mod3) # vif is good now
plot(mod3$residuals, pch = 16, col = "red")
