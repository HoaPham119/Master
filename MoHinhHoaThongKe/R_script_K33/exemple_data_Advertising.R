
setwd("/Users/Jade/Desktop/GiaoAn/DH_KHTN/HKII_2023_2024/CH/MHHTK/R_script/Datasets")

#dev.off ()

Advertising<-read.csv(file.choose(), header = TRUE, sep = ",")
#Advertising <- read.table(file = "Advertising.csv", header = TRUE, sep = ",")
dim(Advertising)
head(Advertising)
str(Advertising)
summary(Advertising)
plot(Advertising)

#M1 <- lm(Advertising$sales ~ Advertising$TV)
#dev.off ()

Advertising<-read.csv(file.choose(), header = TRUE, sep = ",")
#Advertising <- read.table(file = "Advertising.csv", header = TRUE, sep = ",")
dim(Advertising)
head(Advertising)
summary(Advertising)
plot(Advertising)

# 1.  Reg simle
model <- lm(sales ~ TV, data=Advertising)

attach(Advertising)
model <- lm(sales ~ TV)

summary(model)
anova(model)
names(model)
coef(model)
confint(model,level = 0.99)

plot( TV,sales)
abline(model,lwd=3,col="red")

##Assumptions of the model
par(mfrow=c(2,2))
plot(model)

dev.off()
#### a. Linearity
plot(model, 1) 

#### b. Normality
  ## QQ-plot (Theoretical Quantile vs. Empirical Quantile)
plot(model, 2)
  ## Shapiro-Wilk test of normality
shapiro.test(model$residuals)
  ## Lilliefors test 
##-- the Kolmogorov-Smirnov adaptation for testing normality

#installed.packages(nortest)
#library(nortest)
#lillie.test(model$residuals)

nortest::lillie.test(model$residuals)

#### c. Homoscedasticity 
    ## scale-location plot
plot(model, 3)

## Breusch-Pagan test
car::ncvTest(model)


??bptest
## Test of Equal Variance
lmtest::bptest(model, studentize=FALSE)
            #install.packages("lmtest")
              #library(lmtest)
            #bptest(model, studentize=FALSE)
## d. Independence 
##serial plot of the residuals
plot(model$residuals, type = "o")
##lagged plots of (\hat\epsilon_{i-l},\hat\epsilon_{i}),i = l + 1, . . . , n, 
lag.plot(model$residuals, lags = 1, do.lines = FALSE)
#cor(mod$residuals[-1], mod$residuals[-length(mod$residuals)])

##Durbin–Watson test
car::durbinWatsonTest(model)
#Does not reject at alpha = 0.05
## Predict
#Prediction Interval for New Observations
new_TV = data.frame(TV = c(75, 123))
predict(model, newdata = new_TV, 
        interval = c("prediction"))

#confidence interval for the mean response
predict(model, newdata = new_TV, 
        interval = c("confidence"))



#plot(predict(model), residuals(model))
# plot(predict(model), rstudent(model))
# plot(hatvalues(model))
 #which.max(hatvalues(model))


### 2. Reg mult
mod<- lm(sales ~., data=Advertising)
summary(mod)

mod<- lm(sales ~TV+newspaper, data=Advertising)
mod1=lm(sales ~., data=Advertising[,-4])
summary(mod1)



