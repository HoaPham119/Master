
#install.packages("tiduverse")# for easy data manipulation and visualization
#install.packages("caret") #for easy machine learning workflow
#install.packages("leaps") # for computing stepwise regression
#library(tidyverse)
#library(caret)
#library(leaps) # for computing stepwise regression

#Computing stepwise regression

# Load the data
data("swiss")
dim(swiss)
#swiss {datasets}:47 observations on 6 variables:
?swiss
dim(swiss)
head(swiss)

cor(swiss)
###stepAIC() package "MASS"
library(MASS)
# Fit the full model 
full.model <- lm(Fertility ~., data = swiss)
full.model$coefficients
summary(full.model)

model1 <- lm(Fertility ~., data = swiss[,-3])
summary(model1)

# Stepwise regression model
#?stepAIC
model_best <- step(full.model, direction = "both",trace = FALSE)
step.model <- stepAIC(full.model, direction = "both",trace = FALSE)
summary(step.model)

###regsubsets() [leaps package]
library(leaps) 
#?regsubsets
models <- regsubsets(Fertility~., data = swiss, nvmax = 5, method = "seqrep")
summary(models)
summary(models)$cp
summary(models)$adjr2

#Model selection criteria: Adjusted R2, Cp and BIC
res.sum <- summary(models)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)
###train() function [caret package] 
# "leapBackward", to fit linear regression with backward selection
# "leapForward", to fit linear regression with forward selection
# "leapSeq", to fit linear regression with stepwise selection .
library(caret)
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)

# Train the model
step.model <- train(Fertility ~., data = swiss,method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),trControl = train.control)
step.model$results
step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel, 4)

lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality, 
   data = swiss)

#########
library(MASS)
res.lm <- lm(Fertility ~., data = swiss)
step <- stepAIC(res.lm, direction = "both", trace = FALSE)
step
###
# Train the model
step.model <- train(Fertility ~., data = swiss,
                    method = "lmStepAIC", 
                    trControl = train.control,
                    trace = FALSE
)
# Model accuracy
step.model$results
# Final model coefficients
step.model$finalModel
# Summary of the model
summary(step.model$finalModel)
