# iris dataset -- factors in the last column
dim(iris)
head(iris)

summary(iris)
# Summary of a linear model
mod1 <- lm(Sepal.Length ~ ., data = iris) 
summary(mod1)
# How to set a different level as reference (versicolor)
iris$Species <- relevel(iris$Species, ref = "setosa")
# Same estimates except for the dummy coefficients
mod2 <- lm(Sepal.Length ~ ., data = iris) 
summary(mod2)
confint(mod2)

# Show the dummy variables employed for encoding a factor
contrasts(iris$Species)

# exemple
#iris$Species <- relevel(iris$Species, ref = "setosa") 
#contrasts(iris$Species)

#nêu varibale x nhan ky tu thi can as.factor(x) de co duoc levels