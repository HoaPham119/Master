##Loading Required R packages

#install.packages("tiduverse")
library(tidyverse) ##tidyverse for easy data manipulation and visualization
library(caret) ## caret for easy machine learning workflow
theme_set(theme_bw())

 
# Load the data
 
data("marketing", package = "datarium")
# Inspect the data
#sample_n(marketing, 3)
head(marketing)
dim(marketing)

## Split the data into training and test set
set.seed(123)
training.samples <- marketing$sales  %>%  createDataPartition(p = 0.8, list = FALSE)
train.data  <- marketing[training.samples, ]

test.data <- marketing[-training.samples, ]
dim(train.data)
dim(test.data)

# Build the model
model <- lm(sales ~., data = train.data)
summary(model)
anova(model)
model2 <- lm(sales ~youtube +facebook, data = train.data)
summary(model2)
anova(model2)
## Fisher partial test
anova(model, model2)
anova(model2, model)

############
model1 <- lm(sales ~youtube, data = train.data)
model2 <- lm(sales ~youtube+ facebook, data = train.data)
model3 <- lm(sales ~youtube+ facebook+newspaper, data = train.data)
anova(model1)
anova(model2)
anova(model3)
simpleAnova(model1)
simpleAnova(model2)
simpleAnova(model3)
anova(model1, model2)

# Make predictions
predictions <- model2 %>% predict(test.data)

# Model performance
# (a) Prediction error, RMSE
RMSE(predictions, test.data$sales)
# (b) R-square
#R2(predictions, test.data$sales)

####Simple linear regression
model <- lm(sales ~ youtube, data = train.data)
summary(model)
summary(model)$coef
newdata <- data.frame(youtube = c(0,  1000))
model %>% predict(newdata)

####Multiple linear regression
model <- lm(sales ~., data = train.data)
#model <- lm(sales ~ youtube + facebook + newspaper,  data = train.data)
summary(model)
summary(model)$coef
# New advertising budgets
newdata <- data.frame( youtube = 2000, facebook = 1000, newspaper = 1000)
# Predict sales values
model %>% predict(newdata)
modelR <- lm(sales ~ youtube + facebook ,  data = train.data)
newdataR<- data.frame( youtube = 2000, facebook = 1000)
model2 %>% predict(newdataR)

predict(model2, newdata =newdataR , interval = "confidence", level=0.99)
predict(model2, newdata =newdataR , interval = "prediction", level=0.99)


newdataR1<- data.frame( youtube = 2000)
model1 %>% predict(newdataR1)
# Make predictions
predictions <- model %>% predict(test.data)
predictionsR <- modelR %>% predict(test.data)
# Model performance
# (a) Compute the prediction error, RMSE
ss=RMSE(predictions, test.data$sales)
ssR=RMSE(predictionsR, test.data$sales)
# (b) Compute R-square
r2=R2(predictions, test.data$sales)
r2R=R2(predictionsR, test.data$sales)
##scatter plot
ggplot(marketing, aes(x = youtube, y = sales)) + geom_point() +stat_smooth()
