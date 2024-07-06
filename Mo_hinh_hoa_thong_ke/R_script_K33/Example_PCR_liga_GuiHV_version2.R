setwd("/Users/Jade/Desktop/GiaoAn/DH_KHTN/HKI_2023-2024/CH/MHHTK_DS/PCR")
laliga <- readxl::read_excel("la-liga-2015-2016.xlsx", sheet = 1, col_names = TRUE) 
laliga <- as.data.frame(laliga) # Avoid tibble since it drops row.names
dim(laliga)
head(laliga)
names(laliga)
str(laliga)
attach(laliga)

(rownames(laliga) <- Team) #set teams as case names to avoid factors 
laliga <- laliga[,-c(1,3, 9)] # Do not add irrelevant information, #Points and Difference.goals 
dim(laliga)
head(laliga)
summary(laliga)

####### PCA #########
# PCA
pcaLaliga <- princomp(laliga, fix_sign = TRUE)
summary(pcaLaliga)

#### Plot of variances of each component (screeplot)
plot(pcaLaliga, type = "l")


 # The eigenvectors (the a_j vectors) are the column vectors in $loadings
 pcaLaliga$loadings
 
 # Uncorrelated
 corrplot::corrplot(cov(pcaLaliga$scores), addCoef.col = "gray", is.corr = FALSE)
 
 # The scores are A’ * (X_i - mu). We center the data with scale() # and then multiply each row by A’
 scores <- scale(laliga, center = TRUE, scale = FALSE)%*% A
 
 # Reconstruct the data from all the principal components
 head(sweep(pcaLaliga$scores %*% t(pcaLaliga$loadings), 2, pcaLaliga$center, "+") )
 
 ###############  standardize the dataset prior to do a PCA #####
 # Use cor = TRUE to standardize variables (all have unit variance) # and avoid scale distortions
 pcaLaligaStd <- princomp(x = laliga, cor = TRUE, fix_sign = TRUE)
 summary(pcaLaligaStd)

 biplot(pcaLaligaStd, cex = 0.75)
 
 ############ PCR ########
 # A linear model is problematic
 mod <- lm(Points ~ . - Wins - Draws - Loses - Matches.without.conceding, data = laliga)
 summary(mod) # Lots of non-significant predictors
 # We try to clean the model
 modBIC <- MASS::stepAIC(mod, k = log(nrow(laliga)), trace = 0) 
 summary(modBIC) # Better, but still unsatisfactory
 
 # Also, huge multicollinearity
 car::vif(modBIC)
 # A quick way of removing columns without knowing its position
 laligaRed <- subset(laliga, select = -c(Points, Wins, Draws, Loses, Matches.without.conceding))
 
 # PCA without Points, Wins, Draws, Loses, and Matches.without.conceding
 pcaLaligaRed <- princomp(x = laligaRed, cor = TRUE, fix_sign = TRUE)
 summary(pcaLaligaRed) # l = 3 gives 86% of variance explained
 pcaLaligaRed$loadings
 # Interpretation of PC1 and PC2
 biplot(pcaLaligaRed)
 # PC1: attack performance of the team
 
 # Create a new dataset with the response + principal components
 laligaPCA <- data.frame("Points" = laliga$Points, pcaLaligaRed$scores)
 laligaPCA <- data.frame("Points" = laliga$Points, pcaLaligaRed$scores) # Regression on all the principal components
 modPCA <- lm(Points ~ ., data = laligaPCA)
 summary(modPCA) # Predictors clearly significative -- same R^2 as mod
 
  car::vif(modPCA) # No problems at all
  # Using the first three components
  modPCA3 <- lm(Points ~ Comp.1 + Comp.2 + Comp.3, data = laligaPCA) 
  summary(modPCA3)
  # Coefficients associated to each original predictor (gamma)
  (alpha <- modPCA3$coefficients)
 (gamma <- pcaLaligaRed$loadings[, 1:3] %*% alpha[-1]) # Slopes
  (gamma <- c(alpha[1] - pcaLaligaRed$center %*% gamma, gamma) )# Intercept
 

  # principal component does quite well
  modPCABIC <- MASS::stepAIC(modPCA, k = 2 * log(nrow(laliga)), trace = 0) 
  summary(modPCABIC)
  
  # Predictions for FCB and RMA (although they are part of the training sample) 
  newPredictors <- laligaRed[1:2, ]
  newPredictors <- scale(newPredictors, center = pcaLaligaRed$center, scale = pcaLaligaRed$scale) # Centered and scaled
  newScores <- t(apply(newPredictors, 1,function(x) t(pcaLaligaRed$loadings) %*% x))

  # We need a data frame for prediction
  newScores <- data.frame("Comp" = newScores)
  predict(modPCABIC, newdata = newScores, interval = "prediction") #
  # Reality
  laliga[1:2, 1]
  
  
  
  