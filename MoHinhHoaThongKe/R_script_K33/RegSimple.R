setwd("/Users/jadenguyen/Desktop/GiaoAn/DH_KHTN/HKII_2021_2022/CH/MHHTK/Cours_GV/R_RegLin")
View(cars)
str(cars)
dim(cars)
head(cars)
attach(cars)
?cars
plot(cars)

stop_dist_model = lm(dist ~ speed, data = cars)

coef(stop_dist_model)


#
 plot(dist ~ speed, data = cars,
      xlab = "Speed (in Miles Per Hour)",
      ylab = "Stopping Distance (in Feet)",
      main = "Stopping Distance vs Speed",
      pch  = 20,
      cex  = 2,
      col  = "grey")
 abline(stop_dist_model, lwd = 3, col = "darkorange")
 
 names(stop_dist_model)
 stop_dist_model$coefficients
 stop_dist_model$residuals
 stop_dist_model$fitted.values
 

 fitted(stop_dist_model)
 
 #confidence intervals for β_0 and β_1
 stop_dist_model$coefficients
 confint(stop_dist_model)
 confint(stop_dist_model, level = 0.99)
 
 #confidence interval for the mean response
 new_speeds = data.frame(speed = c(5, 21))
 
 predict(stop_dist_model, newdata = new_speeds, 
         interval = c("confidence"), level = 0.99)
 
 #Prediction Interval for New Observations
 predict(stop_dist_model, newdata = new_speeds, 
         interval = c("prediction"), level = 0.99)
 
 ###Confidence and Prediction Bands
 
 speed_grid = seq(min(cars$speed), max(cars$speed), by = 0.01)
 dist_ci_band = predict(stop_dist_model, 
                        newdata = data.frame(speed = speed_grid), 
                        interval = "confidence", level = 0.99)
 dist_pi_band = predict(stop_dist_model, 
                        newdata = data.frame(speed = speed_grid), 
                        interval = "prediction", level = 0.99) 
 
 plot(dist ~ speed, data = cars,
      xlab = "Speed (in Miles Per Hour)",
      ylab = "Stopping Distance (in Feet)",
      main = "Stopping Distance vs Speed",
      pch  = 20,
      cex  = 2,
      col  = "grey",
      ylim = c(min(dist_pi_band), max(dist_pi_band)))
 abline(stop_dist_model, lwd = 5, col = "darkorange")
 
 lines(speed_grid, dist_ci_band[,"lwr"], col = "dodgerblue", lwd = 3, lty = 2)
 lines(speed_grid, dist_ci_band[,"upr"], col = "dodgerblue", lwd = 3, lty = 2)
 lines(speed_grid, dist_pi_band[,"lwr"], col = "dodgerblue", lwd = 3, lty = 3)
 lines(speed_grid, dist_pi_band[,"upr"], col = "dodgerblue", lwd = 3, lty = 3)
 points(mean(cars$speed), mean(cars$dist), pch = "+", cex = 3)
 
 #test
 #verifier les hypotheses
 residus = residuals(stop_dist_model)
 shapiro.test(residus)
 ?shapiro.test
 
 #test sur les beta
 summary(stop_dist_model)$coefficients
 summary(stop_dist_model)
 anova(stop_dist_model)
 
