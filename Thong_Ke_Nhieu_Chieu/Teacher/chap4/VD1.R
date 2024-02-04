###example 4.1

x=c(0,1,6,8,9,5,4,2)
tr=c(1,1,2,2,2,3,3,3)
#change data type to factor
ftr=factor(tr)
fit=aov(lm(x~ftr))
summary(fit)
#the group means are
tapply(x,tr,mean)
mean(x)
residuals(fit)  

#From the table we calculate the observed F
(F=(64.04/2)/(9.83/5))
g=3
n=length(x) 
ctble=qf(.99,g-1,n-g)
c(F,ctble)
