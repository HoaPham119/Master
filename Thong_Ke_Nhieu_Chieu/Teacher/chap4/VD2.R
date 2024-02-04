###Example 4.2

#We first do the decompositions of the two variables separately
#For the first variable
x11=c(0,1)
x12=c(6,8,9)
x13=c(5,4,2)
x1=c(x11,x12,x13)
n1=length(x11)
n2=length(x12)
n3=length(x13)
(xbar11=mean(x11)*c(rep(1,n1)))
(xbar12=mean(x12)*c(rep(1,n2)))
(xbar13=mean(x13)*c(rep(1,n3)))
(x1bar=mean(x1))
(x1bar1_3=c(xbar11,xbar12,xbar13))
(x1-x1bar1_3)
(SSobs1=t(x1)%*%x1)
(SSmean1=(n1+n2+n3)*x1bar**2)
(SStr1=t(x1bar1_3-x1bar)%*%(x1bar1_3-x1bar))#78
(SSres1=t(x1-x1bar1_3)%*%(x1-x1bar1_3))#10
(Totalcor1=SStr1+SSres1)#88
#For the second variable
#data as single column
 
x21=c(2,3)
x22=c(4,0, 5)
x23=c(8,9,7)
(x2=c(x21,x22,x23))
(xbar21=mean(x21)*c(rep(1,n1)))
(xbar22=mean(x22)*c(rep(1,n2)))
(xbar23=mean(x23)*c(rep(1,n3)))
(x2bar=mean(x2))
(x2bar1_3=c(xbar21,xbar22,xbar23))
(x2-x2bar1_3)
(SSobs2=t(x2)%*%x2)
(SSmean2=(n1+n2+n3)*x2bar**2)
(SStr2=t(x2bar1_3-x2bar)%*%(x2bar1_3-x2bar))#48
(SSres2=t(x2-x2bar1_3)%*%(x2-x2bar1_3))#24
(Totalcor2=SStr2+SSres2)#72
#Calculate the cross product terms
(SCPobs12=t(x2)%*%x1)
(SCPmean12=(n1+n2+n3)*x2bar*x1bar)
(SCPobs12-SCPmean12) #-11
(SCPtr12=t(x2bar1_3-x2bar)%*%(x1bar1_3-x1bar))#-12
(SCPres12=t(x2-x2bar1_3)%*%(x1-x1bar1_3))#1

#Form matrices
W=matrix(c(SSres1,SCPres12,SCPres12,SSres2),2,2)
B=matrix(c(SStr1,SCPtr12,SCPtr12,SStr2),2,2)
W
B
Lamb=det(W)/det(B+W)
Lamb
g=3
#transformed test statistic
TLam=((1-sqrt(Lamb))/sqrt(Lamb))*(n1+n2+n3-g-1)/(g-1)
crit=qf(.99,2*(g-1),2*(n1+n2+n3-g-1))
c(TLam,crit)

## Model fit using manova function in R
#change data type to factor
x11=c(0,1)
x12=c(6,8,9)
x13=c(5,4,2)
x1=c(x11,x12,x13)
x21=c(2,3)
x22=c(4,0,5)
x23=c(8,9,7)
x2=c(x21,x22,x23)


tr=c(1,1,2,2,2,3,3,3)
ftr=factor(tr)
(fit_ma=manova(lm(cbind(x1,x2)~ftr)))
summary(fit_ma)
anova(fit_ma)
