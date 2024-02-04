
setwd("/Users/Jade/Desktop/GiaoAn/DH_KHTN/MongNgoc/Rscript_Data_Example/Chap3")
#Example5.3 Data for radiation from microwave Data về bức xạ của lò vi sóng

#install packagae  ellipse to draw ellipse

library(ellipse)
radC=read.table("T4-1.DAT",header=F)
dim(radC)
names(radC)
radO=read.table("T4-5.DAT",header=F)
dim(radO)
radcl=radC**(.25) #radcl=(radC)^(1/4)
head(radcl)
head(radC)
radop=radO**(.25)
rad=cbind(radcl,radop)
dim(rad)
n=nrow(rad)
p=ncol(rad)
(xbar=colMeans(rad))
(S=cov(rad))
solve(S)
#eigenvalue and eigenvector pairs for S
eig=eigen(S)
eig$values
eig$vectors
# c^2=(p(n-1))/(n-p)F_{p,n-p}(alpha)
(c2=p*(n-1)*qf(.95,p,n-p)/(n-p))

###The 95% confidence ellipse for \mu=(mu1, mu2)
#check if in ellipse
xmu=c(.562,.589)
xbar
tsq=n*t(xbar-xmu)%*%solve(S)%*%(xbar-xmu)
c(tsq,c2)# tsq <c2, xmu is in the region.
#(equivalently, a test .... not reject H0 at the 5% level of signification)

###draw ellipse
#eig=eigen(S)
a1=xbar-sqrt(c2*eig$value[1]/n)%*% eig$vector[,1] #equ 5-19
b1=xbar+sqrt(c2*eig$value[1]/n)%*% eig$vector[,1]
a2=xbar-sqrt(c2*eig$value[2]/n)%*% eig$vector[,2]
b2=xbar+sqrt(c2*eig$value[2]/n)%*% eig$vector[,2]

eli = ellipse(S, centre=xbar,t=sqrt(c2/n), npoint=5000)
plot(eli, cex=.3,bty="n", xlab="Radiation door closed-fourth root", ylab="Radiation door open-fourth root",
 xlim=c(.5,.7),ylim=c(.5,.7),type="l",lwd=2,col="blue")
 points(xbar[1],xbar[2],pch=19)
 segments(xbar[1],xbar[2],a1[1],a1[2])
 segments(xbar[1],xbar[2],b1[1],b1[2])
 segments(xbar[1],xbar[2],a2[1],a2[2])
 segments(xbar[1],xbar[2],b2[1],b2[2])

