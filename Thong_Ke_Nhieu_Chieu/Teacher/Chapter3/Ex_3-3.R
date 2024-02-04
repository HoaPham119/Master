setwd("/Users/Jade/Desktop/GiaoAn/DH_KHTN/MongNgoc/Rscript_Data_Example/Chap3")
#Example 5.4 Simultaneous confidence intervals as shadows of the confidence ellipsoid
####Data for radiation from microwave Data về bức xạ của lò vi sóng

#requires library ellipse to draw ellipse
library(ellipse)
radC=read.table("T4-1.dat")
radO=read.table("T4-5.dat")
radcl=radC**(.25)
radop=radO**(.25)
rad=cbind(radcl,radop)
n=nrow(rad)
p=ncol(rad)

xbar=colMeans(rad)
S=cov(rad)
c2=p*(n-1)*qf(.95,p,n-p)/((n-p)*n)
#for door closed the endpoints of the shadow are
xbar[1]-sqrt(c2*S[1,1]) ##equ 5-24
xbar[1]+sqrt(c2*S[1,1])
#for door open
xbar[2]-sqrt(c2*S[2,2])
xbar[2]+sqrt(c2*S[2,2])

eli = ellipse(S, centre=xbar,t=sqrt(c2), npoint=5000)
 plot(eli, cex=.3,bty="n", xlab="Radiation door closed-fourth root", ylab="Radiation door open-fourth root",
 xlim=c(.5,.7),ylim=c(.5,.7),type="l",lwd=2,col="blue")
 segments(.516,.45,.516,.56,lty=2)
 segments(.612,.45,.612,.641,lty=2)
 segments(.45,.651,.60,.651,lty=2)
 segments(.45,.555,.522,.555,lty=2)

