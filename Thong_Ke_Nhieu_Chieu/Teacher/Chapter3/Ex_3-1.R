setwd("/Users/Jade/Desktop/GiaoAn/DH_KHTN/MongNgoc/Rscript_Data_Example/Chap5")
##example 5.1 Data : Perspiration from 20 healthy females was analyzed.
#X1: tỷ lệ mồ hôi
#X2, #X3: ll là hàm lượng natri, và kali

sweat=read.table("T5-1.dat")
dim(sweat)
names(sweat)=c("rate","sodium","potas")
(xbar=colMeans(sweat))
(mS=cov(sweat))
(solve(mS))
#xmu=c(4,50,10)
xmu=c(5,55,10)
n=nrow(sweat)
p=ncol(sweat)
(tsq=n*t(xbar-xmu)%*%solve(mS)%*%(xbar-xmu) )
ctsq=((n-p)/(p*(n-1)))*tsq #giá trị thống kê
c2=qf(.9,p,n-p)# critical value 
c(ctsq,c2)
#cc2=((n-1)*p/(n-p))*qf(.9,p,n-p)
#c(tsq,cc2)# tsq >c2, reject H0 at the 10% level of signification
