#### Example 6.10

#enter sample sizes, means and covariance matrices
n1=270
n2=130
n3=100
p=4
xbar1=c(2.1,.480,.082,.300)
xbar2=c(2.055,.596,.124,.400)
xbar3=c(2.222,.521,.125,.380)
S1=matrix(c(.3,-.001,.002,.010,-.001,.011,.000,.003,.002,.000,.001,.000,.010,.003,.000,.010),4,4)
S2=matrix(c(.555,.011,.001,.037,.011,.025,.004,.007,.001,.004,.005,.002,.037,.007,.002,.019),4,4)
S3=matrix(c(.255,.030,.003,.018,.030,.017,-.000,.006,.003,-.000,.004,.001,.018,.006,.001,.013),4,4)
#calculate statistics
W=(n1-1)*S1+(n2-1)*S2+(n3-1)*S3 # W different from text so different values for test statistic
xbar=(n1*xbar1+n2*xbar2+n3*xbar3)/(n1+n2+n3)
B=n1*(xbar1-xbar)%*%t(xbar1-xbar)+ n2*(xbar2-xbar)%*%t(xbar2-xbar)+n3*(xbar3-xbar)%*%t(xbar3-xbar)
Lamb=det(W)/det(B+W)
Lamb
#trasformed statistic to F
g=3
n=n1+n2+n3
T=((n-p-2)/p)*(1-sqrt(Lamb))/sqrt(Lamb)
crit=qf(.99,g-1,n-g)
c(T,crit)
#the approximation for log Lamb
Tlog=-(n-1-(p+g)/2)*log(Lamb)
critlog=qchisq(.99,p*(g-1))
c(Tlog,critlog) 
