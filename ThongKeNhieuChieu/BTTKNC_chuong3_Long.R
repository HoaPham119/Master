# Bài tập 3.3 Lấy data từ Ví dụ 3.1
install.packages("readxl")
install.packages("writexl")
library(readxl)

setwd("/Users/248ntt/Downloads/R Example")
##example 5.1 Data : Perspiration from 20 healthy females was analyzed.
#X1: tỷ lệ mồ hôi
#X2, #X3: ll là hàm lượng natri, và kali

# Đọc dữ liệu đầu vào 
sweat = read.table("T5-1.dat")
sweat = data.frame(sweat)


# Số lượng các quan sát và số chiều
n = dim(sweat)[1]
p = dim(sweat)[2]

# Đặt tên choc các trường dữ liệu
names(sweat) = c("rate","sodium","potas") 

# Tính vector trung bình xbar
xbar = colMeans(sweat)

# Tính ma trận hiệp phương sai mẫu S
S = cov(sweat)
S


# câu a: Khoảng tin cậy đồng thời 95% T^2 cho muy_1, muy_2 và muy_3

#Ta tìm giá trị của phân vị Fisher với bậc tự do p và n - p với độ tin cậy 95%

f_c = qf(.05, df1 = p , df2 = n-p,  lower.tail= FALSE)

#Theo công thức 12 ta tính được

c = sqrt((p*(n-1)/(n-p))*f_c)

# vậy ta có khoảng tin cậy đồng thời T^2 95% cho muy_1, muy_2 và muy_3 là 

a = diag(3)
a
ktc = c()

for (i in 1:3)
{
  
ktc0 = c(t(a[i,])%*%xbar - c*sqrt(t(a[i,])%*%S%*%a[i,]/n) , t(a[i,])%*%xbar + c*sqrt(t(a[i,])%*%S%*%a[i,]/n) )
names(ktc0)= c(names(sweat)[i], names(sweat)[i])
ktc <- append(ktc,ktc0)

}

ktc

# câu b: Khoảng tin cậy đồng thời 95% Bonferroni

t_f = qt(p = 1- (1-0.95)/(2*p), df = n - 1 )

b = rep(1,3)
ktc_bf = c()

for(i in 1:3)
{

ktc0 = c(xbar[i] - t_f*sqrt(S[i,i]/n) , xbar[i] + t_f*sqrt(S[i,i]/n) )
names(ktc0)= c(names(sweat)[i], names(sweat)[i])
ktc_bf <- append(ktc_bf,ktc0)

}

ktc_bf

# câu c: 

# Ta thấy ktc chứa ktc_bf
# Nghĩa là các khoảng tin cậy đồng thời thông thường chứa ktc đồng thời Bonferroni với cùng độ tin cậy 95%
# Vậy KTC đồng thời Bonferroni ở câu b là tốt hơn so với câu a

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

# Bài tập 3.4 các vecto X_i có phân phối chuẩn 2 chiều với các quan trắc x_i, i từ 1,2,...42.

# Ta có:
p = 2
n = 42

x_bar = c(0.564, 0.603)

S = cbind(c(0.0144, 0.0117), c(0.0117, 0.0146))
S_inv = solve(S)

# câu a:
# Theo định lý 2.2.1 ta có S như trên chính là ước lượng hợp lý cực đại cho ma trận hiệp phương sai Sigma.

# câu b: Miền tin cậy ellip R1 với độ tin cậy 95%

# Tính các giá trị cần thiết

c_square = p*(n-1)*qf(.95,p,n-p)/((n-p)*n)

# Miền tin cậy ellip R1 như hình vẽ sau: 

eli = ellipse(S, centre=x_bar,t=sqrt(c_square), npoint = 5000)
plot(eli, cex=.3, bty="n", xlab="Atribute 1", ylab="Atribute 2",
     xlim=c(.49,.7),ylim=c(.49,.7),type="l",lwd=2,col="green")

x_bar[1]-sqrt(c2*S[1,1]) 
x_bar[1]+sqrt(c2*S[1,1])
x_bar[2]-sqrt(c2*S[2,2])
x_bar[2]+sqrt(c2*S[2,2])

segments(.516,.45,.516,.56,lty= 2)
segments(.612,.45,.612,.641,lty= 2)
segments(.45,.651,.60,.651,lty= 2)
segments(.45,.555,.522,.555,lty= 2)

# câu c: 
muy_0 = c(0.60, 0.58)

T_statistic = (n-p)/(p*(n-1))*t(x_bar-muy_0)%*%S_inv%*%(x_bar-muy_0)

f_cr = qf(.05, df1 = p , df2 = n-p,  lower.tail= FALSE)
f_cr

# Ta thấy T_statistic < f_cr  nên muy_0 thuộc miền tin cậy R1

# câu d:

#Theo công thức 10 ta tính được

c_new = sqrt((p*(n-1)/(n-p))*f_cr)

t_c = qt(p = (1+0.95)/2, df = n - 1 )

# vậy ta có khoảng tin cậy đồng thời cho muy_1 và muy_2 là 

b = rep(1,2)
ktc1 = c()
names = c("muy1","muy2")

for(i in 1:2)
{
  
  ktc0 = c(x_bar[i] - t_c*sqrt(S[i,i]/n) , x_bar[i] + t_c*sqrt(S[i,i]/n))
  names(ktc0)= c(names[i], names[i])
  ktc1 <- append(ktc1,ktc0)
  
}

ktc1


# câu e:  Kiểmn định giả thiết H0: muy = muy_0 = (0.60, 0.58)

#Tính

t_square = n*t(x_bar - muy_0)%*%S_inv%*%(x_bar - muy_0) 

t_cr = ((n-p)/(p*(n-1)))*t_square
t_cr

f_cr = qf(.05, df1 = p , df2 = n-p,  lower.tail= FALSE)

# Ta thấy t_cr > f_cr nên ta bác bỏ giả thiết H0 tức là bác bỏ muy = (0.60, 0.58) vói độ tin cậy 95%

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

# Bài tập 3.5: Từ bảng số liệu thu nhập trên n = 45 con chim. 

# Đọc dữ liệu được nhập vào 
bt5 = read_excel("data_bt5.xls")
data_bt5 = data.frame(bt5)
data_bt5

names(data_bt5)=c("tail length","wing length")

data_bt5

# Ta có số quan sát n và số thuọc tính p

n = dim(data_bt5)[1]
p = dim(data_bt5)[2]

# Câu a: 

# Tính vector trung bình xbar
xbar=colMeans(data_bt5)
xbar

# Tính ma trận hiệp phương sai mẫu S
S = cov(data_bt5)
S
S_inv = solve(S)
# Tính giá trị thống kê cần thiết

csquare = p*(n-1)*qf(.95,p,n-p,lower.tail = TRUE)/((n-p)*n)

# Miền tin cậy 95% ellip R1 như hình vẽ sau: 

eli_2 = ellipse(S, centre = xbar,t = sqrt(csquare), npoint = 5000)
plot(eli_2, points(c(190, 275), col="red", pch="*"), cex =.8, bty = "n", xlab = "tail length", ylab = "wing length",
     xlim = c(187,205),ylim = c(265,290),type = "l",lwd = 2,col = "green")

muy_O = c(190, 275)

t_square = (n*(n-p))/(p*(n-1))*t(xbar-muy_O)%*%S_inv%*%(xbar-muy_O)

f_cr = qf(.05, df1 = p , df2 = n-p,  lower.tail= FALSE)

c(t_square,f_cr)

# ta thấy giá trị t_square nhỏ hơn giá trị phân vị của phân phối Fisher với bậc tự do p và n-p ở mức xác suất 95%
# Vậy các giá trị muy_1 và muy_2 ở trên là hợp lý

# Câu b: Khoảng tin cậy đồng thời T^2 95% cho muy_1 và muy_2


#Theo công thức 12 ta tính được

c = sqrt((p*(n-1)/(n-p))*f_cr)

# vậy ta có khoảng tin cậy đồng thời T^2 95% cho muy_1, muy_2 và muy_3 là 

a = diag(2)
ktc2 = c()
names = c("tail length","wing length")

for (i in 1:2)
{
  
  ktc0 = c(t(a[i,])%*%xbar - c*sqrt(t(a[i,])%*%S%*%a[i,]/n) , t(a[i,])%*%xbar + c*sqrt(t(a[i,])%*%S%*%a[i,]/n) )
  names(ktc0)= c(names[i], names[i])
  ktc2 <- append(ktc2,ktc0)
  
}

ktc2

# Khoảng tin cậy đồng thời 95% Bonferroni

t_f = qt(p = 1- (1-0.95)/(2*p), df = n - 1 )

b = rep(1,2)
ktc_bf1 = c()

for(i in 1:2)
{
  
  ktc0 = c(xbar[i] - t_f*sqrt(S[i,i]/n) , xbar[i] + t_f*sqrt(S[i,i]/n) )
  names(ktc0)= c(names[i], names[i])
  ktc_bf1 <- append(ktc_bf1,ktc0)
  
}

ktc_bf1

# Ta nhận thấy khoảng T^2 rộng hơn khoảng tin cậy Bonferroni

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------


#Câu 6: Quan trắc trên n = 61 con gấu theo các biến X1 = Weight (đơn vị kg), X2 = Body length (đơn vị cm), X3 = Neck (đơn vị cm),
# X4 = Girth (đơn vị cm), X5 = Head length (đơn vị cm), X6 = Head width (đơn vị cm)

n=61
p=6

# vector trung bình mẫu

xbar = c(95.52, 164.38, 55.69, 93.39, 17.98, 31.13)

# Ma trận hiệp phương sai mẫu S

S = cbind(c1 = c(3266.46, 1343.97, 731.54, 1175.50, 162.68, 238.37),
          c2 = c(1343.97, 721.91, 324.25, 537.35, 80.17, 117.73),
          c3 = c(731.54, 324.25, 179.28, 281.17, 39.15, 56.80),
          c4 = c(1175.50, 537.35, 281.17, 474.98, 63.73, 94.85),
          c5 = c(162.68, 80.17, 39.15, 63.73, 9.95, 13.88),
          c6 = c(238.37, 117.73, 56.80, 94.85, 13.88, 21.16))
S

# câu a: KTC đồng thời 95% mẫu lớn cho 6 trung bình 

#Theo công thức 10 ta tính được

c_cr = sqrt((p*(n-1)/(n-p))*f_cr)


t_c = qt(p = (1+0.95)/2, df = n - 1 )

# vậy ta có khoảng tin cậy đồng thời cho các trung bình

a = rep(1,6)
ktc3 = c()
names = c("muy1","muy2","muy3","muy4","muy5","muy6")

for (i in 1:6)
{
  ktc0 = c(xbar[i] - t_c*sqrt(S[i,i]/n) , xbar[i] + t_c*sqrt(S[i,i]/n))
  names(ktc0)= c(names[i], names[i])
  ktc3 <- append(ktc3,ktc0)
}

ktc3

# Câu b: Khoảng tin cậy ellipse 95% cho trung bình muy1 và muy4

xbar_1_4 = c(xbar[1], xbar[4])
xbar_1_4

S_1_4 = rbind( c1 = c(S[1,1],S[1,4]), c4= c(S[1,4],S[4,4]))
S_1_4

p1 = 2

# Tính các giá trị cần thiết

c_square_1 =  qchisq(.95,df = p1,lower.tail = TRUE)
c_square_1


