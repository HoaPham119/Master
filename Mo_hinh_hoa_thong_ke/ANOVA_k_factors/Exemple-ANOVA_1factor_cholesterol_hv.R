
library(multcomp)
data(cholesterol)
head(cholesterol)
dim(cholesterol)
str(cholesterol)

#Trự quan  dữ liệu
library(ggplot2)
ggplot(cholesterol, aes(y=response, x=trt,colour=trt ,fill=trt))+
  geom_boxplot(alpha=0.5, outlier.alpha=0)+
  geom_jitter(width=0.25)+
  theme_classic() 

# Tính trung bình mẫu của 5 tổg thể và khoảng tin cậy của 5 trung binh tổng thể
library(Publish) 
ci.mean(response~trt,data=cholesterol)

# Thực hiện  ANOVA k nhân tố 
###có 2 lệnh chính để ANOVA k nhân tố
is.factor(cholesterol$trt)
lm1 <- lm(response~trt, data=cholesterol) 
anova(lm1)
library(car)
Anova(lm1)

aov1 <- aov(response~trt, data=cholesterol) 
summary(aov1)


###### Kiểm tra các giả định của mô hình
#1. tính độc lập của esiplon 
# Kiểm định Dubin-Watson
   library(car)
   durbinWatsonTest(lm1) 
  #durbinWatsonTest(aov1)

#2. kIỂM ĐỊNH TÍNH CHUẨN của esiplon 
  # Q-Q plot 
  plot(lm1,2)  
  # Kiểm định sHAPRIO Wilk
  shapiro.test(residuals(lm1))

#3. tính đồng nhất phương sai
     plot(lm1,3)
     # Kiểm bartlett
     bartlett.test(residuals(lm1)~cholesterol$trt)
     # kiểm định Leven
      library(car)
      leveneTest(residuals(lm1)~cholesterol$trt)
    # kiểm định Fligner
    fligner.test(residuals(lm1)~cholesterol$trt)
#Vẽ 4 đồ thị chia theo 2-2
par(mfrow=c(2,2)) 
plot(lm1) 

### So sánh bội sau ANOVA (#Post Hoc Testing)
 ####1.Test all possible comparisons of two means
      #Kiểm định Tukey
           TukeyHSD(aov1)  
           plot(TukeyHSD(aov1) )
     #kiểm định t-test với p giá trị hiệu chỉnh với phuong pháp  "bonferroni"
           pairwise.t.test(coagulation$coag, coagulation$diet, p.adj = "bonferroni")


######2. So  sánh từng  trung bình của các nhóm  với trung bình của nhóm đối chứng
# Kiểm định Dunnett
#####Xác định nhóm “drugE”  là nhóm đối chứng
cholesterol$trt <- relevel(cholesterol$trt, ref="drugE")
lm2 <- lm(response~trt, data=cholesterol) 
Anova(lm2)
mc_dunnett <- glht(lm2, linfct=mcp(trt="Dunnett")) 
summary(mc_dunnett)
plot(mc_dunnett) 
#modifier les marges
par(mar=c(3,7,3,3))
plot(mc_dunnett) 

#Test de Tukey
mc_tukey <- glht(lm2, linfct=mcp(trt="Tukey")) 
summary(mc_tukey) 
par(mar=c(3,7,3,3))
plot(mc_tukey)


####Phần thêm khi dùng gói lệch multcomp
library(multcomp)
tuk.cld <- cld(mc_tukey) 
tuk.cld 
library(ggplot2)
letters <- tuk.cld$mcletters$Letters
myletters_df <- data.frame(trt=levels(cholesterol$trt),letters=letters)
myletters_df
ggplot(cholesterol, aes(x=trt, y=response, colour=trt, fill=trt))+
  geom_boxplot(outlier.alpha = 0, alpha=0.25)+
  geom_jitter(width=0.25)+  
  stat_summary(fun.y=mean, colour="black", geom="point", shape=18, size=3) +
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
  geom_text(data = myletters_df, aes(label = letters, y = 30 ), colour="black", size=5) 
