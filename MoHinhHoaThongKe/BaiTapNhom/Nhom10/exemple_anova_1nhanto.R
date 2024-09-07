#exemple: dữ liệu đông máu:bốn chế độ ăn khác nhau (A, B, C, D) được sử dụng 
#cho một mẫu ngẫu nhiên gồm 24 con vật và được chỉ định ngẫu nhiên vào một 
#trong bốn chế độ ăn kiêng.


library(faraway)
names(coagulation)
plot(coag ~ diet, data = coagulation, col = 2:5)
dim(coagulation)
coagulation

# aov() function is used to obtain the relevant sums of squares.
coag_aov = aov(coag ~ diet, data = coagulation)
coag_aov
summary(coag_aov)
#created a dataframe with a row for each diet,By predicting on this dataframe, 
#we obtain the sample means of each diet (group).
diets = data.frame(diet = unique(coagulation$diet))
data.frame(diets, coag = predict(coag_aov, diets))

#Post Hoc Testing
#test all possible comparisons of two means. 
with(coagulation, pairwise.t.test(coag, diet, p.adj = "none"))
#neu ko dung which thi
 pairwise.t.test(coagulation$coag, coagulation$diet, p.adj = "none")

#the Bonferroni correction
with(coagulation, pairwise.t.test(coag, diet, p.adj = "bonferroni"))


##Tukey’s Honest Significance difference 
TukeyHSD(coag_aov, conf.level = 0.95)
# produce a plot of these confidence intervals
plot(TukeyHSD(coag_aov, conf.level = 0.95))
