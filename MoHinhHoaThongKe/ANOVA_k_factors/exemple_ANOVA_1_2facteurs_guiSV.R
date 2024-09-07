############ ANOVA à 1 facteurs #########
##########################################

#dataset from the faraway package.
#n=24 animals and  4 diets (A, B, C, D)
#blood coagulation time (s)
#  μA is the mean blood coagulation time for an animal that ate diet A
library(faraway)
names(coagulation)
coagulation

#plot alone suggests a difference of means. 
plot(coag ~ diet, data = coagulation, col = 2:5)

#aov() creates the desired ANOVA table
coag_aov = aov(coag ~ diet, data = coagulation)
coag_aov
summary(coag_aov)
## ==>>>the diets had an effect on blood coagulation time

diets = data.frame(diet = unique(coagulation$diet))
data.frame(diets, coag = predict(coag_aov, diets))


###### Post Hoc Testing #####
with(coagulation, pairwise.t.test(coag, diet, p.adj = "none"))
#pairwise.t.test(coagulation$coag, coagulation$diet, p.adj = "none")
with(coagulation, pairwise.t.test(coag, diet, p.adj = "bonferroni"))

#### Tukey’s Honest Significance differencHoneste
#TukeyHSD(coag_aov)
TukeyHSD(coag_aov, conf.level = 0.99)
plot(TukeyHSD(coag_aov, conf.level = 0.95))


############ ANOVA 2 nhân tố  #########
##########################################
#the rats data from the faraway package. 
##two factors: poison and treat
#n=48 rats; one of three poisons and one of poison four possible treatments. 
##=> 12 groups, each with 4 replicates.rep

library(faraway)
levels(rats$poison) #the levels() functionpois to extract the levels of a factor variable.
levels(rats$treat)trea

#create interaction plots: nhằm đánh giá ảnh hưởng của nhân tố khi 
###############@#ta chchuyển qua các câp độ  của nhân tố khác 
## song song thì ko có tcóương tác
# các đuong cắt ngang và đi theo hương khác nhau: có tương tác
par(mfrow = c(1, 2))
with(rats, interaction.plointeractit(poison, treat, time, lwd = 2, col = 1:4))
with(rats, interaction.plot(treat, poison, time, lwd = 2, col = 1:3))
 
# fit each of the possible models
rats_int   = aov(time ~ poison * treat, data = rats) # interaction model
rats_add   = aov(time ~ poison + treat, data = rats) # additive model
rats_pois  = aov(time ~ poison , data = rats)        # single factor model   
rats_treat = aov(time ~ treat, data = rats)          # single factor model
rats_null  = aov(time ~ 1, data = rats)              # null model



##create  ANOVA table
#model hierarchy
summary(aov(time ~ poison * treat, data = rats))
###===>l'interaction n'est pas significative.
#######les deux facteurs sont significatifs,==> sélectionnons donc le modèle additif.

#Within the additive model, we could do further testing about the main effects.
TukeyHSD(aov(time ~ poison + treat, data = rats))


##### exemple avec interaction, nous étudions le jeu de données warpbreaks (dataset in R)
#Le nombre de ruptures de fil pendant le tissage
#n=54,  3 variables: breaks wool (factor: A or B);tension (factor: L,M, H)
library(datasets)
data(warpbreaks)
?warpbreaks
dim(warpbreaks)
head(warpbreaks)
par(mfrow = c(1, 2))
with(warpbreaks, interaction.plot(wool, tension, breaks, lwd = 2, col = 2:4))
with(warpbreaks, interaction.plot(tension, wool, breaks, lwd = 2, col = 2:3))
## ==>graphique montre assez clairement que les facteurs de laine et de tension interagissent.

summary(aov(bresuaks ~ wool * tension, data = warpbreaks))
###===>the interaction is significant, so we use the interaction model here.
#.....Học viên tiếp tục hoàn thành phần ANOVA 2 nhân tố , kiểm định cảc gải định của mô hinh và phân tích bội nếu cần
