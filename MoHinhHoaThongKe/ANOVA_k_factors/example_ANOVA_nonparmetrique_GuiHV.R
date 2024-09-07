#ANOVA non paramétrique (test de Kruskal Wallis)
myeloma <- read.delim("//Users/Jade/Desktop/GiaoAn/DH_KHTN/HKII_2023_2024/CH/MHHTK/Cours_GV/ANOVA/myeloma.txt")
dim(myeloma)
head(myeloma)
str(myeloma)
#Visualisation des données
library(ggplot2)
ggplot(myeloma ,aes(x=group, y=DEPDC1, colour=group,fill=group))+
  geom_jitter(width=0.25)+
  geom_boxplot(alpha=0.25, outlier.alpha=0) +
  stat_summary(fun.y=mean, colour="black", geom="point",
               shape=18, size=3,show.legend = FALSE)+
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.text = element_text(angle=30, hjust=1, vjust=1)) 

#Ajustement de l'ANOVA paramétrique (uniquement pour vérifier que les conditions ne sont pas satisfaites)
myeloma_an <- lm(DEPDC1~group, data=myeloma) 
#Evaluation des hypothèses
 #1.Evaluation de l'indépendance
     library(car)
     durbinWatsonTest(myeloma_an)
#2. Evaluation de l'hypothèse de normalité des résidus
     plot(myeloma_an,2)      
     shapiro.test(residuals(myeloma_an))
#3. Evaluation de l'hypothèse d'homogénéité des résidus 
     plot(myeloma_an,3 )
     bartlett.test(residuals(myeloma_an)~myeloma$group)
     #===>Dans cette situation de rejet des hypothèses de normalité et d’homogénéité des résidus, 
     ########il est possible de comparer les moyennes à l’aide d’une ANOVA non paramétrique, 
     ######## ou test de Kruskal-Wallis
#Ajustement de l'ANOVA non paramétrique (test de Kruskal- Wallis)
     kruskal.test(DEPDC1 ~group, data=myeloma) #l’hypothèse de l’égalité des moyennes est rejetée. 
     #### ====> les moyennes des sept groupes sont globalement différentes.
     ####nécessaire de réaliser des comparaisons multiples
     ## tests de Wilcoxon (tests non-paramétriques): pairwise.wilcox.test
     #Comparaisons multiples : toutes les comparaisons deux à deux (procédure de Tukey)
    (pp <- pairwise.wilcox.test(myeloma$DEPDC1, myeloma$group,p.adjust.method ="holm" ))
      
     ###Comparaisons multiples : toutes les comparaisons au témoin (procédure de Dunnett)
      ## La méthode la plus simple consiste à :
           #a.Réaliser toutes les comparaisons deux à deux, sans ajustement
           #b.sélectionner les pvalues correspondant à toutes les comparaisons avec le groupe contrôle
           #c.ajuster ces p-values
     # réalisation de toutes les comparaisons deux à deux sans ajuster les p-values
     pp_all <- pairwise.wilcox.test(myeloma$DEPDC1, myeloma$group,p.adjust.method ="none" )
     pp_all
     #récupération des pvalues des tests de wilcoxon entre la moyenne du groupe contrôle et toutes les autres moyennes.
     pp_dunn <- pp_all[[3]][6,]
     pp_dunn
     # ajustement de ces p-values par la méthode de Holm
      pp_dunn_adjust <- p.adjust(pp_dunn,method="holm") 
     pp_dunn_adjust
     
     ##### ANOVA par tests de permutation
     #Lorsque les hypothèses de normalité et/ou d’homogénéité sont rejetées, une autre solution envisageable est le recours à un test de permutation.
     library(lmPerm)
     set.seed(777)
     myeloma_an_perm <- aovp(DEPDC1~group,perm="Prob", data=myeloma)
     summary(myeloma_an_perm)
     #====>ANOVA par permutation met en évidence au moins une différence significative entre les moyennes des différents groupes.

     