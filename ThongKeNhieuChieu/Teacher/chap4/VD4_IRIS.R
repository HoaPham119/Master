library(tidyverse)# data manipulation and visualization
library(ggpubr)#easy pipe-friendly statistical analyses
library(rstatix)#easy pipe-friendly statistical analyses
library(car) # MANOVA analyses
library(broom)# printing a nice summary of statistical tests as data frames
##e’ll use the built-in R dataset iris. Select columns of interest:

iris2 <- iris %>%
  select(Sepal.Length, Petal.Length, Species) %>%
  add_column(id = 1:nrow(iris), .before = 1)
head(iris2)
dim(iris2)
##Visualization
ggboxplot(
  iris2, x = "Species", y = c("Sepal.Length", "Petal.Length"), 
  merge = TRUE, palette = "jco"
)
##Summary statistics
iris2 %>%
  group_by(Species) %>%
  get_summary_stats(Sepal.Length, Petal.Length, type = "mean_sd")

##Multivariate normality. mshapiro_test( ) (package: rstatix )
##Check sample size assumption
iris2 %>%
  group_by(Species) %>%
  summarise(N = n())
##Identify univariate outliers :identify_outliers() [rstatix package].
iris2 %>%
  group_by(Species) %>%
  identify_outliers(Sepal.Length)
iris2 %>%
  group_by(Species) %>%
  identify_outliers(Petal.Length)
#===>There were no univariate extreme outliers in the Sepal.Length and Petal.length variable, as assessed by box plot methods.

##Detect multivariate outliers: mahalanobis_distance() [rstatix package]
### Compute distance by groups and filter outliers
# Use -id to omit the id column in the computation
iris2 %>%group_by(Species) %>%mahalanobis_distance(-id) %>%
filter(is.outlier == TRUE) %>% as.data.frame()
#===>There were no multivariate outliers in the data, 
#===> as assessed by Mahalanobis distance (p > 0.001).

##Check univariate normality assumption : Shapiro-Wilk test
iris2 %>%
  group_by(Species) %>%
  shapiro_test(Sepal.Length, Petal.Length) %>%
  arrange(variable)
##===>Sepal.Length and Petal.length were normally distributed for each Species groups,
##==>as assessed by Shapiro-Wilk’s test (p > 0.05).
##Q Q plot
# QQ plot of Sepal.Length ( đài hoa)
ggqqplot(iris2, "Sepal.Length", facet.by = "Species",
         ylab = "Sepal Length", ggtheme = theme_bw())

# QQ plot of Petal.Length (cánh hoa)
ggqqplot(iris2, "Petal.Length", facet.by = "Species",
         ylab = "Petal Length", ggtheme = theme_bw())

####Multivariate normality. mshapiro_test( ) (package: rstatix )
##Multivariate normality
iris2 %>%
  select(Sepal.Length, Petal.Length) %>%
  mshapiro_test()
#===>The test is not significant (p > 0.05), so we can assume multivariate normality.

##dentify multicollinearity : cor_test() [rstatix package]
#Ideally the correlation between the outcome variables should be moderate, not too high. A correlation above 0.9 is an indication of multicollinearity,
#which is problematic for MANOVA.
iris2 %>% cor_test(Sepal.Length, Petal.Length)
##===>There was no multicollinearity, as assessed by Pearson correlation (r = 0.87, p < 0.0001).

##Check linearity assumption
# Create a scatterplot matrix by group
library(GGally)
results <- iris2 %>%
  select(Sepal.Length, Petal.Length, Species) %>%
  group_by(Species) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results
# Show the plots
results$plots
#===>There was a linear relationship between Sepal.Length and Petal.Length in each Species group, as assessed by scatter plot.

##Check the homogeneity of covariances assumption
#using Box’s M-test implemented in the rstatix package
box_m(iris2[, c("Sepal.Length", "Petal.Length")], iris2$Species)
#===>The test is statistically significant (i.e., p < 0.001), so the data have violated the assumption of homogeneity of variance-covariance matrices.
## Note that, if you have balanced design (i.e., groups with similar sizes), you don’t need to worry too much about violation of the homogeneity of variances-covariance matrices and you can continue your analysis.
##However, having an unbalanced design is problematic. Possible solutions include: 1) transforming the dependent variables; 2) running the test anyway, but using Pillai’s multivariate statistic instead of Wilks’ statistic.

##Check the homogneity of variance assumption: levene_test() [rstatix package].
iris2 %>% 
  gather(key = "variable", value = "value", Sepal.Length, Petal.Length) %>%
  group_by(variable) %>%
  levene_test(value ~ Species)
##===>The Levene’s test is significant (p < 0.05), so there was no homogeneity of variances.
#Note that, if you do not have homogeneity of variances, you can try to transform the outcome (dependent) variable to correct for the unequal variances.
#Alternatively, you can continue, but accept a lower level of statistical significance (alpha level) for your MANOVA result. Additionally, any follow-up univariate ANOVAs will need to be corrected for this violation (i.e., you will need to use different post-hoc tests).

#Computation : Manova() function [car package].

mod <- Manova(lm(cbind(Sepal.Length, Petal.Length) ~ Species, data=iris2 ))
summary(mod)
###
model <- lm(cbind(Sepal.Length, Petal.Length) ~ Species, iris2)
Manova(model, test.statistic = "Pillai")
#===> There was a statistically significant difference between the Species on the combined dependent variables (Sepal.Length and Petal.Length), F(4, 294) = 71.829, p < 0.0001.

##Post-hoc tests
library(rstatix)
# Group the data by variable
grouped.data <- iris2 %>%
  gather(key = "variable", value = "value", Sepal.Length, Petal.Length) %>%
  group_by(variable)

# Do welch one way anova test
grouped.data %>% welch_anova_test(value ~ Species)
# or do Kruskal-Wallis test
grouped.data %>% kruskal_test(value ~ Species)
# or use aov()
grouped.data %>% anova_test(value ~ Species)
#===>There was a statistically significant difference in Sepal.Length (F(2, 147) = 119, p < 0.0001 )
#and Petal.Length (F(2, 147) = 1180, p < 0.0001 ) between iris Species.

##Compute multiple pairwise comparisons
#The R functions tukey_hsd() [rstatix package] can be used to compute Tukey post-hoc tests if the homogeneity of variance assumption is met.
#If you had violated the assumption of homogeneity of variances, as in our example, you might prefer to run a Games-Howell post-hoc test. It’s also possible to use the function pairwise_t_test() [rstatix] with the option pool.sd = FALSE and var.equal = FALSE .

pwc <- iris2 %>%
  gather(key = "variables", value = "value", Sepal.Length, Petal.Length) %>%
  group_by(variables) %>%
  games_howell_test(value ~ Species) %>%
  select(-estimate, -conf.low, -conf.high) # Remove details
pwc
#===>All pairwise comparisons were significant for each of the outcome variable (Sepal.Length and Petal.Length).

#####Report
#A one-way multivariate analysis of variance was performed to determine the effect of iris Species on Sepal.Length and Petal.Length. There are three different species: setosa, versicolor and virginica.

#There was a statistically significant difference between the Species on the combined dependent variables (Sepal.Length and Petal.Length), F(4, 294) = 71.829, p < 0.0001.

#Follow-up univariate ANOVAs, using a Bonferroni adjusted alpha level of 0.025, showed that there was a statistically significant difference in Sepal.Length (F(2, 147) = 119, p < 0.0001 ) and Petal.Length (F(2, 147) = 1180, p < 0.0001 ) between iris Species.

#All pairwise comparisons between groups were significant for each of the outcome variable (Sepal.Length and Petal.Length).
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "Species")
test.label <- create_test_label(
  description = "MANOVA", statistic.text = quote(italic("F")),
  statistic = 71.83, p= "<0.0001", parameter = "4,294",
  type = "expression", detailed = TRUE
)
ggboxplot(
  iris2, x = "Species", y = c("Sepal.Length", "Petal.Length"), 
  merge = TRUE, palette = "jco"
) + 
  stat_pvalue_manual(
    pwc, hide.ns = TRUE, tip.length = 0, 
    step.increase = 0.1, step.group.by = "variables",
    color = "variables"
  ) +
  labs(
    subtitle = test.label,
    caption = get_pwc_label(pwc, type = "expression")
  )

