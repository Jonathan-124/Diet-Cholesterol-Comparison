# Setting Working Directory
setwd("~/Documents/Gothenburg University/Courses/MSA680 - Data Science for Bioscience/Labs/lab1")

#Load libraries
library(tidyverse)
library(scales)
library(ggplot2)
library(car)
library(emmeans)

######
#2.1.1
######

#Read in the data set, reordering to set PreChol before PostChol
diet <- read.table(file="diet.txt",header=T)
diet <- diet[,c(1,3,2)]

# Changing diet to categorical variable
diet$Diet <- factor(diet$Diet, levels=c(1,2,3,4))

# Creating a summary stats table
diet %>% group_by(Diet) %>% summarise(N=n(),
                                             PreCholMean=mean(PreChol),
                                             PostCholMean=mean(PostChol),
                                             PreCholMeanSD=sd(PreChol),
                                             PostCholMeanSD=sd(PostChol),
                                             MeanCholChange=mean(PostChol)-mean(PreChol))

# Creating a new df to visualize as grouped boxplot by time of diet
diet_new <- pivot_longer(diet, cols = !Diet, names_to= 'Time', values_to= 'Cholesterol')

# Refactoring so that "PreChol" comes before "PostChol" on the graph
diet_new$Time <- factor(diet_new$Time, levels=c("PreChol", "PostChol"))

# Boxplot visualization
ggplot(diet_new, aes(x=Diet, y=Cholesterol, fill=Time)) + 
  geom_boxplot()

######
#2.2.1
######
oneway_postchol <- aov(PostChol ~ Diet, data = diet)
summary(oneway_postchol)
# p=0.00538, we thus reject the null hypothesis that the post-treatment cholesterol levels for the four different
# treatments are the same

######
#2.2.2
######

# baseline level is Diet1, lm probabilities give pairwise t-tests on whether diet2, 3, 4 are significantly
# different from diet1
# beta estimates are the "change" in cholesterol from baseline
postchol_lm <- lm(PostChol ~ Diet, data = diet)
summary(postchol_lm)

# this gives the exact same results as the aov function
anova(postchol_lm)

######
#2.2.3
######
# calculates least square means for each diet, as well as their confidence intervals
emmeans(postchol_lm, ~ Diet)

######
#2.2.4
######
# Can be recovered by estimate +- t(a/2, n-4)*SE for the coefficients

######
#2.3.1
######
# Equal slopes model
model1 <- aov(PostChol ~ PreChol + Diet, data = diet)
summary(model1)

######
#2.3.2, 2.3.3
######
model1_lm <- lm(PostChol ~ PreChol + Diet, data = diet)
summary(model1_lm)

# this gives the exact same results as the aov function
anova(model1_lm)

# Parameters for linear regression
coefs <- coef(model1_lm)
slopes <- rep(coefs[2], 4)
intercepts <- c(coefs[1], coefs[1] + coefs[3], coefs[1] + coefs[4], coefs[1] + coefs[5])
lines_df <- data.frame(Diet = as.factor(c(1,2,3,4)), slope = slopes, intercepts = intercepts)

# Drawing the plot separated by diet
ggplot(diet,aes(x=PreChol, y=PostChol))+
  geom_point(aes(color=Diet))+
  facet_wrap(~Diet)+
  geom_abline(data = lines_df, aes(intercept = intercepts, slope = slopes, color=Diet))

# Drawing the plot together
ggplot(diet,aes(x=PreChol, y=PostChol))+
  geom_point(aes(color=Diet))+
  geom_abline(data = lines_df, aes(intercept = intercepts, slope = slopes, color=Diet))

# PreChol emmeans?
as.data.frame(emmeans(model1_lm, specs="Diet"))

###
# Need to ask how to find:
# 1. lsmeans for corrected PostChol values
# 2. lsmeans for corrected PreChol values, and 95% CI
# 3. What do they mean?
# 4. Why do they differ from ANOVA results, if they do?
###

######
#2.4.1
######
# Unequal slopes model
model2 <- aov(PostChol ~ PreChol * Diet, data = diet)
summary(model2)

######
#2.4.2, 2.4.3
######
model2_lm <- lm(PostChol ~ PreChol * Diet, data = diet)
summary(model2_lm)

# this gives the exact same results as the aov function
anova(model2_lm)

# Parameters for linear regression
coefs <- coef(model2_lm)
slopes <- c(coefs[2], coefs[2] + coefs[6], coefs[2] + coefs[7], coefs[2] + coefs[8])
intercepts <- c(coefs[1], coefs[1] + coefs[3], coefs[1] + coefs[4], coefs[1] + coefs[5])
lines_df <- data.frame(Diet = as.factor(c(1,2,3,4)), slope = slopes, intercepts = intercepts)

# Drawing the plot separated by diet
ggplot(diet,aes(x=PreChol, y=PostChol))+
  geom_point(aes(color=Diet))+
  facet_wrap(~Diet)+
  geom_abline(data = lines_df, aes(intercept = intercepts, slope = slopes, color=Diet))

as.data.frame(emmeans(model2_lm, specs="Diet"))

#####
#2.4.5
#####
# New model with intercept being the mean prediet cholesterol
meanPreChol <- mean(diet$PreChol)
diet <- mutate(diet, mPreChol = PreChol - mean(PreChol))

model3 <- aov(PostChol ~ mPreChol * Diet, data = diet)
summary(model3)

model3_lm <- lm(PostChol ~ mPreChol * Diet, data = diet)
summary(model3_lm)

anova(model3_lm)

# Parameters for linear regression
coefs <- coef(model3_lm)
slopes <- c(coefs[2], coefs[2] + coefs[6], coefs[2] + coefs[7], coefs[2] + coefs[8])
intercepts <- c(coefs[1], coefs[1] + coefs[3], coefs[1] + coefs[4], coefs[1] + coefs[5])
lines_df <- data.frame(Diet = as.factor(c(1,2,3,4)), slope = slopes, intercepts = intercepts)

# Drawing the plot separated by diet
ggplot(diet,aes(x=mPreChol, y=PostChol))+
  geom_point(aes(color=Diet))+
  facet_wrap(~Diet)+
  geom_abline(data = lines_df, aes(intercept = intercepts, slope = slopes, color=Diet))

# Pretty much the same, just recentering so the intercept is more interpretable as the mean pre-diet level..

as.data.frame(emmeans(model3_lm, specs="Diet"))

#####
#2.4.6
#####
#Finding lsmeans and 95% CI for the previous model in 2.4.5

# New data for prediction
low_prechol_data <- data.frame(Diet = as.factor(c(1,2,3,4)), mPreChol = rep(190 - meanPreChol, 4))
high_prechol_data <- data.frame(Diet = as.factor(c(1,2,3,4)), mPreChol = rep(250 - meanPreChol, 4))

# Get confidence interval for the response for the four different diets at low and high prechol levels
predict(model3_lm, newdata = low_prechol_data, interval = "confidence", level = 0.95)
predict(model3_lm, newdata = high_prechol_data, interval = "confidence", level = 0.95)
