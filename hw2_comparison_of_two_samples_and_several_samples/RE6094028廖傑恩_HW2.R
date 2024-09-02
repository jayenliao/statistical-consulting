# title: "統計諮詢 - 作業2"
# subtitle: "國立成功大學統計學系暨數據科學研究所"
# author: Jay Liao 廖傑恩 re6094028@gs.ncku.edu.tw

library(HH)
library(dplyr)
library(rstatix)
library(jtools)
library(ggplot2)

# Exercise 6.5
data(patient)

## Exercise 6.5 - (a)

patient$log.surv.time <- log(patient$surv.time)
par(mfcol = c(1, 2))
boxplot(patient$surv.time, main = 'Boxplot of Survival Time (days)')
boxplot(patient$log.surv.time, main = 'Boxplot of Logged\nSurvival Time')

patient %>% dplyr::select(surv.time, log.surv.time) %>% stack() %>%
  boxplot(values ~ ind, data = ., xlab = '', main = 'Boxplot of Survival Time and Logged Survival Time')

boxplot(data = patient, surv.time ~ organ, ylab = 'Logged survival time (days)',
        main = 'Boxplots of Survival Time (days)')
boxplot(data = patient, log(surv.time) ~ organ, ylab = 'Logged survival time (days)',
        main = 'Boxplots of Logged Survival Time')

par(pty = 's', mfcol= c(1, 2))
qqnorm(patient$surv.time, main = 'Normal Q-Q Plot of\nSurvial Time (Y)')
qqline(patient$surv.time, col = 'red')
qqnorm(log(patient$surv.time), main = 'Normal Q-Q Plot of\nLogged Survial Time (Y\')', ylab = 'Logged Sample Quantile')
qqline(log(patient$surv.time), col = 'red')

shapiro_test_origin <- shapiro.test(patient$surv.time)
shapiro_test_log <- shapiro.test(patient$log.surv.time)
round(shapiro_test_log$statistic, 4)
round(shapiro_test_log$p.value, 4)

## Exercise 6.5 - (b)

levene_test <- lawstat::levene.test(log(patient$surv.time), patient$organ) 
round(levene_test$statistic, 4)
round(levene_test$p.value, 4)

bartlett_test <- bartlett.test(log(surv.time) ~ organ, data = patient)
round(bartlett_test$statistic, 4)
round(bartlett_test$p.value, 4)

model_1 <- aov(log(surv.time) ~ organ, data = patient)
anova_tb <- anova_summary(model_1)

shapiro_test_ <- shapiro.test(model_1$residuals)
round(shapiro_test_$statistic, 4)
round(shapiro_test_$p.value, 4)

df <- tukey_hsd(model_1)
df <- df %>% dplyr::select(group1, group2, estimate, conf.low, conf.high, p.adj, p.adj.signif) %>%
  mutate(estimate = round(estimate, 4), conf.low = round(conf.low,4), conf.high = round(conf.high,4), p.adj = round(p.adj,4))
colnames(df) <- c('組別1', '組別2', 'Diff', '95%CI下界', '95%CI上界', 'p.adj', '是否顯著')
df$是否顯著[df$是否顯著=='**'] <- '顯著'
df$是否顯著[df$是否顯著=='*'] <- '顯著'
df$是否顯著[df$是否顯著=='ns'] <- '不顯著'

knitr::kable(df)
hsd_1 <- TukeyHSD(model_1)
par(mar = c(5.1, 8.1, 4.1, 1))
plot(hsd_1, las=1 , col = 'blue')


# Exercise 6.9

data(blood, package = 'HH')
boxplot(times ~ diets, data = blood, ylab = 'Times (seconds)',
        main = 'Box Plots of Coagulation Times of blood\nDrawn from Animals Fed Different Diets')

par(pty = 's')
qqnorm(blood$times)
qqline(blood$times, col = 'red')

shapiro_test_blood <- shapiro.test(blood$times)
levene_test_blood <- levene.test(blood$times, blood$diets) 
bartlett_test_blood <- bartlett.test(times ~ diets, data = blood)

model_2 <- aov(times ~ diets, data = blood)
anova_tb <- anova_summary(model_2)
shapiro_test_blood_ <- shapiro.test(model_2$residuals)

df <- tukey_hsd(model_2)
df <- df %>% dplyr::select(group1, group2, estimate, conf.low, conf.high, p.adj, p.adj.signif) %>%
  mutate(estimate = round(estimate, 4), conf.low = round(conf.low,4), conf.high = round(conf.high,4), p.adj = round(p.adj,4))
colnames(df) <- c('組別1', '組別2', 'Diff', '95%CI下界', '95%CI上界', 'p.adj', '是否顯著')
df$是否顯著[df$是否顯著=='***'] <- '顯著'
df$是否顯著[df$是否顯著=='**'] <- '顯著'
df$是否顯著[df$是否顯著=='*'] <- '顯著'
df$是否顯著[df$是否顯著=='ns'] <- '不顯著'

knitr::kable(df)
hsd_2 <- TukeyHSD(model_2)
#par(mar = c(5.1, .1, 4.1, 1))
plot(hsd_2, las=2 , col = 'blue')


# Exercise 8.2
data(lake, package = 'HH')

par(mfcol = c(1, 2))
boxplot(lake$level, main = 'Box Plot of The Relative Level\nof Lake Victoria Nyanza')
boxplot(lake$sunspots, main = 'Box Plot of Sunspots')

model_3 <- lm(level ~ sunspots, data = lake)
sigma_2_hat <- sum(model_3$residuals^2) / (length(model_3$residuals)-2)

model_3_tb <- summ(model_3, confint = TRUE)
df <- model_3_tb$coeftable %>% as.data.frame() %>% round(., 4) 
colnames(df) <- c('估計值', '95%CI下界', '95%CI上界', 't檢定統計量', 'p值')
rownames(df) <- c('截距 (beta_0)', 'sunspots (beta_1)')
CI <- paste0('[', df$`95%CI下界`[2], ', ', df$`95%CI上界`[2], ']')
knitr::kable(df)

plot(lake$sunspots, lake$level, pch = 19, xlab = 'Sunspots', ylab = 'Lake level',
     main = 'Scatter Plot of the Number of\nSunspots and Lake Level')

par(mfcol = c(1, 2))
plot(model_3, which = c(1, 2))

shapiro_test_lake_ <- shapiro.test(model_3$residuals)
dwtest_ <- lmtest::dwtest(model_3)
lake$group <- as.factor(lake$sunspots >= median(lake$sunspots))
lake$residuals <- model_3$residuals
bf <- onewaytests::bf.test(residuals ~ group, data = lake, verbose = FALSE)

R_2 <- (var(lake$level) - sigma_2_hat) / var(lake$level)

ggplot(aes(x = sunspots, y = level), data = lake) +
  geom_point() +
  geom_abline(slope = model_3$coefficients[2],
              intercept = model_3$coefficients[1], col = 'red') +
  labs(x = 'No. of Sunspots', y = 'Lake Level') +
  ggtitle('Scatter Plot of No. of Sunspots and Lake Level\nwith The Linear Regression Line') +
  theme_bw()
