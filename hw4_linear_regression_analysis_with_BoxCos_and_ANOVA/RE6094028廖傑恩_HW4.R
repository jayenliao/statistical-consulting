# title: "統計諮詢 - 作業4"
# subtitle: "國立成功大學統計學系暨數據科學研究所"
# author: Jay Liao 廖傑恩 re6094028@gs.ncku.edu.tw

library(HH)
library(dplyr)
library(car)
library(jtools)
library(ggplot2)
library(GGally)
library(EnvStats)
library(onewaytests)
library(lmtest)
library(rstatix)

# Exercise 11.3

data("concord")
df_column <- data.frame(變項名稱 = colnames(concord),
                        變項意義 = c(
                          '1981年家戶用水量（單位：立方英尺）',
                          '1980年家戶用水量（單位：立方英尺）',
                          '1981年家戶收入（單位：千元）',
                          '戶長教育程度（單位：年）',
                          '1981年夏季時家戶人數',
                          '戶長是否退休，是=1，否=0'),
                        變項類型 = c(rep('連續', 5), '類別'))
knitr::kable(df_column)

concord %>% mutate(retired = as.factor(retired)) %>%
  ggpairs(., aes(colour=retired)) +
  theme(axis.text.x = element_text(angle = 45, hjust = .7))


model_full <- lm(water81 ~ water80 + income + educat + peop81 + retired, data = concord)
viff <- vif(model_full)
df_vif <- data.frame(變項 = names(viff), VIF值 = viff)
rownames(df_vif) <- NULL
knitr::kable(df_vif)

model_null <- lm(water81 ~ 1, data = concord)
step_for <- step(model_null, scope = list(upper = model_full), direction = 'for', trace = 0)
df <- step_for$anova[,c(1,6)]
df <- df %>% mutate(步驟編號 = 1:nrow(df)) %>%
  dplyr::select(步驟編號, Step, AIC)
df$Step[1] <- '不考慮任何變項的原始模型'
colnames(df) <- c('步驟編號', '步驟', '步驟完成後的模型AIC')
knitr::kable(df)

par(mfcol = c(2, 2))
plot(step_for)
par(mfcol = c(1, 1))

par(pty = 's')
qqnorm(step_for$residuals, ylab = 'Residuals',
       main = 'Normal Q-Q Plot of Residuals')
qqline(step_for$residuals, col = 'red')
sw_test <- shapiro.test(step_for$residuals)

boxcox_operator <- function(variable_name, dta=concord) {
  boxcox_ <- MASS::boxcox(dta[,variable_name] ~ 1, lambda = seq(-2, 2, .01))
  title(paste0('Plot of Box-Cox Transformation\nfor ', variable_name))
  tuned_lambda <- boxcox_$x[which.max(boxcox_$y)]
  return(tuned_lambda)
}

var_transform <- c('water81', 'water80', 'income', 'peop81')
var_transform_s <- c('1981年的家戶用水量', '1980年的家戶用水量', '1981年的家戶收入', '1981年夏季的家戶人數')

par(mfcol = c(2, 2))
lambdas <- sapply(var_transform, boxcox_operator)
knitr::kable(data.frame(lambdas))

boxcox_ <- MASS::boxcox(model_full, lambda = seq(-2, 2, .0001), plotit = FALSE)
tuned_lambda <- boxcox_$x[which.max(boxcox_$y)]
plot(boxcox_$x, boxcox_$y, type='l', main = 'Plot of Box-Cox Transformation',
     xlab = expression(lambda), ylab = 'Log-Likelihood')
abline(h = max(boxcox_$y), lty = 2)
abline(v = tuned_lambda, lty = 2)
concord_new <- concord %>%
  mutate(water81_boxcox = (water81^tuned_lambda - 1) / tuned_lambda,
         water80_boxcox = (water80^tuned_lambda - 1) / tuned_lambda,
         income_boxcox = (income^tuned_lambda - 1) / tuned_lambda,
         peop81_boxcox = (peop81^tuned_lambda - 1) / tuned_lambda)

boxcox_ <- MASS::boxcox(concord$water81 ~ 1, lambda = seq(-2, 2, .0001), plotit = FALSE)
tuned_lambda <- boxcox_$x[which.max(boxcox_$y)]
plot(boxcox_$x, boxcox_$y, type='l', main = 'Plot of Box-Cox Transformation',
     xlab = expression(lambda), ylab = 'Log-Likelihood')
abline(h = max(boxcox_$y), lty = 2)
abline(v = tuned_lambda, lty = 2)
concord_new <- concord %>%
  mutate(water81_boxcox = (water81^tuned_lambda - 1) / tuned_lambda)

boxcox_transform__ <- function(dta, variable_name, lambda_) {
  return(as.numeric((dta[,variable_name]^lambda_ - 1) / lambda_))
}
 
concord_new <- sapply(var_transform, function(v) boxcox_transform__(concord, v, lambdas[v])) %>%
  as.data.frame() %>% cbind(concord %>% dplyr::select(!var_transform))

plt_distri_ <- function(dta1, dta2, v, vs) {
  p1 <- qplot(dta1[,v], geom = 'blank') + 
    geom_line(aes(y = ..density..), stat = 'density', colour = 'navyblue') + 
    geom_histogram(aes(y = ..density..), alpha = .5) +
    ggtitle(paste0('Concord地區', vs, '\n密度圖與直方圖')) +
    labs(x = vs, y = '密度') + 
    theme_bw() +
    theme(text = element_text(family = '蘋方-繁 標準體'))
  p2 <- qplot(dta2[,v], geom = 'blank') + 
    geom_line(aes(y = ..density..), stat = 'density', colour = 'navyblue') + 
    geom_histogram(aes(y = ..density..), alpha = .5) +
    ggtitle(paste0('Concord地區', vs, '\n密度圖與直方圖（經Box-Cox轉換）')) +
    labs(x = paste0('經Box-Cox轉換的', vs), y = '密度') + 
    theme_bw() +
    theme(text = element_text(family = '蘋方-繁 標準體'))
  return(list(p1, p2))
}

names(var_transform_s) <- var_transform
p <- plt_distri_(concord, concord_new, var_transform[1], var_transform_s[1])
p <- plt_distri_(concord, concord_new, var_transform[2], var_transform_s[2])
p <- plt_distri_(concord, concord_new, var_transform[3], var_transform_s[3])
p <- plt_distri_(concord, concord_new, var_transform[4], var_transform_s[4])

removed_rows <- c(80, 85, 94, 118, 124, 125)
#model_full_tr <- lm(water81 ~ water80 + income + educat + peop81 + retired, data = concord_new[-removed_rows,])
model_full_tr <- lm(water81 ~ water80 + income + educat + peop81 + retired, data = concord_new)
viff <- vif(model_full_tr)
df_vif <- data.frame(變項 = names(viff), VIF值 = viff)
rownames(df_vif) <- NULL
knitr::kable(df_vif)

#model_null_tr <- lm(water81 ~ 1, data = concord_new[-removed_rows,])
model_null_tr <- lm(water81 ~ 1, data = concord_new)
step_for <- step(model_null_tr, scope = list(upper = model_full_tr), direction = 'for', trace = 0)
df <- step_for$anova[,c(1,6)] 
df <- df %>% mutate(步驟編號 = 1:nrow(df)) %>%
  dplyr::select(步驟編號, Step, AIC)
df$Step[1] <- '不考慮任何變項的原始模型'
colnames(df) <- c('步驟編號', '步驟', '步驟完成後的模型AIC')
knitr::kable(df)

par(mfcol = c(2, 2))
plot(step_for)

sw_test <- shapiro.test(step_for$residuals)
ks_test <- ks.test(step_for$residuals, rnorm(100))
bf_ <- concord_new %>%
  mutate(group = as.factor(water81 > median(water81)), residuals_ = step_for$residuals) %>%
  bf.test(residuals_ ~ group, data = ., verbose = FALSE)
dwtest_ <- dwtest(step_for)

model_aov_tb <- anova_summary(aov(water81 ~ water80*retired, data = concord))
model_aov_tb_new <- anova_summary(aov(water81 ~ water80*retired, data = concord_new))
```

我們以經由Box-Cox轉換的資料建立的線性迴歸模型仍未通過殘差常態檢驗，也未通過殘差變異數同質性檢驗，有可能是極端值造成影響。此外，先前模型並未納入「戶長是否退休（`retired`）」這個變項，然而此變項與其他變項可能對依變項有交互作用，因此我們以考慮交互作用的變異數分析檢驗。結果顯示`retired`與1980年家戶用水量（`water80`）（$F=$,`r round(model_aov_tb$F[3])`$p=$`r round(model_aov_tb$p[3])`）與經由Box-Cox轉換的家戶用水量有交互作用（$F=$,`r round(model_aov_tb_new$F[3])`$p=$`r round(model_aov_tb_new$p[3])`）。而我們使用轉換後的資料時，在逐步選擇時排除了`retired`，顯示有可能要將`retired`各類別拆開，分別建立迴歸模型。

### 定義變數與模型

- 定義變數

    1. 令$Y'$為經Box-Cox轉換的`water81`（1981年家戶用水量）
    2. 令$X'_{1}$為經Box-Cox轉換的`water80`（1980年家戶用水量）
    3. 令$X'_{2}$為經Box-Cox轉換的`income`（1981年家戶收入）
    4. 令$X_{3}$為`educat`（戶長教育程度）
    5. 令$X'_{4}$為`peop81`（1981年夏季時家戶人數）
    6. 令$X_{5}$為`retired`（戶長是否退休），是=1，否=0。
    
- 定義模型

    - 模型A - 針對戶長非退休之家戶建立的模型（$X_{i5}=0$）：$Y'_i = \beta_{0.0}' + \beta_{1.0}'X'_{i1} + \beta_{2.0}'X'_{i2} + \beta_{3.0}'X_{i3} + \beta_{4.0}'X_{i4} + \epsilon_i',\ \forall i = 1, ..., 350$
    
    - 模型B - 針對戶長已退休之家戶建立的模型（$X_{i5}=1$）：$Y'_j = \beta_{0.1}' + \beta_{1.1}'X'_{j1} + \beta_{2.1}'X'_{j2} + \beta_{3.1}'X_{j3} + \beta_{4.1}'X_{j4} + \epsilon_j',\ \forall j = 1, ..., 146$

其中$\epsilon_i' \overset{iid} \sim N(0,{\sigma_i'}^2)$，$\epsilon_j' \overset{iid} \sim N(0,{\sigma_j'}^2)$。

#### 檢查共線性

在配適模型前，我們同樣以VIF值來檢查是否共線性存在。我們4個獨變項的VIF值如下表所示，在兩模型中均不具共線性。

```{r, comment='', message=FALSE, warning=FALSE, echo=FALSE}
#model_full_0 <- lm(water81 ~ water80 + income + educat + peop81,
#                   data = concord_new[-removed_rows,][concord_new[-removed_rows,]$retired == 0,])
#model_full_1 <- lm(water81 ~ water80 + income + educat + peop81,
#                   data = concord_new[-removed_rows,][concord_new[-removed_rows,]$retired == 1,])
model_full_0 <- lm(water81 ~ water80 + income + educat + peop81,
                   data = concord_new[concord_new$retired == 0,])
model_full_1 <- lm(water81 ~ water80 + income + educat + peop81,
                   data = concord_new[concord_new$retired == 1,])
viff <- vif(model_full_0)
df_vif <- data.frame(變項 = names(viff),
                     模型A之VIF值 = viff,
                     模型B之VIF值 = vif(model_full_1))
rownames(df_vif) <- NULL
knitr::kable(df_vif)
```

#### 逐步迴歸分析

我們同樣使用向前選擇法，逐步納入對模型貢獻程度最高的變項，直到模型配適度不再改善。我們同樣以AIC作為模型配適度指標，$AIC$越大表示模型配適越差。下兩表分別列出了我們在模型A與模型B進行逐步選擇的過程，其中「$+X$」意味著在上一步驟的模型中再納入變項$X$。

```{r, comment='', message=FALSE, warning=FALSE, echo=FALSE}
#model_null_0 <- lm(water81 ~ 1, data = concord_new[-removed_rows,][concord_new[-removed_rows,]$retired == 0,])
#model_null_1 <- lm(water81 ~ 1, data = concord_new[-removed_rows,][concord_new[-removed_rows,]$retired == 1,])
model_null_0 <- lm(water81 ~ 1, data = concord_new[concord_new$retired == 0,])
model_null_1 <- lm(water81 ~ 1, data = concord_new[concord_new$retired == 1,])
step_for_0 <- step(model_null_0, scope = list(upper = model_full_0), direction = 'for', trace = 0)
step_for_1 <- step(model_null_1, scope = list(upper = model_full_1), direction = 'for', trace = 0)

df <- step_for_0$anova[,c(1,6)] 
df <- df %>% mutate(步驟編號 = 1:nrow(df)) %>%
  dplyr::select(步驟編號, Step, AIC)
df$Step[1] <- '不考慮任何變項的原始模型'
colnames(df) <- c('步驟編號', '步驟', '步驟完成後的模型AIC')
knitr::kable(df)

df <- step_for_1$anova[,c(1,6)] 
df <- df %>% mutate(步驟編號 = 1:nrow(df)) %>%
  dplyr::select(步驟編號, Step, AIC)
df$Step[1] <- '不考慮任何變項的原始模型'
colnames(df) <- c('步驟編號', '步驟', '步驟完成後的模型AIC')
knitr::kable(df)

sum_tb <- summ(step_for_0, confint = TRUE)
df_ <- sum_tb$coeftable %>% as.data.frame() %>% round(., 4) 
df_ <- df_[c(1,2,4,5,3),]
colnames(df_) <- c('估計值', '95CI下界', '.95CI上界', 't檢定統計量', 'p值')
S <- rownames(df_)
rownames(df_) <- paste0(c('截距', S[-1]), ' (beta_', c(0:4), '.0)')
knitr::kable(df_)

sum_tb <- summ(step_for_1, confint = TRUE)
df_ <- sum_tb$coeftable %>% as.data.frame() %>% round(., 4) 
df_ <- df_[c(1,2,4,3),]
colnames(df_) <- c('估計值', '.95CI下界', '.95CI上界', 't檢定統計量', 'p值')
S <- rownames(df_)
rownames(df_) <- paste0(c('截距', S[-1]), ' (beta_', c(0,1,2,4), ')')
knitr::kable(df_)

model_0 <- lm(water81 ~ water80 + income + peop81, data = concord_new[concord_new$retired == 0,])
par(mfcol = c(2, 2))
plot(model_0)

model_1 <- lm(water81 ~ water80 + peop81, data = concord_new[concord_new$retired == 1,])
par(mfcol = c(2, 2))
plot(model_1)

sw_test_0 <- shapiro.test(model_0$residuals)
#ks_test_0 <- ks.test(model_0$residuals, rnorm(100))
sw_test_1 <- shapiro.test(model_1$residuals)
#ks_test_1 <- ks.test(model_1$residuals, rnorm(100))

bf_0 <- concord_new %>% dplyr::filter(retired == 0) %>%
  mutate(group = as.factor(water81 > median(water81)), residuals_ = model_0$residuals) %>%
  bf.test(residuals_ ~ group, data = ., verbose = FALSE)
bf_1 <- concord_new %>% dplyr::filter(retired == 1) %>%
  mutate(group = as.factor(water81 > median(water81)), residuals_ = model_1$residuals) %>%
  bf.test(residuals_ ~ group, data = ., verbose = FALSE)

dwtest_0 <- dwtest(model_0)
dwtest_1 <- dwtest(model_1)

coef_ <- round(step_for$coefficients, 4)
coef_0 <- round(model_0$coefficients, 4)
coef_1 <- round(model_1$coefficients, 4)


# Exercise 12.6

data('skateslc')

skateslc_ <- skateslc %>% 
  mutate(Total.score = Technique + Presentation,
         Judge = as.factor(Judge))
skateslc_long  <- skateslc_ %>%
  reshape2::melt(id.var = c('Judge', 'Skater'))

library(showtext)
showtext.auto(enable = TRUE)

boxplot(value ~ Judge, data = skateslc_long,
        main = '各評審給分盒鬚圖',
        xlab = '評審編號', ylab = '分數',
        family = '蘋方-繁 標準體')

ggplot(aes(x = Judge, y = value), data = skateslc_long) + 
  geom_boxplot() +
  #facet_wrap(variable ~ .) +
  xlab('評審編號') + ylab('分數') +
  ggtitle('各評審給分盒鬚圖') +
  theme_bw() +
  theme(text = element_text(family = '蘋方-繁 標準體'),
        legend.position = 'none')

qplot(x = Skater, y = value, #col = factor(Skater),
      geom = 'boxplot', data = skateslc_long) + 
  #facet_wrap(variable ~ ., nrow = 1) +
  labs(x = '溜冰選手名字', y = '分數') +
  ggtitle('各溜冰選手得分盒鬚圖') +
  theme_bw() +
  theme(text = element_text(family = '蘋方-繁 標準體'),
        axis.text.x = element_text(angle = 45, hjust = .9),
        legend.position = 'none')

boxplot(value ~ Skater, data = skateslc_long,
        main = '各選手得分盒鬚圖',
        xlab = '溜冰選手名字', ylab = '分數',
        family = '蘋方-繁 標準體')

qplot(y= Skater, x = Judge,
      col = Total.score, size = 5,
      geom = 'point',
      data = skateslc_) + 
  scale_color_continuous(type = 'viridis') +
  labs(y = '溜冰選手名字', x = '評審編號', colour = '總分') +
  ggtitle('各評審給予各溜冰選手總分點圖') +
  theme_bw() +
  theme(text = element_text(family = '蘋方-繁 標準體')) +
  guides(size = FALSE)

leveneTest_i <- with(skateslc_, leveneTest(Total.score ~ factor(Judge)))
leveneTest_j <- with(skateslc_, leveneTest(Total.score ~ Skater))
df_levene <- data.frame(
  因子 = c('Judge', 'Skater'),
  `自由度1` = c(leveneTest_i$Df[1], leveneTest_j$Df[1]),
  `自由度2` = c(leveneTest_i$Df[2], leveneTest_j$Df[2]),
  `F檢定統計量` = round(c(leveneTest_i$`F value`[1], leveneTest_j$`F value`[1]), 4),
  p值 = round(c(leveneTest_i$`Pr(>F)`[1], leveneTest_j$`Pr(>F)`[1]), 4)
)
knitr::kable(df_levene)

model_1 <- aov(Total.score ~ Judge + Skater, data = skateslc_)
model_1_tb <- anova_summary(model_1)
df <- data.frame(
  變異來源 = c('因子（評審）', '集區（選手）', '殘差'),
  自由度 = c(8, 4, 32),
  平方和 = c(44.58, 179.56, 49.64)
)
df$均方 = df$平方和 / df$自由度
df$`F檢定統計量` = c(model_1_tb[,'F'], '')
df$p值 = c(model_1_tb[,'p'], '')
knitr::kable(df)

par(mfcol = c(2, 2))
plot(model_1)
par(mfcol = c(1, 1))

sw_test <- shapiro.test(model_1$residuals)
bartlett_i <- bartlett.test(Total.score ~ Judge, data = skateslc_)
bartlett_j <- bartlett.test(Total.score ~ Skater, data = skateslc_)
bf_ <- skateslc_ %>%
  mutate(group = as.factor(Total.score > median(Total.score)),
         residuals_ = model_1$residuals) %>%
  bf.test(residuals_ ~ group, data = ., verbose = FALSE)
dwtest_0 <- dwtest(model_0)
dwtest_ <- dwtest(model_1)
