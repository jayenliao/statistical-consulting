# title: "統計諮詢 - 作業7"
# subtitle: "國立成功大學統計學系暨數據科學研究所"
# author: Jay Liao 廖傑恩 re6094028@gs.ncku.edu.tw

library(HH)
library(dplyr)
library(jtools)
library(ggplot2)
library(interactions)
library(GGally)
library(EnvStats)
library(pROC)

# Exercise 15.4

data('mortality')

plot(mortality, col = c('darkorange', 'dodgerblue'))
sigma_sq_psi <- 1/530 + 1/333 + 1/4340 + 1/32637


# Exercise 17.4

data('icu')

icu %>% dplyr::select(STA, SEX) %>% table() %>%
  plot(., main = '', col = c('darkorange', 'dodgerblue'))

icu %>% mutate(STA = factor(STA)) %>%
  qplot(x = STA, y = AGE, geom = 'boxplot', data = .) +
  theme_bw()

icu %>% mutate(STA = factor(STA), SEX = factor(SEX)) %>%
  qplot(x = SEX, y = AGE, col = STA, geom = 'boxplot', data = .) +
  theme_bw() +
  theme(legend.position = 'top')

set.seed(2021)
df1 <- icu %>% filter(STA == 'Died') %>%
  mutate(TR = sample(c(rep(TRUE, 32), rep(FALSE, 8))))
df2 <- icu %>% filter(STA != 'Died') %>%
  mutate(TR = sample(c(rep(TRUE, 128), rep(FALSE, 32))))
icu_new <- rbind(df1, df2)

lr_model <- icu_new %>% filter(TR) %>%
  mutate(STA = factor(STA), SEX = factor(SEX)) %>%
  glm(STA ~ SEX + AGE, family = binomial('logit'), data = .)

result <- summ(lr_model)
df <- result$coeftable %>% round(., 4)
colnames(df) <- c('係數估計值', '標準誤', 'Z檢定統計量', 'p值')
rownames(df) <- c('beta_0', 'beta_1', 'beta_2')
knitr::kable(df)

y_hat_tr <- lr_model$fitted.values
y_hat_te <- predict.glm(lr_model, type = 'response',
                        newdata = icu_new[!icu_new$TR,])
y_tr <- icu_new$STA[icu_new$TR] == 'Died'
y_te <- icu_new$STA[!icu_new$TR] == 'Died'

acc_tr <- mean((y_hat_tr >= .5) == y_tr)
acc_te <- mean((y_hat_te >= .5) == y_te)

ph <- predict.glm(lr_model, type='response')
oh <- ph / (1 - ph)
loh <- log(oh)

p1 <- icu_new %>% filter(TR) %>%
  qplot(x = AGE, y = ph, geom = c('smooth', 'point'), col = SEX, data = .) +
  labs(x = 'Age', y = 'Predicted Probability') +
  theme_bw() + theme(legend.position = 'top')

p2 <- icu_new %>% filter(TR) %>%
  qplot(x = AGE, y = oh, geom = c('smooth', 'point'), col = SEX, data = .) +
  labs(x = 'Age', y = 'Predicted Odds') +
  theme_bw() + theme(legend.position = 'top')

p3 <- icu_new %>% filter(TR) %>%
  qplot(x = AGE, y = loh, geom = c('smooth', 'point'), col = SEX, data = .) +
  labs(x = 'Age', y = 'Logged Predicted Odds') +
  theme_bw() + theme(legend.position = 'top')

p1
p2
p3

ph <- predict.glm(lr_model, newdata = icu_new[!icu_new$TR,], type='response')
oh <- ph / (1 - ph)
loh <- log(oh)

p4 <- icu_new %>% filter(!TR) %>%
  qplot(x = AGE, y = ph, geom = c('smooth', 'point'), col = SEX, data = .) +
  labs(x = 'Age', y = 'Predicted Probability') +
  theme_bw() + theme(legend.position = 'top')

p5 <- icu_new %>% filter(!TR) %>%
  qplot(x = AGE, y = oh, geom = c('smooth', 'point'), col = SEX, data = .) +
  labs(x = 'Age', y = 'Predicted Odds') +
  theme_bw() + theme(legend.position = 'top')

p6 <- icu_new %>% filter(!TR) %>%
  qplot(x = AGE, y = loh, geom = c('smooth', 'point'), col = SEX, data = .) +
  labs(x = 'Age', y = 'Logged Predicted Odds') +
  theme_bw() + theme(legend.position = 'top')

p4
p5
p6

par(mfcol = c(1, 2))
plot(roc(as.numeric(y_tr), y_hat_tr),
     main = 'ROC on The Training Dataset')
plot(roc(as.numeric(y_te), y_hat_te),
     main = 'ROC on The Testing Dataset')
auc_tr <- auc(roc(as.numeric(y_tr), y_hat_tr)) %>% round(.,4)
auc_te <- auc(roc(as.numeric(y_te), y_hat_te)) %>% round(.,4)
