# title: "統計諮詢 - 作業3"
# subtitle: "國立成功大學統計學系暨數據科學研究所"
# author: Jay Liao 廖傑恩 re6094028@gs.ncku.edu.tw

library(HH)
library(dplyr)
library(ggplot2)
library(GGally)
library(jtools)

# Exercise 9.3

data(hpErie)
hpErie %>% dplyr::select(-type, -style) %>% ggpairs(., upper = list(na = 'na'))

par(mfcol = c(1, 2))
hist(hpErie$price, freq = FALSE, xlab = 'House Price', main = 'Histogram of House Price')
boxplot(hpErie$price, main = 'Box Plot of House Price')

hpErie %>% dplyr::filter(hpErie$price < 500) %>% 
  dplyr::select(-type, -style) %>% ggpairs(., upper = list(na = 'na'))

X <- hpErie %>%
  mutate(type.bf = as.numeric(type == 'brick&frame'),
         type.af = as.numeric(type == 'alum&frame'),
         type.f = as.numeric(type == 'frame'),
         style.1.5 = as.numeric(style == '1.5.story'),
         style.ranch = as.numeric(style == 'ranch'))

model_null <- lm(price ~ 1, data = X)
model_full <- lm(price ~ lotsize + garage + rooms + age + type.bf + type.af +
                   type.f + style.1.5 + style.ranch + fireplac, data = X)
step_back <- step(model_full, scope = list(upper = model_full),
                  direction = 'back', trace = 0)

df <- step_back$anova[,c(1,6)]
df$Step[2:nrow(df)] <- gsub('- ', '移除', df$Step[2:nrow(df)])
df$Step[1] <- '納入所有變項的原始模型'
colnames(df) <- c('步驟', '步驟完成後的模型AIC')
knitr::kable(df)

sum_tb <- summ(step_back, confint = TRUE)
df_ <- sum_tb$coeftable %>% as.data.frame() %>% round(., 4) 
colnames(df_) <- c('估計值', '95%CI下界', '95%CI上界', 't檢定統計量', 'p值')
S <- rownames(df_)
rownames(df_) <- paste0(c('截距', S[-1]), ' (beta_', c(0,6,8,9,11,14), ')')
knitr::kable(df_)

sw_test <- shapiro.test(step_back$residuals) 

X_ <- X %>% filter(price < 500)
model_remove <- lm(price ~ rooms + age + type.bf + type.f + fireplac, data = X_)
sum_tb <- summ(model_remove, confint = TRUE)

X <- hpErie %>%
  mutate(type.bf = as.numeric(type == 'brick&frame'),
         type.af = as.numeric(type == 'alum&frame'),
         type.f = as.numeric(type == 'frame'),
         style.1.5 = as.numeric(style == '1.5.story'),
         style.ranch = as.numeric(style == 'ranch'),
         sqfeetsq = sqfeet^2)

model_null <- lm(price ~ 1, data = X)
model_full <- lm(price ~ lotsize + garage + age + type.bf + type.af + type.f +
                   style.1.5 + style.ranch + fireplac + sqfeetsq, data = X)
step_back <- step(model_full, scope = list(upper = model_full),
                  direction = 'back', trace = 0)

sum_tb <- summ(step_back, confint = TRUE)
df_ <- sum_tb$coeftable %>% as.data.frame() %>% round(., 4) 
colnames(df_) <- c('估計值', '95%CI下界', '95%CI上界', 't檢定統計量', 'p值')
S <- rownames(df_)
rownames(df_) <- paste0(c('截距', S[-1]), ' (beta_', c(0,5,8,9,10,11,14,15), ')')
knitr::kable(df_)

sw_test <- shapiro.test(step_back$residuals) 

X_ <- X %>% filter(price < 500)
model_remove <- lm(price ~ garage + age + type.bf + type.af + type.f + fireplac + sqfeetsq, data = X_)
sum_tb <- summ(model_remove, confint = TRUE)


# Exercise 10.4

data(water)

water %>%
  mutate(Location = c('South of Derby', 'North of Derby')[derbynor + 1]) %>%
  qplot(y = mortality, x = calcium, col = Location, data = .) +
  facet_wrap(. ~ Location) +
  theme_bw() + theme(legend.position = 'none') +
  ggtitle('Scatter Plots of Calcium in Public Water and Avg. Male Mortality')

model_N <- lm(mortality ~ calcium, data = water[water$derbynor==1,])
model_S <- lm(mortality ~ calcium, data = water[water$derbynor==0,])
sigma_2_hat_N <- sum(model_N$residuals^2) / (length(model_N$residuals)-2)
sigma_2_hat_S <- sum(model_S$residuals^2) / (length(model_S$residuals)-2)


model_N_tb <- summ(model_N, confint = TRUE)
df_N <- model_N_tb$coeftable %>% as.data.frame() %>% round(., 4) 
colnames(df_N) <- c('估計值', '95%CI下界', '95%CI上界', 't檢定統計量', 'p值')
rownames(df_N) <- c('截距 (beta_0)', '鈣濃度 (beta_1)')
CI_N <- paste0('[', df_N$`95%CI下界`[2], ', ', df_N$`95%CI上界`[2], ']')

model_S_tb <- summ(model_S, confint = TRUE)
df_S <- model_S_tb$coeftable %>% as.data.frame() %>% round(., 4) 
colnames(df_S) <- c('估計值', '95%CI下界', '95%CI上界', 't檢定統計量', 'p值')
rownames(df_S) <- c('截距 (beta_0)', '鈣濃度 (beta_1)')
CI_S <- paste0('[', df_S$`95%CI下界`[2], ', ', df_S$`95%CI上界`[2], ']')

knitr::kable(df_N)
knitr::kable(df_S)

par(mfcol = c(1, 2))
plot(model_N, which = c(1, 2))
par(mfcol = c(1, 2))
plot(model_S, which = c(1, 2))

shapiro_test_N <- shapiro.test(model_N$residuals)
shapiro_test_S <- shapiro.test(model_S$residuals)

dwtest_N <- lmtest::dwtest(model_N)
dwtest_S <- lmtest::dwtest(model_S)

bf_N <- water %>% dplyr::filter(derbynor == 1) %>%
  mutate(group = as.factor(calcium > median(calcium)),
         residuals_ = model_N$residuals) %>%
  onewaytests::bf.test(residuals_ ~ group, data = ., verbose = FALSE)

bf_S <- water %>% dplyr::filter(derbynor == 0) %>%
  mutate(group = as.factor(calcium > median(calcium)),
         residuals_ = model_S$residuals) %>%
  onewaytests::bf.test(residuals_ ~ group, data = ., verbose = FALSE)

L <- water$derbynor == 1
R_2_N <- (var(water[L, 'mortality']) - sigma_2_hat_N) / var(water[L, 'mortality'])
L <- water$derbynor == 0
R_2_S <- (var(water[L, 'mortality']) - sigma_2_hat_S) / var(water[L, 'mortality'])

water %>%
  mutate(Location = c('South of Derby', 'North of Derby')[derbynor + 1]) %>%
  qplot(y = mortality, x = calcium, col = Location, data = .) +
  geom_abline(slope = model_N$coefficients[2],
              intercept = model_N$coefficients[1], col = 'darkorange2') +
  geom_abline(slope = model_S$coefficients[2],
              intercept = model_S$coefficients[1], col = 'dodgerblue') +
  scale_colour_manual(values = c('darkorange2', 'dodgerblue')) +
  theme_bw() + theme(legend.position = 'top') +
  ggtitle('Scatter Plot of Calcium in Public Water and Avg. Male Mortality\nwith Lines of Two Linear Regression Models')


model_NS <- lm(mortality ~ calcium*derbynor, data = water)
sigma_2_hat_NS <- sum(model_NS$residuals^2) / (length(model_NS$residuals)-1*1-1)

model_NS_tb <- summ(model_NS, confint = TRUE)
df_NS <- model_NS_tb$coeftable %>% as.data.frame() %>% round(., 4) 
colnames(df_NS) <- c('估計值', '95%CI下界', '95%CI上界', 't檢定統計量', 'p值')
rownames(df_NS) <- c('截距 (beta_0)', '鈣濃度 (beta_1)', ' 相對地理位置 (beta_2)', '交互作用 (beta_3)')
CI_NS <- paste0('[', df_NS$`95%CI下界`[2], ', ', df_NS$`95%CI上界`[2], ']')

knitr::kable(df_NS)

model_NS_wo_inter <- lm(mortality ~ calcium + derbynor, data = water)
sigma_2_hat_NSwo_inter <- sum(model_NS_wo_inter$residuals^2) / (length(model_NS_wo_inter$residuals) - 1 - 1 - 1)

model_NS_wo_inter_tb <- summ(model_NS_wo_inter, confint = TRUE)
df_NS_wo_inter <- model_NS_wo_inter_tb$coeftable %>%
  as.data.frame() %>% round(., 4) 
colnames(df_NS_wo_inter) <- c('估計值', '95%CI下界', '95%CI上界', 't檢定統計量', 'p值')
rownames(df_NS_wo_inter) <- c('截距 (beta_0)', '鈣濃度 (beta_1)', ' 相對地理位置 (beta_2)')

knitr::kable(df_NS_wo_inter)

par(mfcol = c(1, 2))
plot(model_NS_wo_inter, which = c(1, 2))
shapiro_test_NS <- shapiro.test(model_NS_wo_inter$residuals)
dwtest_NS <- lmtest::dwtest(model_NS_wo_inter)

bf_NS <- water %>%
  mutate(group = as.factor(calcium > median(calcium)),
         residuals_ = model_NS_wo_inter$residuals) %>%
  bf.test(residuals_ ~ group, data = ., verbose = FALSE)

R_2_NS <- (var(water[,'mortality']) - sigma_2_hat_NSwo_inter) / var(water[, 'mortality'])

water %>%
  qplot(y = mortality, x = calcium, col = factor(derbynor), data = .) +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(colour = 'derbynor') +
  theme_bw() + theme(legend.position = 'top') +
  ggtitle('Scatter Plot of Calcium in Public Water and Avg. Male Mortality\nwith Lines of One Linear Regreesion Model')
