# title: "統計諮詢 - 作業1"
# subtitle: "國立成功大學統計學系暨數據科學研究所"
# author: Jay Liao 廖傑恩 re6094028@gs.ncku.edu.tw

library(HH)
library(dplyr)
library(GGally)
library(ggplot2)

# Exercise 4.2
data(usair)

### Scatter plot matrices

ggpairs(usair) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = .8)) +
  ggtitle('Scatter Plot Matrix of 7 Variables of usair Dataset')

usair_transform <- apply(usair, 2, log) %>% as.data.frame()
ggpairs(usair_transform) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = .8)) +
  ggtitle('Scatter Plot Matrix of 7 Variables of usair Dataset with Log-Transformation')

### The most highly correlated predictors with the logged response
mtx_cor <- usair %>% mutate(log_SO2 = log(SO2)) %>% cor()
v_cor <- mtx_cor[-which(rownames(mtx_cor) %in% c('SO2', 'log_SO2')), 'log_SO2']
knitr::kable(data.frame(`與logged response的相關係數` = round(v_cor, 4)))

### Pairs of highly correlated logged predictors
mtx_cor <- usair_transform %>% dplyr::select(-SO2) %>% cor()
L <- lower.tri(mtx_cor)
corrplot::corrplot(mtx_cor, type = 'lower', method = 'color')

Predictor_1 <- sapply(1:nrow(mtx_cor), function(i) {
  rownames(mtx_cor)[L[,i]]
}) %>% unlist()

Predictor_2 <-sapply(1:(ncol(mtx_cor)-1), function(i) {
  rep(rownames(mtx_cor)[i], ncol(mtx_cor) - i)
}) %>% unlist()

df_output <- data.frame(Predictor_1 = Predictor_1,
                        Predictor_2 = Predictor_2,
                        Correlation_coefficient = mtx_cor[lower.tri(mtx_cor)])
df_output$High <- c('No', 'Yes')[as.numeric(abs(df_output$Correlation_coefficient) > .7) + 1]
df_output <- df_output[order(
  abs(df_output$Correlation_coefficient), decreasing = TRUE),]
df_output$Correlation_coefficient <- round(df_output$Correlation_coefficient, 4)
colnames(df_output) <- c('變項1', '變項2', '相關係數', '是否高相關')
rownames(df_output) <- NULL
knitr::kable(df_output)


# Exercise 5.13
data(har1)
normality_test <- shapiro.test(har1$Post - har1$Pre)

## Exercise 5.13 - (a)
grubbs_test_l <- outliers::grubbs.test(har1$Post - har1$Pre)
grubbs_test_h <- outliers::grubbs.test(har1$Post - har1$Pre, opposite = TRUE)
model_ <-t.test(har1$Post, har1$Pre, paired = TRUE, alternative = 'less')

## Exercise 5.13 - (b)
set.seed(4028)
t_sample_df5 <- rt(100, df = 5)
par(pty="s")
qqplot(t_sample_df5, har1$Post,
       xlim = c(-8.5, 8.5), ylim = c(-11, 11),
       xlab = 'Theoretical Quantile of t-distribution with df=5',
       ylab = 'Quantile of The Post-Treatment Sample')
abline(a=0, b=1, col='red')
