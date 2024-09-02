# title: "統計諮詢 - 作業8"
# subtitle: "國立成功大學統計學系暨數據科學研究所"
# author: Jay Liao 廖傑恩 re6094028@gs.ncku.edu.tw

library(HH)
library(dplyr)
library(ggplot2)
library(aTSA)
library(tseries)
library(forecast)

# Exercise 18.4
data('product')

## 資料探索
plot(product, type = 'o', pch = 19, cex = .5, col = 'dodgerblue4',
     xlab = 'Week', ylab = 'Sales', main = 'Time Series Plot of Sales')
grid()

plot(product, type = 'o', pch = 19, cex = .5 ,
     col = 'dodgerblue4', family = '蘋方-繁 標準體',
     xlab = '週次', ylab = '銷售量', main = '塑膠容器銷售量時間序列圖')
grid()

product_diff <- diff(product)
plot(product_diff, type = 'o', pch = 19, cex = .5, col = 'dodgerblue4',
     xlab = 'Week', ylab = 'Sales', main = 'Time Series Plot of Sales (lag=1)')
grid()

plot(product_diff, type = 'o', pch = 19, cex = .5 ,
     col = 'dodgerblue4', family = '蘋方-繁 標準體',
     xlab = '週次', ylab = '銷售量差異', main = '塑膠容器銷售量時間序列圖（一階差分）')
grid()

product_diff4 <- diff(product, lag = 4)
plot(product_diff4, type = 'o', pch = 19, cex = .5, col = 'dodgerblue4',
     xlab = 'Week', ylab = 'Sales', main = 'Time Series Plot of Sales (lag=4)')
grid()

plot(product_diff4, type = 'o', pch = 19, cex = .5 ,
     col = 'dodgerblue4', family = '蘋方-繁 標準體',
     xlab = '週次', ylab = '銷售量差異', main = '塑膠容器銷售量時間序列圖（季節性差分）')
grid()

## 資料分析

### 自迴歸函數

par(mfcol = c(3, 2))
TSA::acf(product, main = 'ACF Plot (lag=0)')
TSA::acf(product_diff, main = 'ACF Plot (lag=1)')
TSA::acf(product_diff4, main = 'ACF Plot (lag=4)')
pacf(product, main = 'PACF Plot (lag=0)')
pacf(product_diff, main = 'PACF Plot (lag=1)')
pacf(product_diff4, main = 'ACF Plot (lag=4)')

### 平穩性檢定

df_test_0 <- tseries::adf.test(product)
s0 <- round(df_test_0$statistic, 4)
p0 <- round(df_test_0$p.value, 4)

df_test_1 <- tseries::adf.test(product_diff)
s1 <- round(df_test_1$statistic, 4)
p1 <- round(df_test_1$p.value, 4)

df_test_4 <- tseries::adf.test(product_diff4)
s4 <- round(df_test_4$statistic, 4)
p4 <- round(df_test_4$p.value, 4)

### 時間序列模型定義

### 資料切割

N_tr <- round(length(product)*.9)

### 配適結果

fit1 <- Arima(product[1:N_tr], c(1,1,1), seasonal=list(order=c(1,0,0), period=4))
fit2 <- Arima(product[1:N_tr], c(1,1,2), seasonal=list(order=c(1,0,1), period=4))

### 殘差診斷

plot_arima_residuals <- function(fit, fit_name) {
  res <- fit$residuals
  par(mfcol = c(2, 2))
  # plot 1
  plot(res, main = paste0('Time Series Plot of Residuals\nof ', fit_name))
  abline(h = 0, col = 'red')
  grid()
  # plot 2
  hist(res, main = paste0('Histogram of Residuals\nof ', fit_name))
  # plot 3
  TSA::acf(res, main = paste0('ACF Plot of Residuals\nof ', fit_name), )
  # plot 4
  par(pty = 's')
  qqplot(rnorm(length(res)), res,
         main = paste0('Normal QQ-Plot of Residuals\nof ', fit_name))
  grid()
}

plot_arima_residuals(fit1, 'SARIMA(1,1,1) (1,0,0)4')
plot_arima_residuals(fit2, 'SARIMA(1,1,2) (1,0,1)4')

t1 <- t.test(fit1$residuals)
t1s <- round(t1$statistic, 4)
t1p <- round(t1$p.value, 4)
t2 <- t.test(fit2$residuals)
t2s <- round(t2$statistic, 4)
t2p <- round(t2$p.value, 4)

ks1 <- ks.test(fit1$residuals, rnorm(5))
ks1s <- round(ks1$statistic, 4)
ks1p <- round(ks1$p.value, 4)
ks2 <- ks.test(fit2$residuals, rnorm(5))
ks2s <- round(ks2$statistic, 4)
ks2p <- round(ks2$p.value, 4)

ljung1 <- Box.test(x = fit1$residuals, lag = 25, type = "Ljung-Box")
ljung1s <- round(ljung1$statistic, 4)
ljung1p <- round(ljung1$p.value, 4)
ljung2 <- Box.test(x = fit2$residuals, lag = 25, type = "Ljung-Box")
ljung2s <- round(ljung2$statistic, 4)
ljung2p <- round(ljung2$p.value, 4)

### 模型比較
forecast1 <- forecast(fit1, 30)
forecast2 <- forecast(fit2, 30)

plot(forecast1, type = 'o', pch = 19, cex = .5, xlab = 'Week', ylab = 'Sales')
points(270:299, product[270:299], type = 'o', pch = 19, cex = .5, col = 'dodgerblue4')
grid()

plot(forecast2, type = 'o', pch = 19, cex = .5, xlab = 'Week', ylab = 'Sales')
points(270:299, product[270:299], type = 'o', pch = 19, cex = .5, col = 'dodgerblue4')
grid()

aic1 <- round(fit1$aic, 2)
aic2 <- round(fit2$aic, 2)
mse1_tr <- round(mean((as.numeric(forecast1$fitted) - product[1:269])^2), 2)
mse1_te <- round(mean((as.numeric(forecast1$mean) - product[270:299])^2), 2)
mse2_tr <- round(mean((as.numeric(forecast2$fitted) - product[1:269])^2), 2)
mse2_te <- round(mean((as.numeric(forecast2$mean) - product[270:299])^2), 2)
