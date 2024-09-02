# title: "統計諮詢 - 作業5"
# subtitle: "國立成功大學統計學系暨數據科學研究所"
# author: Jay Liao 廖傑恩 re6094028@gs.ncku.edu.tw

library(HH)
library(dplyr)
library(jtools)
library(ggplot2)
library(GGally)
library(EnvStats)

# Exercise 12.4

data('barleyp')

barleyp_ <- barleyp %>% mutate(Trt = factor(Trt), Soil = factor(Soil))
levels(barleyp_$Trt) <- c('(NH4)SO4', 'NH4NO3', 'CO(NH2)2', 'Ca(NO3)2', 'NaNO3', 'Control')

qplot(x = Trt, y = Yield, geom = 'boxplot', data = barleyp_) +
  labs(x = 'Treatment') + theme_bw()

barleyp %>% mutate(Soil = paste0('Soil type ', Soil)) %>%
  qplot(x = Soil, y = Yield, geom = 'boxplot', data = .) +
  labs(x = 'Soil Type') + theme_bw()

barleyp %>% mutate(Soil = paste0('Soil type ', Soil)) %>%
  ggplot(aes(y = Yield, x = factor(Trt)), data = .) +
  geom_point() +
  geom_segment(aes(y = 0, x = factor(Trt), yend = Yield, xend = factor(Trt))) +
  xlab('Treatment') +
  facet_wrap(. ~ Soil, nrow = 1) + theme_bw()

sw_test <- shapiro.test(barleyp$Yield)

bartlett_T <- bartlett.test(Yield ~ factor(Trt), data = barleyp)
bartlett_S <- bartlett.test(Yield ~ factor(Soil), data = barleyp)
ts_T <- round(bartlett_T$statistic, 4)
ts_S <- round(bartlett_S$statistic, 4)
p_T <- round(bartlett_T$p.value, 4)
p_S <- round(bartlett_S$p.value, 4)

bartlett_T <- car::leveneTest(Yield ~ factor(Trt), data = barleyp)
bartlett_S <- car::leveneTest(Yield ~ factor(Soil), data = barleyp)
p_T <- round(bartlett_T$`Pr(>F)`[1], 4)
p_S <- round(bartlett_S$`Pr(>F)`[1], 4)

model_barleyp <- aov(Yield ~ factor(Trt) + Error(factor(Soil)), data = barleyp)
df <- rstatix::anova_summary(model_barleyp, detailed = TRUE)
df <- data.frame(
  變異來源 = c('處置', '殘差'),
  自由度 = round(c(df$DFn, df$DFd), 2),
  平方和 = round(c(df$SSn, df$SSd), 2),
  均方 = round(c(df$SSn, df$SSd) / c(df$DFn, df$DFd), 2),
  `F值` = c(round(df$`F`, 2), ''),
  `p值` = c(df$p, '')
)
knitr::kable(df)

aov.out.pr <- proj(model_barleyp)
resi <- aov.out.pr[['Within']][,'Residuals']
par(pty = 's')
qqnorm(resi, main = 'Normal Q-Q Plot')
qqline(resi, col = 'red')
sw_test <- shapiro.test(resi)

contrasts(barleyp_$Trt) <- contr.treatment(6)
barleyp.wrong <- aov(terms(Yield ~ Trt + Soil, keep.order = TRUE), data = barleyp_)
Trt.lmat <- contr.poly(6)
rownames(Trt.lmat) <- levels(barleyp$Trt)
barleyp.mmc <- mmc(barleyp.wrong, focus = 'Trt', alternative = 'less')
df <- barleyp.mmc$mca$table %>% as.data.frame()
colnames(df) <- c('組間差異平均估計值', '標準誤', 'CI下界', 'CI上界')


# Exercise 13.1

data(heartvalve)

heartvalve_ <- heartvalve %>% mutate(Valve = factor(Valve),
                                     Run = factor(Run),
                                     Pulse = factor(Pulse))
heartvalve_ %>% 
  qplot(x = Valve, y = Flow, geom = 'boxplot', data = .) +
  labs(x = 'Valve Type') + theme_bw()

heartvalve_ %>% 
  qplot(x = Pulse, y = Flow, geom = 'boxplot', data = .) +
  labs(x = 'Pulse Level') + theme_bw()

heartvalve %>% mutate(Valve = paste('Valve', Valve)) %>%
  ggplot(aes(x = Pulse, y = Flow), data = .) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(. ~ Valve) +
  labs(x = 'Pulse Level') + theme_bw()

sw_test <- shapiro.test(heartvalve_$Flow)

bartlett_T <- bartlett.test(Flow ~ Valve, data = heartvalve_)
bartlett_S <- bartlett.test(Flow ~ Pulse, data = heartvalve_)
ts_T <- round(bartlett_T$statistic, 4)
ts_S <- round(bartlett_S$statistic, 4)
p_T <- round(bartlett_T$p.value, 4)
p_S <- round(bartlett_S$p.value, 4)

bartlett_T <- car::leveneTest(Flow ~ Valve, data = heartvalve_)
bartlett_S <- car::leveneTest(Flow ~ Pulse, data = heartvalve_)
p_T <- round(bartlett_T$`Pr(>F)`[1], 4)
p_S <- round(bartlett_S$`Pr(>F)`[1], 4)

model_heart <- aov(Flow ~ Valve*Pulse + Valve/Run, data = heartvalve_)
df <- rstatix::anova_summary(car::Anova(model_heart), detailed = TRUE)
df <- data.frame(
  變異來源 = c('瓣膜型態（V）', '脈衝速率（P）', 'V與P交互作用', 'V與回合次別交互作用', '殘差'),
  自由度 = round(c(df$DFn, df$DFd[1]), 2),
  平方和 = round(c(df$SSn, df$SSd[1]), 2),
  均方 = round(c(df$SSn, df$SSd[1]) / c(df$DFn, df$DFd[1]), 2),
  `F值` = c(round(df$`F`, 2), ''),
  `p值` = c(df$p, '')
)
knitr::kable(df)

model_ <- lme4::lmer(Flow ~ Pulse + Valve/(1|Run) + Valve:Pulse, data = heartvalve)
anova(model_)

heartvalve %>%
  group_by(Valve, Pulse) %>%
  summarise(Flow = mean(Flow)) %>%
  ggplot(aes(x = Pulse, y = Flow, col = factor(Valve)), data = .) +
  geom_point() +
  geom_line() +
  labs(x = 'Pulse Level', y = 'Flow Mean', colour = 'Valve Type') +
  scale_x_continuous(breaks = 1:6, labels = 1:6) +
  theme_bw() + theme(legend.position = 'top')

par(pty = 's')
qqnorm(model_heart$residuals, main = 'Normal Q-Q Plot')
qqline(model_heart$residuals, col = 'red')
sw_test <- shapiro.test(model_heart$residuals)

contrasts(heartvalve_$Valve) <- contr.treatment(4)
heartvalve.wrong <- aov(terms(Flow ~ Valve*Pulse, keep.order = TRUE),
                        data = heartvalve_)
heartvalve.mmc <- mmc(heartvalve.wrong, focus = 'Valve', alternative = 'greater')
df <- heartvalve.mmc$mca$table %>% as.data.frame()
colnames(df) <- c('組間差異平均估計值', '標準誤', 'CI下界', 'CI上界')
knitr::kable(round(df, 4)[,-2])
mmcplot(heartvalve.mmc)
