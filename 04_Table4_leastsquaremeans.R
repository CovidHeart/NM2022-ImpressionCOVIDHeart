library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(emmeans)

dfbl.cov <- readRDS('dfbl.cov.RDS')

dfbl.ancova <- dfbl.cov

dfbl.ancova1 <- dfbl.ancova[,c('T1', 'T1FU','shared2')]
dfbl.ancova1 %>% anova_test(T1FU ~ shared2*T1)
model <- lm(T1FU ~ T1 + shared2, data = dfbl.ancova1)
shapiro_test(model$residuals)
res.aov <- dfbl.ancova1 %>% anova_test(T1FU ~ T1 + shared2)
get_anova_table(res.aov)
emm <- emmeans_test(dfbl.ancova1, T1FU ~ shared2, covariate = T1, p.adjust.method = 'bonferroni')
emm
get_emmeans(emm)

#________
dfbl.ancova1 <- dfbl.ancova[,c('T2', 'T2FU','shared2')]
dfbl.ancova1 %>% anova_test(T2FU ~ shared2*T2)
model <- lm(T2FU ~ T2 + shared2, data = dfbl.ancova1)
shapiro_test(model$residuals)
res.aov <- dfbl.ancova1 %>% anova_test(T2FU ~ T2 + shared2)
get_anova_table(res.aov)
emm <- emmeans_test(dfbl.ancova1, T2FU ~ shared2, covariate = T2, p.adjust.method = 'bonferroni')
emm
get_emmeans(emm)
#________
dfbl.ancova1 <- dfbl.ancova[,c('HR', 'HRFU','shared2')]
dfbl.ancova1 <- na.omit(dfbl.ancova1)
dfbl.ancova1 %>% anova_test(HRFU ~ shared2*HR)
model <- lm(HRFU ~ HR + shared2, data = dfbl.ancova1)
shapiro_test(model$residuals)
res.aov <- dfbl.ancova1 %>% anova_test(HRFU ~ HR + shared2)
get_anova_table(res.aov)
emm <- emmeans_test(dfbl.ancova1, HRFU ~ shared2, covariate = HR, p.adjust.method = 'bonferroni')
emm
get_emmeans(emm)
#________
dfbl.ancova1 <- dfbl.ancova[,c('RVEF', 'RVEFFU','shared2')]
dfbl.ancova1 <- na.omit(dfbl.ancova1)
dfbl.ancova1 %>% anova_test(RVEFFU ~ shared2*RVEF)
model <- lm(RVEFFU ~ RVEF + shared2, data = dfbl.ancova1)
shapiro_test(model$residuals)
res.aov <- dfbl.ancova1 %>% anova_test(RVEFFU ~ RVEF + shared2)
get_anova_table(res.aov)
emm <- emmeans_test(dfbl.ancova1, RVEFFU ~ shared2, covariate = RVEF, p.adjust.method = 'bonferroni')
emm
get_emmeans(emm)
#----------
dfbl.ancova1 <- dfbl.ancova[,c('RRsys', 'RRsysFU','shared2')]
dfbl.ancova1 <- na.omit(dfbl.ancova1)
dfbl.ancova1 %>% anova_test(RRsysFU ~ shared2*RRsys)
model <- lm(RRsysFU ~RRsys + shared2, data = dfbl.ancova1)
shapiro_test(model$residuals)
res.aov <- dfbl.ancova1 %>% anova_test(RRsysFU ~ RRsys + shared2)
get_anova_table(res.aov)
emm <- emmeans_test(dfbl.ancova1, RRsysFU ~ shared2, covariate = RRsys, p.adjust.method = 'bonferroni')
emm
get_emmeans(emm)
#----------
dfbl.ancova1 <- dfbl.ancova[,c('LVEF', 'LVEFFU','shared2')]
dfbl.ancova1 <- na.omit(dfbl.ancova1)
dfbl.ancova1 %>% anova_test(LVEFFU ~ shared2*LVEF)
model <- lm(LVEFFU ~LVEF + shared2, data = dfbl.ancova1)
shapiro_test(model$residuals)
res.aov <- dfbl.ancova1 %>% anova_test(LVEFFU ~ LVEF + shared2)
get_anova_table(res.aov)
emm <- emmeans_test(dfbl.ancova1, LVEFFU ~ shared2, covariate = LVEF, p.adjust.method = 'bonferroni')
emm
get_emmeans(emm)
#----------
dfbl.ancova1 <- dfbl.ancova[,c('GLS', 'GLSFU','shared2')]
dfbl.ancova1 <- na.omit(dfbl.ancova1)
dfbl.ancova1 %>% anova_test(GLSFU ~ shared2*GLS)
model <- lm(GLSFU ~GLS + shared2, data = dfbl.ancova1)
shapiro_test(model$residuals)
res.aov <- dfbl.ancova1 %>% anova_test(GLSFU ~ GLS + shared2)
get_anova_table(res.aov)
emm <- emmeans_test(dfbl.ancova1, GLSFU ~ shared2, covariate = GLS, p.adjust.method = 'bonferroni')
emm
get_emmeans(emm)
#----------
dfbl.ancova1 <- dfbl.ancova[,c('EDVi', 'EDViFU','shared2')]
dfbl.ancova1 <- na.omit(dfbl.ancova1)
dfbl.ancova1 %>% anova_test(EDViFU ~ shared2*EDVi)
model <- lm(EDViFU ~EDVi + shared2, data = dfbl.ancova1)
shapiro_test(model$residuals)
res.aov <- dfbl.ancova1 %>% anova_test(EDViFU ~ EDVi + shared2)
get_anova_table(res.aov)
emm <- emmeans_test(dfbl.ancova1, EDViFU ~ shared2, covariate = EDVi, p.adjust.method = 'bonferroni')
emm
get_emmeans(emm)
#----------
dfbl.ancova1 <- dfbl.ancova[,c('LVMi', 'LVMiFU','shared2')]
dfbl.ancova1 <- na.omit(dfbl.ancova1)
dfbl.ancova1 %>% anova_test(LVMiFU ~ shared2*LVMi)
model <- lm(LVMiFU ~LVMi + shared2, data = dfbl.ancova1)
shapiro_test(model$residuals)
res.aov <- dfbl.ancova1 %>% anova_test(LVMiFU ~ LVMi + shared2)
get_anova_table(res.aov)
emm <- emmeans_test(dfbl.ancova1, LVMiFU ~ shared2, covariate = LVMi, p.adjust.method = 'bonferroni')
emm
get_emmeans(emm)
