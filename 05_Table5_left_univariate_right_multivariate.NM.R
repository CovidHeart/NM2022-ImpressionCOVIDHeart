library(dplyr)
library(questionr) # for the OR

dfbl.cov <- readRDS('dfbl.cov.RDS')

dfbl.cov$lg_TNT <- log(dfbl.cov$TNT)
dfbl.cov$lg_BNP <- log(dfbl.cov$BNP)
dfbl.cov$lg_CRP <- log(dfbl.cov$CRP)
dfbl.cov$time_from_dx <- as.numeric(dfbl.cov$time_from_dx, units = 'days')

#---------
library(gtsummary)

dfbl.cov$Gender2 <- as.integer(dfbl.cov$Gender)
dfbl.model <- subset(dfbl.cov)

dfbl.model %>%
 ungroup()

tbl_uvregression <- dfbl.model %>%
  select(long_covid_fu, Age,Gender,HR,RRsys,RRdia, BNP,TNT,CRP,LVEF,GLS,EDVi,LVMi,RVEF,
         T1, T2,LGEblbin,Pericardial_Effusion_bin,Pericardial_Enhancement,time_from_dx) %>%
  tbl_uvregression(
    method = glm,
    y = long_covid_fu,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  add_global_p() %>%  
  add_q() %>%         
  bold_p() %>%        
  bold_p(t = 0.10, q = TRUE) %>% 
  bold_labels()
tbl_uvregression

#--------
full.model <- glm(long_covid_fu ~ T1 + T2 + LVEF + Age + Gender + HR + log(TNT) +
                    log(CRP) + log(BNP) + time_from_dx + LGEblbin + RVEF + RRsys + PE  + GLS,
                  family=binomial(link='logit'),data=dfbl.cov)
library(blorr)
blr <- blr_step_p_forward(full.model)

#--------
##add significant terms from above here ##
final.model <- glm(long_covid_fu ~ T1 + Gender,
                   family=binomial(link='logit'),data=dfbl.cov)
summary(final.model)
odds.ratio(final.model)
final.model$aic
#---------------

