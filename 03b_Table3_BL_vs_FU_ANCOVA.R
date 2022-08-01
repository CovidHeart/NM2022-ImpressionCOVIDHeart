library(gtsummary)

dftable3 <- readRDS('dftable3.RDS')

#---------
table3.NM <- dftable3 %>%
  select(HR, time_point)%>%
  tbl_summary(
    by = time_point,
    statistic =
      list(
        all_continuous() ~ "{mean} ({sd})",
        all_dichotomous() ~ "{n} ({p}%)"
      ),
    missing = "no",
    digits = all_continuous() ~ 1
  ) %>%
  add_difference(test=everything() ~ "ancova", adj.vars=c(HR), pvalue_fun = ~style_pvalue(.x, digits = 2))
table3.NM  
#---------
table3.NM <- dftable3 %>%
  select(RRsys, time_point)%>%
  tbl_summary(
    by = time_point,
    statistic =
      list(
        all_continuous() ~ "{mean} ({sd})",
        all_dichotomous() ~ "{n} ({p}%)"
      ),
    missing = "no",
    digits = all_continuous() ~ 1
  ) %>%
  add_difference(test=everything() ~ "ancova", adj.vars=c(RRsys), pvalue_fun = ~style_pvalue(.x, digits = 2))
table3.NM  
#---------
table3.NM <- dftable3 %>%
  select(RRdia, time_point)%>%
  tbl_summary(
    by = time_point,
    statistic =
      list(
        all_continuous() ~ "{mean} ({sd})",
        all_dichotomous() ~ "{n} ({p}%)"
      ),
    missing = "no",
    digits = all_continuous() ~ 1
  ) %>%
  add_difference(test=everything() ~ "ancova", adj.vars=c(RRdia), pvalue_fun = ~style_pvalue(.x, digits = 2))
table3.NM  
#---------

table3.NM <- dftable3 %>%
  select(CRP, time_point)%>%
  tbl_summary(
    by = time_point,
    statistic =
      list(
        all_continuous() ~ "{mean} ({sd})",
        all_dichotomous() ~ "{n} ({p}%)"
      ),
    missing = "no",
    digits = all_continuous() ~ 1
  ) %>%
  add_difference(test=everything() ~ "ancova", adj.vars=c(CRP), pvalue_fun = ~style_pvalue(.x, digits = 2))
table3.NM  
#---------

table3.NM <- dftable3 %>%
  select(TNT, time_point)%>%
  tbl_summary(
    by = time_point,
    statistic =
      list(
        all_continuous() ~ "{mean} ({sd})",
        all_dichotomous() ~ "{n} ({p}%)"
      ),
    missing = "no",
    digits = all_continuous() ~ 1
  ) %>%
  add_difference(test=everything() ~ "ancova", adj.vars=c(TNT), pvalue_fun = ~style_pvalue(.x, digits = 2))
table3.NM  

#---------

table3.NM <- dftable3 %>%
  select(BNP, time_point)%>%
  tbl_summary(
    by = time_point,
    statistic =
      list(
        all_continuous() ~ "{mean} ({sd})",
        all_dichotomous() ~ "{n} ({p}%)"
      ),
    missing = "no",
    digits = all_continuous() ~ 1
  ) %>%
  add_difference(test=everything() ~ "ancova", adj.vars=c(BNP), pvalue_fun = ~style_pvalue(.x, digits = 2))
table3.NM  

#---------
table3.NM <- dftable3 %>%
  select(LVEF, time_point)%>%
  tbl_summary(
    by = time_point,
    statistic =
      list(
        all_continuous() ~ "{mean} ({sd})",
        all_dichotomous() ~ "{n} ({p}%)"
      ),
    missing = "no",
    digits = all_continuous() ~ 1
  ) %>%
  add_difference(test=everything() ~ "ancova", adj.vars=c(LVEF), pvalue_fun = ~style_pvalue(.x, digits = 2))

table3.NM  
#---------
table3.NM <- dftable3 %>%
  select(GLS, time_point)%>%
  tbl_summary(
    by = time_point,
    statistic =
      list(
        all_continuous() ~ "{mean} ({sd})",
        all_dichotomous() ~ "{n} ({p}%)"
      ),
    missing = "no",
    digits = all_continuous() ~ 1
  ) %>%
  add_difference(test=everything() ~ "ancova", adj.vars=c(GLS), pvalue_fun = ~style_pvalue(.x, digits = 2))
table3.NM  
#---------
table3.NM <- dftable3 %>%
  select(EDVi, time_point)%>%
  tbl_summary(
    by = time_point,
    statistic =
      list(
        all_continuous() ~ "{mean} ({sd})",
        all_dichotomous() ~ "{n} ({p}%)"
      ),
    missing = "no",
    digits = all_continuous() ~ 1
  ) %>%
  add_difference(test=everything() ~ "ancova", adj.vars=c(EDVi), pvalue_fun = ~style_pvalue(.x, digits = 2))
table3.NM  
#---------
table3.NM <- dftable3 %>%
  select(LVMi, time_point)%>%
  tbl_summary(
    by = time_point,
    statistic =
      list(
        all_continuous() ~ "{mean} ({sd})",
        all_dichotomous() ~ "{n} ({p}%)"
      ),
    missing = "no",
    digits = all_continuous() ~ 1
  ) %>%
  add_difference(test=everything() ~ "ancova", adj.vars=c(LVMi), pvalue_fun = ~style_pvalue(.x, digits = 2))
table3.NM  
#---------
table3.NM <- dftable3 %>%
  select(RVEF, time_point)%>%
  tbl_summary(
    by = time_point,
    statistic =
      list(
        all_continuous() ~ "{mean} ({sd})",
        all_dichotomous() ~ "{n} ({p}%)"
      ),
    missing = "no",
    digits = all_continuous() ~ 1
  ) %>%
  add_difference(test=everything() ~ "ancova", adj.vars=c(RVEF), pvalue_fun = ~style_pvalue(.x, digits = 2))
table3.NM  
#---------
table3.NM <- dftable3 %>%
  select(T1, time_point)%>%
  tbl_summary(
    by = time_point,
    statistic =
      list(
        all_continuous() ~ "{mean} ({sd})",
        all_dichotomous() ~ "{n} ({p}%)"
      ),
    missing = "no",
    digits = all_continuous() ~ 1
  ) %>%
  add_difference(test=everything() ~ "ancova", adj.vars=c(T1), pvalue_fun = ~style_pvalue(.x, digits = 2))
table3.NM  
#---------
table3.NM <- dftable3 %>%
  select(T2, time_point)%>%
  tbl_summary(
    by = time_point,
    statistic =
      list(
        all_continuous() ~ "{mean} ({sd})",
        all_dichotomous() ~ "{n} ({p}%)"
      ),
    missing = "no",
    digits = all_continuous() ~ 1
  ) %>%
  add_difference(test=everything() ~ "ancova", adj.vars=c(T2), pvalue_fun = ~style_pvalue(.x, digits = 2))
table3.NM  
