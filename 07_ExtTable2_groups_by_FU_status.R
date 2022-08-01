library(gtsummary)

dfbl.cov <- readRDS('dfbl.cov.RDS')

add_stat_pairwise <- function(data, variable, by, ...) {
  pw <- pairwise.t.test(data[[variable]], data[[by]], p.adj = "bonferroni")
   index <- 0L
  p.value.list <- list()
  for (i in seq_len(nrow(pw$p.value))) {
    for (j in seq_len(nrow(pw$p.value))) {
      index <- index + 1L
      p.value.list[[index]] <- 
        c(pw$p.value[i, j]) %>%
        setNames(glue::glue("**{colnames(pw$p.value)[j]} vs. {rownames(pw$p.value)[i]}**"))
    }
  }
    p.value.list %>% 
    unlist() %>%
    purrr::discard(is.na) %>%
    t() %>%
    as.data.frame() %>%
     dplyr::mutate(dplyr::across(everything(), style_pvalue))
}

dftable2ext <- dfbl.cov[,c('Age','Gender', 'BMI','Hypertension', 'Diabetes', 'Hypercholesterolemia', 'Smoking', 'HR', 'RRsys', 'RRdia',
                 'CRP','TNT', 'TNTcat','BNP','LVEF','LVEFcat','varid_1733','EDVi','EDVicat', 'LVMi','RVEF','RVEFcat','T1', 'T2',
                 'LGEblbin','LGEbl.factor', 'Pericardial_Effusion_bin', 'Pericardial_Effusion','Pericardial_Enhancement',
                 'shared2')]
dftable2ext %>%
  ungroup()
table2.ext.NM <- dftable2ext %>%
  tbl_summary(
    by = shared2,
    statistic =
      list(
        all_continuous() ~ "{mean}±{sd}",
        all_dichotomous() ~ "{n} ({p}%)",
        'BNP' ~ "{median} [{p25}, {p75}]",
        'TNT' ~ "{median} [{p25}, {p75}]",
        'CRP' ~ "{median} [{p25}, {p75}]"
      ),
    missing = "no",
    digits = all_continuous() ~ 1,
    label = list (Gender ~ 'Gender (male)', Age ~ 'Age (years)', BMI ~ 'BMI (kg/m2)', HR ~ 'Heart Rate (beats/min)', RRsys ~ 'Systolic Blood Pressure (mmHg)',
                  RRdia ~'Diastolic Blood Pressure (mmHg)', CRP ~ 'CRP (mg/dl)', TNT ~ 'Hs-TropT (pg/ml)', TNTcat ~'TNT (by category)', BNP ~'NT proBNP (pg/ml)',
                  LVEFcat ~ 'LVEF (by category)', varid_1733 ~'Global Longitudinal Strain (%)', EDVi ~ 'EDVi (ml/m2)', EDVicat ~ 'EDVi (by category)', LVMi ~ 'Left Ventricular Mass (ml/m2)',
                  RVEFcat ~ 'RVEF (by category)', T1 ~'Native T1 (ms)', T2~ 'Native T2 (ms)', LGEblbin ~ 'LGE (any)', LGEbl.factor ~'LGE (by type)', 
                  Pericardial_Enhancement ~'Pericardial Enhancement', Pericardial_Effusion_bin ~'Pericardial Effusion (any detectable)', 'Pericardial_Effusion' ~ 'Pericardial Effusion (by amount)')
  ) %>%
  add_p(test = list(all_continuous() ~ "aov", contains ('BNP') ~ 'kruskal.test', contains ('TNT') ~ 'kruskal.test', contains ('CRP') ~ 'kruskal.test', matches('TNTcat')~'fisher.test'), pvalue_fun = ~style_pvalue(.x, digits = 2))%>%
  separate_p_footnotes()
table2.ext.NM
