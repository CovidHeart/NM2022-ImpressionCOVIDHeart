library(gtsummary)

dftable1.left <- readRDS('dftable1.left.RDS')

table1.left.NM <- dftable1.left %>%
  tbl_summary(
    by = PatientGroups1,
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
table1.left.NM
