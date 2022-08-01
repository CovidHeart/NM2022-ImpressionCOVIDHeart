library(gtsummary)
library(dplyr)

dftable2.right <- readRDS('dftable2.right.RDS')

add_stat_pairwise <- function(data, variable, by, ...) {
  pw <- pairwise.fisher.test((data[[variable]]), (data[[by]]), p.adjust.method = 'bonferroni')
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
    dplyr::mutate(dplyr::across(everything()))
}

table2.right.NM <- dftable2.right %>%
  tbl_summary(
    by = PatientGroups2,
    statistic =
      all_dichotomous() ~ "{n} ({p}%)",
    missing = "ifany",
    digits = all_continuous() ~ 1,
    label = list (SOB ~'Shortness of Breath', palpitations~'Palpitations', chestpain~'Chest Pain', syncope~'Syncope', PCFS~'PCFS')
  ) %>%
  add_p(test = list(all_categorical()~'chisq.test'), pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  add_stat(everything() ~ add_stat_pairwise)
table2.right.NM
