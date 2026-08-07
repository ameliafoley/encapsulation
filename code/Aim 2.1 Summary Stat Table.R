##Aim 2.1 statistical tables for writing
##run Aim 2.1 Figs for Paper-20260730 FIRST so that this code pulls the correct data frames
library(broom)

anova_table_fla <-
  tidy(fla_anova) %>%
  mutate(
    PAH = "Fluoranthene",
    Significant = ifelse(p.value < 0.05, "Yes", "No")
  ) %>%
  select(
    PAH,
    term,
    df,
    statistic,
    p.value,
    Significant
  )

anova_table_fla
anova_table_phe <-
  tidy(phe_anova) %>%
  mutate(
    PAH="Phenanthrene",
    Significant=ifelse(p.value<0.05,"Yes","No")
  )

anova_table_nap <-
  tidy(nap_anova) %>%
  mutate(
    PAH="Naphthalene",
    Significant=ifelse(p.value<0.05,"Yes","No")
  )
anova_summary <-
  bind_rows(
    anova_table_fla,
    anova_table_phe,
    anova_table_nap
  )

anova_summary #table 1
##table 2
table2_fla <-
  summary(tuk_fla) %>%
  as.data.frame() %>%
  mutate(PAH="Fluoranthene")

table2_fla
table2_phe <-
  summary(tuk_phe) %>%
  as.data.frame() %>%
  mutate(PAH="Phenanthrene")

table2_nap <-
  summary(tuk_nap) %>%
  as.data.frame() %>%
  mutate(PAH="Naphthalene")
tukey_summary <-
  bind_rows(
    table2_fla,
    table2_phe,
    table2_nap
  )

tukey_summary
#make table easier to read
tukey_summary <-
  tukey_summary %>%
  mutate(
    sig =
      case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ "ns"
      )
  )
#filter only significant comparisons
tukey_results <-
  tukey_summary %>%
  filter(
    p.value < 0.05
  ) %>%
  arrange(PAH,strain,p.value)
#dunnet table
#rerun code from within function
emm_control_fla <-
  emmeans(fla_anova,
          ~strain|treatment*day)

dunnett_fla <-
  contrast(
    emm_control_fla,
    method="trt.vs.ctrl",
    ref="abiotic",
    adjust="dunnett"
  )

#rerun phe
emm_control_phe <-
  emmeans(phe_anova,
          ~strain|treatment*day)

dunnett_phe <-
  contrast(
    emm_control_phe,
    method="trt.vs.ctrl",
    ref="abiotic",
    adjust="dunnett"
  )

##rereun nap
emm_control_nap <-
  emmeans(nap_anova,
          ~strain|treatment*day)

dunnett_nap <-
  contrast(
    emm_control_nap,
    method="trt.vs.ctrl",
    ref="abiotic",
    adjust="dunnett"
  )

table3_fla <-
  summary(dunnett_fla) %>%
  as.data.frame() %>%
  mutate(PAH = "Fluoranthene")

table3_phe <-
  summary(dunnett_phe) %>%
  as.data.frame() %>%
  mutate(PAH = "Phenanthrene")

table3_nap <-
  summary(dunnett_nap) %>%
  as.data.frame() %>%
  mutate(PAH = "Naphthalene")

dunnett_summary <-
  bind_rows(
    table3_fla,
    table3_phe,
    table3_nap
  ) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  ) %>%
  select(
    PAH,
    treatment,
    day,
    contrast,
    estimate,
    SE,
    df,
    t.ratio,
    p.value,
    sig
  )

dunnett_summary

dunnett_summary
##add anovas by strain
anova_by_strain <-
  bind_rows(
    anova_results %>% mutate(PAH = "Fluoranthene"),
    anova_results_phe %>% mutate(PAH = "Phenanthrene"),
    anova_results_nap %>% mutate(PAH = "Naphthalene")
  ) %>%
  filter(
    (stratum == "sample" & term == "treatment") |
      (stratum == "sample:day" & term %in% c("day", "treatment:day"))
  ) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  ) %>%
  select(
    PAH,
    strain,
    term,
    df,
    statistic,
    p.value,
    sig
  ) %>%
  arrange(PAH, strain)
#summary stats for quick references (mean and SE)
mean_se_summary <-
  bind_rows(
    sum_fla %>%
      transmute(
        PAH = "Fluoranthene",
        strain,
        treatment,
        day,
        mean = fluoranthene_mean,
        SE = fluoranthene_se
      ),
    
    sum_phe %>%
      transmute(
        PAH = "Phenanthrene",
        strain,
        treatment,
        day,
        mean = phe_mean,
        SE = phe_se
      ),
    
    sum_nap %>%
      transmute(
        PAH = "Naphthalene",
        strain,
        treatment,
        day,
        mean = nap_mean,
        SE = nap_se
      )
  ) %>%
  arrange(PAH, strain, treatment, day)

mean_se_summary
#save to excel workbook
library(openxlsx)

wb <- createWorkbook()

addWorksheet(wb,"ANOVA")
writeData(wb,"ANOVA",anova_summary)

addWorksheet(wb, "ANOVA_by_strain")
writeData(wb, "ANOVA_by_strain", anova_by_strain)

addWorksheet(wb,"Tukey")
writeData(wb,"Tukey",tukey_summary)

addWorksheet(wb,"Dunnett")
writeData(wb,"Dunnett",dunnett_summary)

addWorksheet(wb, "Means_SE")
writeData(wb, "Means_SE", mean_se_summary)

saveWorkbook(
  wb,
  "Aim 2.1 PAH_statistics_summary.xlsx",
  overwrite=TRUE
)