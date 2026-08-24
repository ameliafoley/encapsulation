##stats for aim 2.2 qpcr
#fresh wb
wb <- createWorkbook()
qpcr_plot <- qpcr_avg_ind %>%
  mutate(
    below_lod = mean_sq < LOD,
    
    gene_copies =
      if_else(
        mean_sq < LOD,
        (50*150/1)/2, #because for all samples where this is a concern, they are supernatant, and this was the calculation
        gene_copies_per_mL
      )
  )
###############################################################
# Statistical dataset (LOD/2 substitution)
###############################################################

qpcr_stats <- qpcr_plot %>%
  mutate(
    log_gc = log10(gene_copies),
    day = factor(day, levels = c(0, 7, 14, 21, 42)),
    treatment = factor(treatment, levels = c("free", "encapsulated")),
    consortia = factor(consortia, levels = c("k", "m", "r")),
    strain = factor(strain)
  )

k_data <- qpcr_stats %>%
  filter(consortia == "k")

m_data <- qpcr_stats %>%
  filter(consortia == "m")

r_data <- qpcr_stats %>%
  filter(consortia == "r")

qpcr_comm <- qpcr_stats %>%
  mutate(
    day = factor(day, levels = c(0,7,14,21,42)),
    treatment = factor(treatment,
                       levels = c("free","encapsulated")),
    consortia = factor(consortia,
                       levels = c("k","m","r"))
  )
###############################################################
# qPCR COMPARTMENT WORKBOOK
###############################################################

library(openxlsx)
library(broom)
library(dplyr)

wb_qpcr <- createWorkbook()

###############################################################
# Means ± SE
###############################################################

means_summary <-
  
  qpcr_stats %>%
  
  group_by(
    
    consortia,
    treatment,
    sample.type,
    strain,
    day
    
  ) %>%
  
  summarise(
    
    Mean =
      mean(gene_copies,
           na.rm=TRUE),
    
    SE =
      sd(gene_copies,
         na.rm=TRUE)/
      sqrt(n()),
    
    n=n(),
    
    .groups="drop"
    
  ) %>%
  
  mutate(
    
    Mean_SE =
      sprintf(
        "%.2e ± %.2e",
        Mean,
        SE
      )
    
  ) %>%
  
  arrange(
    
    consortia,
    strain,
    sample.type,
    treatment,
    day
    
  )

addWorksheet(
  wb_qpcr,
  "Means_SE"
)

writeData(
  wb_qpcr,
  "Means_SE",
  means_summary
)
###############################################################
# Community Global ANOVA
###############################################################

###############################################################
# Community-level qPCR ANOVA
###############################################################

community_fit <- aov(
  log_gc ~
    consortia *
    treatment *
    sample.type *
    day,
  data = qpcr_stats
)

summary(community_fit)
community_anova <-
  
  broom::tidy(
    community_fit
  ) %>%
  
  mutate(
    
    sig=
      
      case_when(
        
        p.value<0.001~"***",
        
        p.value<0.01~"**",
        
        p.value<0.05~"*",
        
        TRUE~"ns"
        
      )
    
  )

addWorksheet(
  wb_qpcr,
  "Community_Global_ANOVA"
)

writeData(
  wb_qpcr,
  "Community_Global_ANOVA",
  community_anova
)
###############################################################
# ANOVA by Consortium
###############################################################

###############################################################
# Analyze one consortium
###############################################################

analyze_consortium <- function(dat){
  
  fit <-
    
    aov(
      
      log_gc ~
        
        strain *
        
        treatment *
        
        sample.type*
        
        day,
      
      data = dat
      
    )
  
  ###########################################################
  # Treatment comparisons
  ###########################################################
  
  emm_treatment <-
    
    emmeans(
      
      fit,
      
      ~ sample.type |
        
        strain * day
      
    )
  
  treatment_tests <-
    
    pairs(
      
      emm_treatment,
      
      adjust = "tukey"
      
    )
  
  ###########################################################
  # Day comparisons
  ###########################################################
  
  emm_day <-
    
    emmeans(
      
      fit,
      
      ~ day |
        
        strain * sample.type
      
    )
  
  day_tests <-
    
    pairs(
      
      emm_day,
      
      adjust = "tukey"
      
    )
  
  list(
    
    fit = fit,
    
    treatment_tests = treatment_tests,
    
    day_tests = day_tests
    
  )
  
}
###############################################################
# Consortium analyses
###############################################################

stats_k <-
  
  analyze_consortium(
    
    filter(
      
      qpcr_stats,
      
      consortia == "k"
      
    )
    
  )

stats_m <-
  
  analyze_consortium(
    
    filter(
      
      qpcr_stats,
      
      consortia == "m"
      
    )
    
  )

stats_r <-
  
  analyze_consortium(
    
    filter(
      
      qpcr_stats,
      
      consortia == "r"
      
    )
    
  )
anova_by_consortium <-
  
  bind_rows(
    
    broom::tidy(stats_k$fit) %>%
      mutate(Consortium = "K"),
    
    broom::tidy(stats_m$fit) %>%
      mutate(Consortium = "Mixed"),
    
    broom::tidy(stats_r$fit) %>%
      mutate(Consortium = "R")
    
  ) %>%
  
  # filter(
  #   term %in% c(
  #     "treatment",
  #     "day",
  #     "treatment:day", 
  #     "strain",
  #     "strain:day", 
  #     "strain:treatment", 
  #     "strain:treatment:day"
  #   )
  # ) %>%
  
  mutate(
    
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
    
  ) %>%
  
  select(
    
    Consortium,
    
    term,
    
    df,
    
    statistic,
    
    p.value,
    
    sig
    
  ) %>%
  
  arrange(
    Consortium,
    term
  )

addWorksheet(
  wb_qpcr,
  "ANOVA_by_Consortium"
)

writeData(
  wb_qpcr,
  "ANOVA_by_Consortium",
  anova_by_consortium
)
###############################################################
# Treatment Comparisons
###############################################################

treatment_comparisons <-
  
  bind_rows(
    
    summary(stats_k$treatment_tests) %>%
      as.data.frame() %>%
      mutate(Consortium = "K"),
    
    summary(stats_m$treatment_tests) %>%
      as.data.frame() %>%
      mutate(Consortium = "Mixed"),
    
    summary(stats_r$treatment_tests) %>%
      as.data.frame() %>%
      mutate(Consortium = "R")
    
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
    Consortium,
    strain,
    day,
    contrast,
    estimate,
    SE,
    df,
    p.value,
    sig
  ) %>%
  
  arrange(
    
    Consortium,
    
    strain,
    
    day
    
  )

addWorksheet(
  wb_qpcr,
  "Treatment_Comparisons"
)

writeData(
  wb_qpcr,
  "Treatment_Comparisons",
  treatment_comparisons
)
###############################################################
# Day Comparisons
###############################################################

day_comparisons <-
  
  bind_rows(
    summary(stats_k$day_tests) %>%
      as.data.frame() %>%
      mutate(Consortium = "K"),
    
    summary(stats_m$day_tests) %>%
      as.data.frame() %>%
      mutate(Consortium = "Mixed"),
    
    summary(stats_r$day_tests) %>%
      as.data.frame() %>%
      mutate(Consortium = "R")
  ) %>%
  
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  
  select(
    Consortium,
    strain,
    sample.type,
    contrast,
    estimate,
    SE,
    df,
    p.value,
    sig
  ) %>%
  
  arrange(
    Consortium,
    strain,
    sample.type
  )

addWorksheet(
  wb_qpcr,
  "Day_Comparisons"
)

writeData(
  wb_qpcr,
  "Day_Comparisons",
  day_comparisons
)
###############################################################
# ANOVA by Shared Strain
###############################################################

###############################################################
# Analyze one shared strain
###############################################################

analyze_strain <- function(strain_name){
  
  dat <-
    
    qpcr_stats %>%
    
    filter(
      
      strain == strain_name
      
    )
  
  fit <-
    
    aov(
      
      log_gc ~
        
        consortia *
        
        sample.type *
        
        day,
      
      data = dat
      
    )
  
  ###########################################################
  # Treatment comparisons
  ###########################################################
  
  emm_treatment <-
    
    emmeans(
      
      fit,
      
      ~ sample.type |
        
        consortia * day
      
    )
  
  treatment_tests <-
    
    pairs(
      
      emm_treatment,
      
      adjust = "tukey"
      
    )
  
  ###########################################################
  # Consortium comparisons
  ###########################################################
  
  emm_consortia <-
    
    emmeans(
      
      fit,
      
      ~ consortia |
        
        sample.type * day
      
    )
  
  consortia_tests <-
    
    pairs(
      
      emm_consortia,
      
      adjust = "tukey"
      
    )
  
  
  ###########################################################
  # Day comparisons
  ###########################################################
  
  emm_day <-
    
    emmeans(
      
      fit,
      
      ~ day |
        
        consortia * sample.type
      
    )
  
  day_tests <-
    
    pairs(
      
      emm_day,
      
      adjust = "tukey"
      
    )
  
  list(
    
    fit = fit,
    
    treatment_tests = treatment_tests,
    
    consortia_tests = consortia_tests,
    
    day_tests = day_tests
    
  )
  
}


###############################################################
# Shared strains
###############################################################

stats_putida <-
  
  analyze_strain(
    
    "p.putida"
    
  )

stats_npenta <-
  
  analyze_strain(
    
    "n.penta"
    
  )

stats_sphingo <-
  
  analyze_strain(
    
    "sphingo.sp"
    
  )

stats_presin <-
  
  analyze_strain(
    
    "p.resin"
    
  )
anova_shared_strains <-
  
  bind_rows(
    
    broom::tidy(stats_putida$fit) %>%
      mutate(Strain = "P. putida"),
    
    broom::tidy(stats_npenta$fit) %>%
      mutate(Strain = "N. pentaromativorans"),
    
    broom::tidy(stats_sphingo$fit) %>%
      mutate(Strain = "Sphingomonas sp."),
    
    broom::tidy(stats_presin$fit) %>%
      mutate(Strain = "P. resinovorans")
    
  ) %>%
  
  # filter(
  #   
  #   term %in% c(
  #     
  #     "consortia",
  #     
  #     "sample.type",
  #     
  #     "day",
  #     
  #     "consortia:treatment",
  #     
  #     "consortia:day",
  #     
  #     "treatment:day",
  #     
  #     "consortia:treatment:day"
  #     
  #   )
  #   
  # ) %>%
  
  mutate(
    
    sig = case_when(
      
      p.value < 0.001 ~ "***",
      
      p.value < 0.01 ~ "**",
      
      p.value < 0.05 ~ "*",
      
      TRUE ~ "ns"
      
    )
    
  ) %>%
  
  arrange(
    Strain,
    term
  )

addWorksheet(
  wb_qpcr,
  "ANOVA_by_Shared_Strain"
)

writeData(
  wb_qpcr,
  "ANOVA_by_Shared_Strain",
  anova_shared_strains
)
###############################################################
# Consortium Comparisons
###############################################################

consortium_comparisons <-
  
  bind_rows(
    
    summary(stats_putida$consortia_tests) %>%
      as.data.frame() %>%
      mutate(Strain = "P. putida"),
    
    summary(stats_npenta$consortia_tests) %>%
      as.data.frame() %>%
      mutate(Strain = "N. pentaromativorans"),
    
    summary(stats_sphingo$consortia_tests) %>%
      as.data.frame() %>%
      mutate(Strain = "Sphingomonas sp."),
    
    summary(stats_presin$consortia_tests) %>%
      as.data.frame() %>%
      mutate(Strain = "P. resinovorans")
    
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
    day,
    Strain,
    sample.type,
    contrast,
    estimate,
    SE,
    df,
    p.value,
    sig
  ) %>%
  
  arrange(
    
    Strain,
    
    sample.type,
    
    day
    
  )

addWorksheet(
  wb_qpcr,
  "Consortium_Comparisons"
)

writeData(
  wb_qpcr,
  "Consortium_Comparisons",
  consortium_comparisons
)

###############################################################
# Save Workbook
###############################################################

saveWorkbook(
  
  wb_qpcr,
  
  "Aim2.2_qPCR_Compartment_Statistics.xlsx",
  
  overwrite = TRUE
  
)