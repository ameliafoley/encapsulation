#Aim 2.2 qPCR Mass Balance Summary stat tables
#must run 2.2 qPCR summary state tables FIRST 
##MASS BALANCE FOR QPCR DATA#

library(dplyr)
library(tidyr)

df <- qpcr_plot #was qpcr_avg_ind before updating based on LOD of 50 cp.uL

# standardize sample.type values so 'planktonic' maps to 'supernatant'
df <- df %>%
  rename(sample_type = sample.type,
         gc_per_mL = gene_copies) %>%
  mutate(sample_type = tolower(sample_type),
         sample_type = case_when(
           sample_type %in% c("planktonic", "plank", "liquid") ~ "supernatant",
           sample_type %in% c("supernatant", "super") ~ "supernatant",
           sample_type %in% c("capsule", "caps") ~ "capsule",
           TRUE ~ sample_type
         ))

# pivot: include strain so mass-balance is per-strain
wide <- df %>%
  pivot_wider(
    id_cols = c(consortia, day, rep, treatment, strain),
    names_from = sample_type,
    values_from = c(gc_per_mL, sd_sq, n),
    values_fn = mean,
    values_fill = NA
  )

# normalize column names
wide <- wide %>%
  rename(gc_super = gc_per_mL_supernatant,
         gc_capsule = gc_per_mL_capsule,
         sd2_super = sd_sq_supernatant,
         sd2_capsule = sd_sq_capsule,
         n_super = n_supernatant,
         n_capsule = n_capsule)

# assign volumes: any reactor labelled encapsulated -> V_caps = 2, else 0
wide <- wide %>%
  mutate(
    is_encapsulated = tolower(treatment) == "encapsulated",
    V_super = 10,
    V_caps  = if_else(is_encapsulated, 2, 0),
    V_total = V_super + V_caps
  )

# compute totals per strain
wide <- wide %>%
  mutate(
    gc_super = as.numeric(gc_super),
    gc_capsule = as.numeric(gc_capsule),
    
    total_copies = case_when(
      !is.na(gc_super) & !is.na(gc_capsule) ~ gc_super * V_super + gc_capsule * V_caps,
      !is.na(gc_super) &  is.na(gc_capsule) ~ gc_super * V_super,
      is.na(gc_super)  & !is.na(gc_capsule) ~ gc_capsule * V_caps,
      TRUE ~ NA_real_
    ),
    
    gc_total_per_mL = if_else(!is.na(total_copies), total_copies / V_total, NA_real_),
    
    pct_in_capsule = case_when(
      !is.na(total_copies) & !is.na(gc_capsule) ~ (gc_capsule * V_caps) / total_copies * 100,
      TRUE ~ NA_real_
    )
  )

# Inspect a few planktonic rows to confirm gc_total_per_mL is present
wide %>% filter(tolower(treatment) == "free" | tolower(treatment) == "planktonic") %>%
  select(consortia, day, rep, treatment, strain, gc_super, gc_capsule, V_total, total_copies, gc_total_per_mL) %>%
  head(20) %>% print(n = 20)

###############################################################
# qPCR MASS BALANCE WORKBOOK
###############################################################

library(openxlsx)
library(dplyr)
library(broom)
library(emmeans)

wb_mass <- createWorkbook()

###############################################################
# Statistical dataset
###############################################################

mass_stats <-
  
  wide %>%
  
  mutate(
    
    log_gc = log10(gc_total_per_mL),
    
    day = factor(
      day,
      levels = c(0,7,14,21,42)
    ),
    
    treatment = factor(
      treatment,
      levels = c("free","encapsulated")
    ),
    
    consortia = factor(
      consortia,
      levels = c("k","m","r")
    ),
    
    strain = factor(strain)
    
  )

###############################################################
# Means ± SE
###############################################################

means_summary <-
  
  mass_stats %>%
  
  group_by(
    
    consortia,
    treatment,
    strain,
    day
    
  ) %>%
  
  summarise(
    
    Mean =
      mean(
        gc_total_per_mL,
        na.rm=TRUE
      ),
    
    SE =
      sd(
        gc_total_per_mL,
        na.rm=TRUE
      )/sqrt(n()),
    
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
    
  )

addWorksheet(
  wb_mass,
  "Means_SE"
)

writeData(
  wb_mass,
  "Means_SE",
  means_summary
)
###############################################################
# Global ANOVA
###############################################################

community_fit <- aov(
  log_gc ~
    consortia *
    treatment *
    day,
  data = mass_stats
)

community_anova <-
  
  broom::tidy(
    community_fit
  ) %>%
  
  mutate(
    
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
    
  )

addWorksheet(
  wb_mass,
  "Global_ANOVA"
)

writeData(
  wb_mass,
  "Global_ANOVA",
  community_anova
)

###############################################################
# Analyze one consortium
###############################################################

analyze_consortium <- function(dat){
  
  fit <-
    
    aov(
      
      log_gc ~
        
        strain *
        
        treatment *
        
        day,
      
      data = dat
      
    )
  
  ###########################################################
  # Treatment comparisons
  ###########################################################
  
  emm_treatment <-
    
    emmeans(
      
      fit,
      
      ~ treatment |
        
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
        
        strain * treatment
      
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
# Run consortium analyses
###############################################################

stats_k <-
  
  analyze_consortium(
    
    filter(
      mass_stats,
      consortia == "k"
    )
    
  )

stats_m <-
  
  analyze_consortium(
    
    filter(
      mass_stats,
      consortia == "m"
    )
    
  )

stats_r <-
  
  analyze_consortium(
    
    filter(
      mass_stats,
      consortia == "r"
    )
    
  )

###############################################################
# ANOVA by Consortium
###############################################################

anova_by_consortium <-
  
  bind_rows(
    
    broom::tidy(stats_k$fit) %>%
      mutate(Consortium = "K"),
    
    broom::tidy(stats_m$fit) %>%
      mutate(Consortium = "Mixed"),
    
    broom::tidy(stats_r$fit) %>%
      mutate(Consortium = "R")
    
  ) %>%
  
  filter(
    term %in% c(
      "treatment",
      "day",
      "treatment:day", 
      "strain",
      "strain:day", 
      "strain:treatment", 
      "strain:treatment:day"
    )
  ) %>%
  
  mutate(
    
    sig = case_when(
      
      p.value < 0.001 ~ "***",
      
      p.value < 0.01 ~ "**",
      
      p.value < 0.05 ~ "*",
      
      TRUE ~ "ns"
      
    )
    
  ) %>%
  
  arrange(
    
    Consortium,
    
    term
    
  )

addWorksheet(
  wb_mass,
  "ANOVA_by_Consortium"
)

writeData(
  wb_mass,
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
      
      p.value < 0.01 ~ "**",
      
      p.value < 0.05 ~ "*",
      
      TRUE ~ "ns"
      
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
  wb_mass,
  "Treatment_Comparisons"
)

writeData(
  wb_mass,
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
    
    treatment,
    
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
    
    treatment
    
  )

addWorksheet(
  wb_mass,
  "Day_Comparisons"
)

writeData(
  wb_mass,
  "Day_Comparisons",
  day_comparisons
)

###############################################################
# Analyze shared strains
###############################################################

analyze_strain <- function(strain_name){
  
  dat <-
    
    mass_stats %>%
    
    filter(
      
      strain == strain_name
      
    )
  
  fit <-
    
    aov(
      
      log_gc ~
        
        consortia *
        
        treatment *
        
        day,
      
      data = dat
      
    )
  
  ###########################################################
  # Consortium comparisons
  ###########################################################
  
  emm_consortia <-
    
    emmeans(
      
      fit,
      
      ~ consortia |
        
        treatment * day
      
    )
  
  consortia_tests <-
    
    pairs(
      
      emm_consortia,
      
      adjust = "tukey"
      
    )
  
  list(
    
    fit = fit,
    
    consortia_tests = consortia_tests
    
  )
  
}

###############################################################
# Shared strains
###############################################################

stats_putida <-
  analyze_strain("p.putida")

stats_npenta <-
  analyze_strain("n.penta")

stats_sphingo <-
  analyze_strain("sphingo.sp")

stats_presin <-
  analyze_strain("p.resin")

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
      
      p.value < 0.01 ~ "**",
      
      p.value < 0.05 ~ "*",
      
      TRUE ~ "ns"
      
    )
    
  ) %>%
  
  select(
    
    Strain,
    
    treatment,
    
    day,
    
    contrast,
    
    estimate,
    
    SE,
    
    df,
    
    p.value,
    
    sig
    
  ) %>%
  
  arrange(
    
    Strain,
    
    treatment,
    
    day
    
  )

addWorksheet(
  wb_mass,
  "Consortium_Comparisons"
)

writeData(
  wb_mass,
  "Consortium_Comparisons",
  consortium_comparisons
)
###############################################################
# ANOVA by Shared Strain
###############################################################

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
  
  filter(
    
    term %in% c(
      
      "consortia",
      
      "treatment",
      
      "day",
      
      "consortia:treatment",
      
      "consortia:day",
      
      "treatment:day",
      
      "consortia:treatment:day"
      
    )
    
  ) %>%
  
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
  wb_mass,
  "ANOVA_by_Shared_Strain"
)

writeData(
  wb_mass,
  "ANOVA_by_Shared_Strain",
  anova_shared_strains
)
###############################################################
# Save Workbook
###############################################################

saveWorkbook(
  
  wb_mass,
  
  "Aim2.2_qPCR_MassBalance_Statistics.xlsx",
  
  overwrite = TRUE
  
)