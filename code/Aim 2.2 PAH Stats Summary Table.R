## Aim 2.2 summary stat tables
#Run Aim 2.2 Figs for Paper code FIRST

#fresh wb
wb <- createWorkbook()

##sheet 1 summary
means_summary <-
  summary_data %>%
  mutate(
    Mean_SE =
      sprintf("%.2f ± %.2f",
              mean,
              se)
  )

addWorksheet(wb,"Means_SE")
writeData(wb,"Means_SE",means_summary)

##sheet 2 global anova
library(broom)

global_fla <-
  tidy(stats_fla$fit) %>%
  mutate(
    Compound="Fluoranthene"
  )

global_phe <-
  tidy(stats_phe$fit) %>%
  mutate(
    Compound="Phenanthrene"
  )

global_nap <-
  tidy(stats_nap$fit) %>%
  mutate(
    Compound="Naphthalene"
  )

global_anova <-
  bind_rows(
    global_fla,
    global_phe,
    global_nap
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

addWorksheet(wb,"Global_ANOVA")
writeData(wb,"Global_ANOVA",global_anova)

#sheet 3 anova by consortium
anova_by_consortium <-
  
  long %>%
  
  group_by(compound,
           consortia) %>%
  
  do({
    
    fit <-
      
      aov(
        
        value~
          treatment*day,
        
        data=.
        
      )
    
    tidy(fit)
    
  }) %>%
  
  filter(
    
    term %in%
      
      c("treatment",
        "day",
        "treatment:day")
    
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

addWorksheet(wb,"ANOVA_by_consortium")

writeData(
  
  wb,
  
  "ANOVA_by_consortium",
  
  anova_by_consortium
  
)
##sheet 4 tukey letters
make_targeted_tukey <- function(fit, compound_name){
  
  ###########################################################
  # Treatment comparisons (within each day & consortium)
  ###########################################################
  
  emm_treatment <- emmeans(
    fit,
    ~ treatment | consortia * day
  )
  
  treatment_comp <-
    pairs(
      emm_treatment,
      adjust = "tukey"
    ) %>%
    summary() %>%
    as.data.frame() %>%
    mutate(
      Compound = compound_name,
      Comparison = "Treatment"
    )
  
  ###########################################################
  # Day comparisons (within each treatment & consortium)
  ###########################################################
  
  emm_day <-
    emmeans(
      fit,
      ~ day | consortia * treatment
    )
  
  day_comp <-
    pairs(
      emm_day,
      adjust = "tukey"
    ) %>%
    summary() %>%
    as.data.frame() %>%
    mutate(
      Compound = compound_name,
      Comparison = "Day"
    )
  
  ###########################################################
  # Combine and add significance
  ###########################################################
  
  bind_rows(
    treatment_comp,
    day_comp
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
      Compound,
      consortia,
      treatment,
      day,
      Comparison,
      contrast,
      estimate,
      SE,
      df,
      t.ratio,
      p.value,
      sig
    )
  
}

###############################################################
# Run for each PAH
###############################################################

tukey_summary <-
  
  bind_rows(
    
    make_targeted_tukey(
      stats_fla$fit,
      "Fluoranthene"
    ),
    
    make_targeted_tukey(
      stats_phe$fit,
      "Phenanthrene"
    ),
    
    make_targeted_tukey(
      stats_nap$fit,
      "Naphthalene"
    )
    
  )

###############################################################
# Add worksheet to workbook
###############################################################

addWorksheet(wb, "Targeted_Tukey")
writeData(wb, "Targeted_Tukey", tukey_summary)


##sheet 5 dunnett
dunnett_summary <-
  
  bind_rows(
    
    stats_fla$dunnett %>%
      mutate(Compound="Fluoranthene"),
    
    stats_phe$dunnett %>%
      mutate(Compound="Phenanthrene"),
    
    stats_nap$dunnett %>%
      mutate(Compound="Naphthalene")
    
  ) %>%
  
  mutate(
    
    sig = case_when(
      p.value < .001 ~ "***",
      p.value < .01  ~ "**",
      p.value < .05  ~ "*",
      TRUE ~ "ns"
    )
    
  )

addWorksheet(
  
  wb,
  
  "Dunnett"
  
)

writeData(
  
  wb,
  
  "Dunnett",
  
  dunnett_summary
  
)

##save workbook
saveWorkbook(
  
  wb,
  
  "Aim2.2_PAH_statistics_summary.xlsx",
  
  overwrite=TRUE
  
)