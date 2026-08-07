##run AFTER Aim 2.2 mass balance script

###############################################################
## STATISTICS WORKBOOK - MASS BALANCE
###############################################################

library(openxlsx)
library(broom)
library(dplyr)
library(emmeans)

wb_mass <- createWorkbook()

###############################################################
## SHEET 1 - Mean ± SE
###############################################################

means_mass <-
  
  mass_summary %>%
  
  transmute(
    
    Compound = compound,
    
    Consortium = consortia,
    
    Treatment = treatment_combined,
    
    Day = day,
    
    Mean = mean_perc_remaining,
    
    SE = se_perc_remaining,
    
    Mean_SE = sprintf("%.1f ± %.1f", Mean, SE)
    
  )

addWorksheet(wb_mass,"Means_SE")

writeData(
  wb_mass,
  "Means_SE",
  means_mass
)

###############################################################
## SHEET 2 - Global ANOVA
###############################################################

global_anova <-
  
  bind_rows(
    
    broom::tidy(stats_mass_fla$fit) %>%
      mutate(Compound="Fluoranthene"),
    
    broom::tidy(stats_mass_phe$fit) %>%
      mutate(Compound="Phenanthrene"),
    
    broom::tidy(stats_mass_nap$fit) %>%
      mutate(Compound="Naphthalene")
    
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

addWorksheet(wb_mass,"Global_ANOVA")

writeData(
  wb_mass,
  "Global_ANOVA",
  global_anova
)

###############################################################
## SHEET 3 - ANOVA BY CONSORTIUM
###############################################################

anova_by_consortium <-
  
  mass_balance %>%
  
  group_by(
    compound,
    consortia
  ) %>%
  
  do({
    
    fit<-
      
      aov(
        
        perc_remaining~
          
          treatment_combined*
          day,
        
        data=.
        
      )
    
    broom::tidy(fit)
    
  }) %>%
  
  filter(
    
    term %in%
      
      c(
        
        "treatment_combined",
        
        "day",
        
        "treatment_combined:day"
        
      )
    
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
  wb_mass,
  "ANOVA_by_consortium"
)

writeData(
  wb_mass,
  "ANOVA_by_consortium",
  anova_by_consortium
)

###############################################################
## FUNCTION FOR TARGETED TUKEY
###############################################################

make_targeted_tukey <- function(fit,
                                compound_name){
  
  ###########################################################
  ## Treatment comparisons
  ###########################################################
  
  emm_treatment <-
    
    emmeans(
      
      fit,
      
      ~treatment_combined|
        
        consortia*day
      
    )
  
  treatment_comp<-
    
    pairs(
      
      emm_treatment,
      
      adjust="tukey"
      
    )%>%
    
    summary()%>%
    
    as.data.frame()%>%
    
    mutate(
      
      Compound=compound_name,
      
      Comparison="Treatment",
      
      sig=
        
        case_when(
          
          p.value<0.001~"***",
          
          p.value<0.01~"**",
          
          p.value<0.05~"*",
          
          TRUE~"ns"
          
        )
      
    )
  
  ###########################################################
  ## Day comparisons
  ###########################################################
  
  emm_day<-
    
    emmeans(
      
      fit,
      
      ~day|
        
        consortia*treatment_combined
      
    )
  
  day_comp<-
    
    pairs(
      
      emm_day,
      
      adjust="tukey"
      
    )%>%
    
    summary()%>%
    
    as.data.frame()%>%
    
    mutate(
      
      Compound=compound_name,
      
      Comparison="Day",
      
      sig=
        
        case_when(
          
          p.value<0.001~"***",
          
          p.value<0.01~"**",
          
          p.value<0.05~"*",
          
          TRUE~"ns"
          
        )
      
    )
  
  list(
    
    Treatment=treatment_comp,
    
    Day=day_comp
    
  )
  
}

###############################################################
## RUN TUKEY
###############################################################

tuk_fla<-
  
  make_targeted_tukey(
    
    stats_mass_fla$fit,
    
    "Fluoranthene"
    
  )

tuk_phe<-
  
  make_targeted_tukey(
    
    stats_mass_phe$fit,
    
    "Phenanthrene"
    
  )

tuk_nap<-
  
  make_targeted_tukey(
    
    stats_mass_nap$fit,
    
    "Naphthalene"
    
  )

###############################################################
## SHEET 4 - Treatment comparisons
###############################################################

treatment_summary<-
  
  bind_rows(
    
    tuk_fla$Treatment,
    
    tuk_phe$Treatment,
    
    tuk_nap$Treatment
    
  )

addWorksheet(
  wb_mass,
  "Treatment_Comparisons"
)

writeData(
  wb_mass,
  "Treatment_Comparisons",
  treatment_summary
)

###############################################################
## SHEET 5 - Day comparisons
###############################################################

day_summary<-
  
  bind_rows(
    
    tuk_fla$Day,
    
    tuk_phe$Day,
    
    tuk_nap$Day
    
  )

addWorksheet(
  wb_mass,
  "Day_Comparisons"
)

writeData(
  wb_mass,
  "Day_Comparisons",
  day_summary
)

###############################################################
## SHEET 6 - Dunnett
###############################################################

make_dunnett_table<-function(x){
  
  x$dunnett%>%
    
    mutate(
      
      consortia=sub(" - a","",contrast),
      
      sig=
        
        case_when(
          
          p.value<0.001~"***",
          
          p.value<0.01~"**",
          
          p.value<0.05~"*",
          
          TRUE~"ns"
          
        )
      
    )
  
}

dunnett_summary<-
  
  bind_rows(
    
    make_dunnett_table(stats_mass_fla)%>%
      mutate(Compound="Fluoranthene"),
    
    make_dunnett_table(stats_mass_phe)%>%
      mutate(Compound="Phenanthrene"),
    
    make_dunnett_table(stats_mass_nap)%>%
      mutate(Compound="Naphthalene")
    
  )

addWorksheet(
  wb_mass,
  "Dunnett"
)

writeData(
  wb_mass,
  "Dunnett",
  dunnett_summary
)

###############################################################
## SAVE
###############################################################

saveWorkbook(
  
  wb_mass,
  
  "Aim2.2_MassBalance_Statistics.xlsx",
  
  overwrite=TRUE
  
)

##follow ups
###############################################################
# Compare consortia within treatment and day
###############################################################

emm_consortia <- emmeans(
  stats_mass_phe$fit,
  ~ consortia | treatment_combined * day
)

pairs(
  emm_consortia,
  adjust = "tukey"
)
