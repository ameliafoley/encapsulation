library(emmeans)
library(multcomp)
library(multcompView)

treatment_colors <- c(
  # Free / planktonic
  "free"        = "#D85A44FF",
  "planktonic"  = "#D85A44FF",
  "f"  = "#D85A44FF",
  
  # Aqueous / supernatant
  "aqueous"     = "#98A54FFF",
  "super"       = "#98A54FFF",
  "supernatant" = "#98A54FFF",
  
  # Capsule
  "capsule"     = "#2E92A2FF",
  "cap"         = "#2E92A2FF",
  "c"         = "#2E92A2FF",
  
  # Encapsulated aqueous
  "cap+aq"      = "#61BEA4FF",
  "encapsulated"= "#61BEA4FF"   # if this level exists anywhere
)

treatment_linetypes <- c(
  "free"        = "solid",
  "planktonic"  = "solid",
  "f"  = "solid",
  
  "aqueous"     = "dashed",
  "super"       = "dashed",
  "supernatant" = "dashed",
  
  "capsule"     = "dotted",
  "cap"         = "dotted",
  "c"         = "dotted",
  
  "cap+aq"      = "dotdash",
  "encapsulated"= "dotdash"
)

###############################################################
# FLUORANTHENE
###############################################################
#load data. 
fla <- read_excel(here::here("data", "Exp 2.1 fluor and code copy.xlsx"))
glimpse(fla)

#clean and remove h.taenio (contaminated with A. venet)
clean <- fla %>% dplyr::filter(sample.no != "6") %>% filter(strain != "h.taenio") %>% filter(strain != "m.fred")
clean$strain <- clean$strain %>% factor(levels = c("abiotic", 
                                                   "p.putida", 
                                                   "a.venet", 
                                                   "n.aroma", 
                                                   "n.penta"))
clean <- clean %>%
  mutate(
    day = factor(day,
                 levels = c(0, 21, 42))
  )
glimpse(clean)
sum_fla<- clean %>% group_by(strain, day, treatment) %>% summarise(fluoranthene_mean = mean(concentration.ngml), 
                                                                   fluoranthene_se = sd(concentration.ngml) / sqrt(n()), 
                                                                   )
                                                                   

fla_anova <- aov(concentration.ngml ~ strain * treatment * day,
                 data = clean)

summary(fla_anova)

###############################################################
# Estimated marginal means
###############################################################

emm_fla <- emmeans(
  fla_anova,
  ~ treatment * day | strain
)

###############################################################
# Tukey-adjusted pairwise comparisons
###############################################################

tuk_fla <- pairs(
  emm_fla,
  adjust = "tukey"
)

print(tuk_fla)

###############################################################
# Compact letter display
###############################################################

letters_fla <- cld(
  emm_fla,
  adjust = "tukey",
  Letters = letters
)

print(letters_fla)

###############################################################
# Repeated-measures ANOVA by strain
###############################################################

anova_results <- clean %>%
  group_by(strain) %>%
  do({
    
    model <- aov(
      concentration.ngml ~ treatment * day +
        Error(sample/day),
      data = .
    )
    
    tidy(model)
    
  })

print(anova_results)

significant_results <- anova_results %>%
  filter(p.value < 0.05) %>%
  select(strain, term, p.value)

print(significant_results)

#joing letters onto plotting data
plot_fla <- left_join(
  sum_fla,
  letters_fla,
  by = c("strain","treatment","day")
) 

plot_fla <- plot_fla %>%
  mutate(.group = trimws(.group)) #clean up spaces
plot_fla <- plot_fla %>%
  mutate(
    letters = as.character(trimws(.group))
  ) #plain characters

##compare to abiotic control - dunnett's adjustment
emm_control <- emmeans(
  fla_anova,
  ~ strain | treatment * day
)

abiotic_tests <- contrast(
  emm_control,
  method = "trt.vs.ctrl",
  ref = "abiotic",
  adjust = "dunnett"
)

summary(abiotic_tests)

dunnett_plot <- summary(abiotic_tests) %>% ##add abiotic comparison to plot in form of asterisks
  as.data.frame() %>%
  mutate(
    strain = sub(" - abiotic", "", contrast),
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  ) %>%
  select(strain, treatment, day, stars)

plot_fla <- plot_fla %>%
  left_join(
    dunnett_plot,
    by = c("strain", "treatment", "day")
  )
plot_fla$strain <- plot_fla$strain %>% factor(levels = c("abiotic", 
                                                         "p.putida", 
                                                         "a.venet", 
                                                         "n.aroma", 
                                                         "n.penta")) #this needs to go last before the plot or it won't work

##PLOT
bac_label = c('a.venet'="A. venetianus",
              'h.taenio'="H. taeniospiralis",
              'm.fred'="M. fredericksbergense",
              'n.aroma'="N. aromaticivorans", 
              'n.penta' = 'N. pentaromativorans', 
              'p.putida' = 'P. putida', 
              'abiotic' = "Abiotic")
pd <- position_dodge(width = .4)
ggplot(plot_fla,
       aes(day,
           fluoranthene_mean,
           colour=strain,
           group=treatment)) +
  
  geom_line(aes(linetype=treatment),
            linewidth=0.8, 
            position = pd)+
  
  geom_point(position = pd, size=2)+
  
  geom_errorbar(
    aes(
      ymin=fluoranthene_mean-fluoranthene_se,
      ymax=fluoranthene_mean+fluoranthene_se
    ),
    position = pd,
    width=.1
  )+
  
  geom_text( #tukey letters
    aes(
      y = fluoranthene_mean + fluoranthene_se + 15,
      label = letters,
      group = treatment
    ),
    position = pd,
    size = 3.5
  )+
  geom_text( #abiotic stars
    aes(
      y = fluoranthene_mean + fluoranthene_se + 28,
      label = stars
    ),
    size = 4,
    position = pd, 
    fontface = "bold"
  )+
  
  facet_wrap(~strain,
             labeller=as_labeller(bac_label),
             scales="free_x")+
  theme_pubr() +
  theme(
    strip.text = element_text(
      face = "italic",
      size = 12
    )
  ) +
  scale_colour_manual(
    values = treatment_colors, 
    #limits = c("Free", "Extracapsular"),
    labels = c(
      
      f = "Free",
      
      c = "Extracapsular"
      
    ),
    
    name = "Sample"
  ) +
  scale_linetype_manual(
    values = treatment_linetypes
  ) +
  labs(
    x = "Day",
    y = ylab,
    colour = NULL,
    linetype = NULL
  )+
  guides(colour = "none")+ ylab("Fluoranthene (ng/mL)") + xlab("Day")



###############################################################
# NAPHTHALENE
###############################################################

## Naphthalene data

#load data. 
nap <- read_excel(here::here("data", "Exp 2.1 Naphthalene and code copy.xlsx"))
glimpse(nap)

#clean and remove h.taenio (contaminated with A. venet)
clean_nap <- nap %>% dplyr::filter(sample.no != "6") %>% filter(strain != "h.taenio") %>% filter(strain != "m.fred")
clean_nap$strain <- clean_nap$strain %>% factor(levels = c("abiotic", 
                                                           "p.putida", 
                                                           "a.venet", 
                                                           "n.aroma", 
                                                           "n.penta", 
                                                           "m.fred"))
#remove NAs
clean_nap <- clean_nap %>% filter(concentration.ngml != "N/A")
clean_nap$concentration.ngml <- as.double(clean_nap$concentration.ngml)
clean_nap <- clean_nap %>%
  mutate(
    day = factor(day,
                 levels = c(0, 21, 42))
  )
glimpse(clean_nap)

sum_nap<- clean_nap %>% group_by(strain, day, treatment) %>% summarise(nap_mean = mean(concentration.ngml), 
                                                                       nap_se = sd(concentration.ngml) / sqrt(n()), 
                                                                       
)


nap_anova <- aov(
  concentration.ngml ~ strain * treatment * day,
  data = clean_nap
)

summary(nap_anova)

###############################################################
# Estimated marginal means
###############################################################

emm_nap <- emmeans(
  nap_anova,
  ~ treatment * day | strain
)

###############################################################
# Tukey-adjusted comparisons
###############################################################

tuk_nap <- pairs(
  emm_nap,
  adjust = "tukey"
)

print(tuk_nap)

###############################################################
# Compact letter display
###############################################################

letters_nap <- cld(
  emm_nap,
  adjust = "tukey",
  Letters = letters
)

print(letters_nap)

###############################################################
# Repeated-measures ANOVA
###############################################################

anova_results_nap <- clean_nap %>%
  group_by(strain) %>%
  do({
    
    model <- aov(
      concentration.ngml ~ treatment * day +
        Error(sample/day),
      data = .
    )
    
    tidy(model)
    
  })

print(anova_results_nap)

significant_results_nap <- anova_results_nap %>%
  filter(p.value < 0.05) %>%
  select(strain, term, p.value)

print(significant_results_nap)

###############################################################
# PHENANTHRENE
###############################################################
#PHENANTHRENE DATA

#load data. 
phe <- read_excel(here::here("data", "Exp 2.1 Phenanthrene and code copy.xlsx"))
glimpse(phe)

#clean and remove h.taenio (contaminated with A. venet)
clean_phe <- phe %>% dplyr::filter(sample.no != "6") %>% filter(strain != "h.taenio") %>% filter(strain != "m.fred")
clean_phe$strain <- clean_phe$strain %>% factor(levels = c("abiotic", 
                                                           "p.putida", 
                                                           "a.venet", 
                                                           "n.aroma", 
                                                           "n.penta", 
                                                           "m.fred"))

#remove NAs
clean_phe <- clean_phe %>% filter(concentration.ngml != "N/A")
clean_phe$concentration.ngml <- as.double(clean_phe$concentration.ngml)
clean_phe <- clean_phe %>%
  mutate(day = factor(day, levels = c(0, 21, 42)))
glimpse(clean_phe)

sum_phe<- clean_phe %>% group_by(strain, day, treatment) %>% summarise(phe_mean = mean(concentration.ngml), 
                                                                       phe_se = sd(concentration.ngml) / sqrt(n()), 
                                                                       
)

phe_anova <- aov(
  concentration.ngml ~ strain * treatment * day,
  data = clean_phe
)

summary(phe_anova)

###############################################################
# Estimated marginal means
###############################################################

emm_phe <- emmeans(
  phe_anova,
  ~ treatment * day | strain
)

###############################################################
# Tukey-adjusted comparisons
###############################################################

tuk_phe <- pairs(
  emm_phe,
  adjust = "tukey"
)

print(tuk_phe)

###############################################################
# Compact letter display
###############################################################

letters_phe <- cld(
  emm_phe,
  adjust = "tukey",
  Letters = letters
)

print(letters_phe)

###############################################################
# Repeated-measures ANOVA
###############################################################

anova_results_phe <- clean_phe %>%
  group_by(strain) %>%
  do({
    
    model <- aov(
      concentration.ngml ~ treatment * day +
        Error(sample/day),
      data = .
    )
    
    tidy(model)
    
  })

print(anova_results_phe)

significant_results_phe <- anova_results_phe %>%
  filter(p.value < 0.05) %>%
  select(strain, term, p.value)

print(significant_results_phe)


##PLOTTING FUNCTIONS

make_plot_data <- function(sum_data, letters_data, anova_model) {
    
    sum_data <- sum_data %>%
      mutate(
        strain = as.character(strain),
        treatment = as.character(treatment),
        day = as.character(day)
      )
    
    letters_data <- letters_data %>%
      mutate(
        strain = as.character(strain),
        treatment = as.character(treatment),
        day = as.character(day)
      )
    
    plot_data <- left_join(
      sum_data,
      letters_data,
      by = c("strain", "treatment", "day")
    ) %>%
    mutate(
      .group = trimws(.group),
      letters = trimws(.group)
    )
  
  ## Dunnett comparisons vs abiotic
  emm_control <- emmeans(
    anova_model,
    ~ strain | treatment * day
  )
  
  abiotic_tests <- contrast(
    emm_control,
    method = "trt.vs.ctrl",
    ref = "abiotic",
    adjust = "dunnett"
  )
  
  dunnett_plot <- summary(abiotic_tests) %>%
    as.data.frame() %>%
    mutate(
      strain = sub(" - abiotic", "", contrast),
      stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE ~ ""
      )
    ) %>%
    select(strain, treatment, day, stars)
  
  plot_data <- left_join(
    plot_data,
    dunnett_plot,
    by = c("strain", "treatment", "day")
  )
  
  plot_data$strain <- factor(
    plot_data$strain,
    levels = c(
      "abiotic",
      "p.putida",
      "a.venet",
      "n.aroma",
      "n.penta"
    )
  )
  
  plot_data
}
plot_fla <- make_plot_data(sum_fla, letters_fla, fla_anova)

plot_nap <- make_plot_data(sum_nap, letters_nap, nap_anova)

plot_phe <- make_plot_data(sum_phe, letters_phe, phe_anova)

#plot functions
plot_pah <- function(plot_data,
                     mean_col,
                     se_col,
                     ylab,
                     letter_frac = 0.06,
                     star_frac = 0.09) {
  
  pd <- position_dodge(width = 0.4)
  
  ## Automatically scale annotation spacing
  y_max <- max(
    plot_data[[mean_col]] +
      plot_data[[se_col]],
    na.rm = TRUE
  )
  
  letter_offset <- letter_frac * y_max
  star_offset   <- star_frac * y_max
  
  ggplot(
    plot_data,
    aes(
      x = day,
      y = .data[[mean_col]],
      colour = treatment,
      group = treatment
    )
  ) +
    
    geom_line(
      aes(linetype = treatment),
      linewidth = 0.8,
      position = pd
    ) +
    
    geom_point(
      position = pd,
      size = 2
    ) +
    
    geom_errorbar(
      aes(
        ymin = .data[[mean_col]] - .data[[se_col]],
        ymax = .data[[mean_col]] + .data[[se_col]]
      ),
      width = 0.1,
      position = pd
    ) +
    
    ## Tukey letters
    # geom_text(
    #   aes(
    #     y = .data[[mean_col]] +
    #       .data[[se_col]] +
    #       letter_offset,
    #     label = letters,
    #     group = treatment
    #   ),
    #   position = pd,
    #   size = 3.5,
    #   show.legend = FALSE
    # ) +
    
    ## Dunnett stars
    geom_text(
      aes(
        y = .data[[mean_col]] +
          .data[[se_col]] +
          star_offset,
        label = ifelse(day == "0", "", stars),
        group = treatment
      ),
      position = pd,
      size = 4,
      fontface = "bold",
      show.legend = FALSE
    ) +
    
    facet_wrap(
      ~strain,
      labeller = as_labeller(bac_label),
      scales = "free_x"
    ) +
    
    theme_pubr() +
    theme(
      strip.text = element_text(
        face = "italic",
        size = 12
      )
    ) +
    scale_colour_manual(
      name = "Sample",
      values = treatment_colors,
      limits = c("f", "c"),
      labels = c(
        c = "Extracapsular",
        f = "Free"
      ), 
    #  limits = c("Free", "Extracapsular")
    ) +
    
    scale_linetype_manual(
      name = "Sample",
      values = treatment_linetypes,
      #limits = c("Free", "Extracapsular"),
      labels = c(
        c = "Extracapsular",
        f = "Free"
      )
    ) +
    
    guides(
      colour = guide_legend(
        override.aes = list(
          linewidth = 1,
          linetype = c("solid", "dotted"),
          shape = 16
        )
      ),
      linetype = "none"
    )+
    theme(
      legend.position = "top",
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.spacing.x = unit(0.4, "cm")
    )+
    
    labs(
      
      x = "Day",
      
      y = ylab
      
    )
}
#fluoranthene
plot_pah(
  plot_fla,
  mean_col = "fluoranthene_mean",
  se_col = "fluoranthene_se",
  ylab = "Fluoranthene (ng/mL)"
)
#napthalene
plot_pah(
  plot_nap,
  mean_col = "nap_mean",
  se_col = "nap_se",
  ylab = "Naphthalene (ng/mL)"
)
#phenanthrene
plot_pah(
  plot_phe,
  mean_col = "phe_mean",
  se_col = "phe_se",
  ylab = "Phenanthrene (ng/mL)"
)
