###############################################################
# AIM 2.2
# PAH CONCENTRATION ANALYSIS
###############################################################



library(tidyverse)
library(readxl)
library(here)

library(emmeans)
library(multcomp)
library(multcompView)
library(ggpubr)
library(ggprism)
library(ggtext)

treatment_colors <- c(
  # Free / planktonic
  "free"        = "#D85A44FF",
  "planktonic"  = "#D85A44FF",
  "f"  = "#D85A44FF",
  
  # Aqueous / supernatant
  "aqueous"     = "#2E92A2FF",
  "super"       = "#2E92A2FF",
  "supernatant" = "#2E92A2FF",
  "c" = "#2E92A2FF",
  
  # Capsule
  "capsule"     = "#98A54FFF",
  "cap"         = "#98A54FFF",
  "cc"         = "#98A54FFF",
  
  # Encapsulated combined
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
  "c" = "dashed",
  
  "capsule"     = "dotted",
  "cap"         = "dotted",
  "cc"         = "dotted",
  
  "cap+aq"      = "dotdash",
  "encapsulated"= "dotdash"
)

###############################################################
# Import
###############################################################

results <- read_excel(
  here("data",
       "Exp 2.2 PAH Results-updated-20260819.xlsx")
)

results <- results[, -c(6,7)]

sample_key <- read_excel(
  here("data",
       "Exp 2.2 Sample Code updated.xlsx")
)

data <- left_join(
  results,
  sample_key,
  by = "sample.no"
)

###############################################################
# Convert PAHs to numeric
###############################################################

data <- data %>%
  mutate(
    naphthalene = as.numeric(naphthalene),
    phenanthrene = as.numeric(phenanthrene),
    fluoranthene = as.numeric(fluoranthene)
  )

###############################################################
# Correct capsule concentrations
###############################################################

data <- data %>%
  mutate(
    across(
      c(naphthalene,
        phenanthrene,
        fluoranthene),
      ~ifelse(
        treatment == "cc",
        .x * 5,
        .x
      )
    )
  )

###############################################################
# Replace below detection values
###############################################################

data <- data %>%
  mutate(
    
    naphthalene =
      ifelse(
        is.na(naphthalene),
        0.01/2,
        naphthalene
      ),
    
    phenanthrene =
      ifelse(
        is.na(phenanthrene),
        6.29/2,
        phenanthrene
      ),
    
    fluoranthene =
      ifelse(
        is.na(fluoranthene),
        0.33/2,
        fluoranthene
      ),
    
    naphthalene_bdl =
      naphthalene < 0.01,
    
    phenanthrene_bdl =
      phenanthrene < 6.29,
    
    fluoranthene_bdl =
      fluoranthene < 0.33
    
  )

###############################################################
# Long format
###############################################################

long <- data %>%
  pivot_longer(
    cols = c(
      naphthalene,
      phenanthrene,
      fluoranthene
    ),
    names_to = "compound",
    values_to = "value"
  ) %>%
  mutate(
    
    bdl =
      case_when(
        
        compound == "naphthalene" ~
          naphthalene_bdl,
        
        compound == "phenanthrene" ~
          phenanthrene_bdl,
        
        compound == "fluoranthene" ~
          fluoranthene_bdl
        
      ),
    
    ###########################################################
    # IMPORTANT
    ###########################################################
    
    consortia =
      factor(
        consortia,
        levels = c(
          "a",
          "k",
          "m",
          "r"
        )
      ),
    
    treatment =
      factor(
        treatment,
        levels = c(
          "f",
          "c",
          "cc"
        )
      ),
    
    day =
      factor(
        day,
        levels = c(
          0,
          7,
          14,
          21,
          42
        )
      )
    
  )
summary_data <- long %>%
  group_by(
    compound,
    consortia,
    treatment,
    day
  ) %>%
  summarise(
    
    mean =
      mean(value),
    
    se =
      sd(value) /
      sqrt(n()),
    
    .groups = "drop"
    
  )
#stats function
###############################################################
# Statistical analysis
###############################################################

analyze_pah <- function(compound_name){
  
  dat <-
    
    long %>%
    
    filter(
      compound == compound_name
    )
  
  ###########################################################
  # Three-way ANOVA
  ###########################################################
  
  fit <-
    
    aov(
      
      value ~
        
        consortia *
        treatment *
        day,
      
      data = dat
      
    )
  
  print(summary(fit))
  
  ###########################################################
  # Tukey letters
  ###########################################################
  
  emm_tukey <-
    
    emmeans(
      
      fit,
      
      ~ treatment * day | 
        consortia
      
    )
  
  library(multcomp)
  
  letters <-
    
    cld(
      
      emm_tukey,
      
      adjust = "tukey",
      
      Letters = letters
      
    )
  
  ###########################################################
  # Abiotic comparisons
  ###########################################################
  
  emm_control <-
    
    emmeans(
      
      fit,
      
      ~ consortia |
        treatment * day
      
    )
  
  dunnett <-
    
    contrast(
      
      emm_control,
      
      method = "trt.vs.ctrl",
      
      ref = "a",
      
      adjust = "dunnett"
      
    )
  
  list(
    
    fit = fit,
    
    tukey = letters,
    
    dunnett = summary(dunnett)
    
  )
  
}
#runs stats
stats_fla <-
  
  analyze_pah(
    "fluoranthene"
  )

stats_nap <-
  
  analyze_pah(
    "naphthalene"
  )

stats_phe <-
  
  analyze_pah(
    "phenanthrene"
  )
###############################################################
# Clean Tukey letters
###############################################################

letters_fla <- stats_fla$tukey %>%
  as.data.frame() %>%
  mutate(
    .group = trimws(.group),
    letters = trimws(.group)
  )

letters_nap <- stats_nap$tukey %>%
  as.data.frame() %>%
  mutate(
    .group = trimws(.group),
    letters = trimws(.group)
  )

letters_phe <- stats_phe$tukey %>%
  as.data.frame() %>%
  mutate(
    .group = trimws(.group),
    letters = trimws(.group)
  )
###############################################################
# Build Dunnett stars
###############################################################

make_dunnett <- function(x){
  
  x$dunnett %>%
    
    mutate(
      
      consortia = sub(" - a", "", contrast),
      
      stars = case_when(
        
        p.value < 0.001 ~ "***",
        
        p.value < 0.01 ~ "**",
        
        p.value < 0.05 ~ "*",
        
        TRUE ~ ""
        
      )
      
    ) %>%
    
    select(
      consortia,
      treatment,
      day,
      stars
    )
  
}

dunnett_fla <- make_dunnett(stats_fla)

dunnett_nap <- make_dunnett(stats_nap)

dunnett_phe <- make_dunnett(stats_phe)
##PLOTTING DATA
make_plot_data <- function(summary_data,
                           letters,
                           dunnett,
                           compound_name){
  
  plot_data <-
    
    summary_data %>%
    
    filter(compound == compound_name) %>%
    
    left_join(
      letters,
      by = c(
        "consortia",
        "treatment",
        "day"
      )
    ) %>%
    
    left_join(
      dunnett,
      by = c(
        "consortia",
        "treatment",
        "day"
      )
    ) 
  
  plot_data
  
}
plot_fla <- make_plot_data(
  summary_data,
  letters_fla,
  dunnett_fla,
  "fluoranthene"
)

plot_nap <- make_plot_data(
  summary_data,
  letters_nap,
  dunnett_nap,
  "naphthalene"
)


plot_phe <- make_plot_data(
  summary_data,
  letters_phe,
  dunnett_phe,
  "phenanthrene"
)
##plotting function
###############################################################
# Plot concentration
###############################################################

plot_pah <- function(plot_data,
                     raw_data,
                     ylab){
  # NEW: make day a true continuous time variable
  plot_data <- plot_data %>%
    mutate(day = as.numeric(as.character(day)))
  
  raw_data <- raw_data %>%
    mutate(day = as.numeric(as.character(day)))
  
  pd <- position_dodge(width = 7)
  
  ###########################################################
  # Automatic annotation spacing
  ###########################################################
  
  y_max <-
    
    max(
      plot_data$mean +
        plot_data$se,
      na.rm = TRUE
    )
  
  letter_offset <- 0.04 * y_max
 # star_offset   <- 0.04 * y_max #was .08 when combined with letters
  
  ggplot() +
    
    ###########################################################
  # Raw observations
  ###########################################################
  
  geom_point(
    
    data = raw_data,
    
    aes(
      x = day,
      y = value,
      colour = treatment,
      shape = factor(
        bdl,
        levels = c(FALSE, TRUE)
      )
    ),
    
    position = pd,
    
    alpha = 0.25,
    
    size = 1
    
  ) +
    
    ###########################################################
  # Mean lines
  ###########################################################
  
  geom_line(
    
    data = plot_data,
    
    aes(
      x = day,
      y = mean,
      colour = treatment,
      group = treatment, 
      linetype = treatment
    ),
    
    linewidth = 0.9,
    
    position = pd
    
  ) +
    
    ###########################################################
  # Mean points
  ###########################################################
  
  geom_point(
    
    data = plot_data,
    
    aes(
      x = day,
      y = mean,
      colour = treatment,
      group = treatment
    ),
    
    size = 1.5,
    
    position = pd
    
  ) +
    
    ###########################################################
  # Error bars
  ###########################################################
  
  geom_errorbar(
    
    data = plot_data,
    
    aes(
      
      x = day,
      
      ymin = mean - se,
      
      ymax = mean + se,
      
      colour = treatment,
      
      group = treatment
      
    ),
    
    width = 5,
    
    position = pd
    
  ) +
    
    ###########################################################
  # Tukey letters
  ###########################################################
  
  # geom_text(
  # 
  #   data =
  #     plot_data,
  # 
  #   aes(
  # 
  #     x = day,
  # 
  #     y =
  #       mean +
  #       se +
  #       letter_offset,
  # 
  #     label = letters,
  # 
  #     group = treatment
  # 
  #   ),
  # 
  #   position = pd,
  # 
  #   size = 3.5
  # 
  # ) +
    
    ###########################################################
  # Dunnett stars
  ###########################################################
  
  geom_text(
    data = plot_data,
    aes(
      x = day,
      y = mean + se +
        case_when(
          treatment == "f"  ~ 0.02 * y_max,
          treatment == "c"  ~ 0.03 * y_max,
          treatment == "cc" ~ 0.04 * y_max,
          TRUE ~ 0
        ),
      label = ifelse(day == 0, "", stars),
      group = treatment
    ),
    position = pd,
    size = 3.5,
    fontface = "bold"
  
  ) +
    
    ###########################################################
  # Facets
  ###########################################################
  
  facet_wrap(
    
    ~consortia,
    nrow = 1,
    
    labeller = as_labeller(
      
      c(
        
        a = "Abiotic",
        
        k = "K-Strat",
        
        m = "Mixed",
        
        r = "R-Strat"
        
      )
      
    )
    
  ) +
    
    ###########################################################
  # Theme
  ###########################################################
  
  scale_x_continuous(
    breaks = c(0, 7, 14, 21, 42)
  ) +
  theme_pubr() +
    theme(
      strip.text = element_text(
        face = "italic",
        size = 12
      ), 
      axis.text.x = element_text(size = 9), 
      axis.text.y = element_text(size = 9), 
      panel.spacing.x = unit(0.08, "cm")
    ) +
    scale_colour_manual(
      values = treatment_colors, 
      limits = c("f", "c", "cc"),
      name = NULL, 
      labels = c(
        
        f = "Free",
        
        c = "Extracapsular",
        
        cc = "Capsule")
    ) +
    scale_linetype_manual(
      values = treatment_linetypes, 
      limits = c("f", "c", "cc"),
      name = NULL, 
      labels = c(
        
        f = "Free",
        
        c = "Extracapsular",
        
        cc = "Capsule")
    ) +
    labs(
      x = "Day",
      y = ylab,
      colour = NULL,
      linetype = NULL
    )+
    
    ###########################################################
  # Colors
  ###########################################################
    
    scale_shape_manual(
      
      values = c(16,1),
      
      labels = c(
        "Detected",
        "<LOD"
      )
      
    ) +
    
    labs(
      
      x = "Day",
      
      y = ylab,
      
      colour = "Sample",
      
      shape = NULL
      
    )+
    
    guides(
      colour = guide_legend(
        title = "Sample",
        order = 1,
        keywidth = unit(1.5, "cm"),
        override.aes = list(
          linewidth = 1,
          linetype = c("solid", "dashed", "dotted"),
          shape = 16
        )
      ),
      linetype = "none" ,
      
      shape = guide_legend(
        order = 2,
        keywidth = unit(0.4, "cm")
      )
    ) +
    theme(
      legend.position = "top",
      
      # Put everything on one compact row
      legend.box = "horizontal",
      
      # Much shorter line samples
      #legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(0.35, "cm"),
      
      # Reduce spacing between legend entries
      legend.spacing.x = unit(0.1, "cm"),
      
      # Reduce padding around legend keys
      legend.key.spacing.x = unit(0.1, "cm"),
      
      # Reduce overall legend margins
      legend.margin = margin(0, 0, 2, 0)
    )
  
}
#plot fluoranthene
plot_pah(
  plot_fla,
  filter(long, compound == "fluoranthene"),
  "Fluoranthene (ng/mL)"
)
fla_cons <- last_plot()
# plot nap
plot_pah(
  plot_nap,
  filter(long, compound == "naphthalene"),
  "Naphthalene (ng/mL)"
)
nap_cons <- last_plot()
# plot phe
plot_pah(
  plot_phe,
  filter(long, compound == "phenanthrene"),
  "Phenanthrene (ng/mL)"
)
phe_cons <- last_plot()

##testing out faceting to break down messy figures more
plot_pah_facet <- function(plot_data,
                     raw_data,
                     ylab){
  # NEW: make day a true continuous time variable
  plot_data <- plot_data %>%
    mutate(day = as.numeric(as.character(day)))
  
  raw_data <- raw_data %>%
    mutate(day = as.numeric(as.character(day)))
  
  pd <- position_dodge(width = 7)
  
  ###########################################################
  # Automatic annotation spacing
  ###########################################################
  
  y_max <-
    
    max(
      plot_data$mean +
        plot_data$se,
      na.rm = TRUE
    )
  
  letter_offset <- 0.04 * y_max
  star_offset   <- 0.04 * y_max #was .08 when combined with letters
  
  ggplot() +
    
    ###########################################################
  # Raw observations
  ###########################################################
  
  geom_point(
    
    data = raw_data,
    
    aes(
      x = day,
      y = value,
      colour = treatment,
      shape = factor(
        bdl,
        levels = c(FALSE, TRUE)
      )
    ),
    
    position = pd,
    
    alpha = 0.25,
    
    size = 1
    
  ) +
    
    ###########################################################
  # Mean lines
  ###########################################################
  
  geom_line(
    
    data = plot_data,
    
    aes(
      x = day,
      y = mean,
      colour = treatment,
      group = treatment, 
      linetype = treatment
    ),
    
    linewidth = 0.9,
    
    position = pd
    
  ) +
    
    ###########################################################
  # Mean points
  ###########################################################
  
  geom_point(
    
    data = plot_data,
    
    aes(
      x = day,
      y = mean,
      colour = treatment,
      group = treatment
    ),
    
    size = 1.5,
    
    position = pd
    
  ) +
    
    ###########################################################
  # Error bars
  ###########################################################
  
  geom_errorbar(
    
    data = plot_data,
    
    aes(
      
      x = day,
      
      ymin = mean - se,
      
      ymax = mean + se,
      
      colour = treatment,
      
      group = treatment
      
    ),
    
    width = 5,
    
    position = pd
    
  ) +
    
    ###########################################################
  # Tukey letters
  ###########################################################
  
  # geom_text(
  # 
  #   data =
  #     plot_data,
  # 
  #   aes(
  # 
  #     x = day,
  # 
  #     y =
  #       mean +
  #       se +
  #       letter_offset,
  # 
  #     label = letters,
  # 
  #     group = treatment
  # 
  #   ),
  # 
  #   position = pd,
  # 
  #   size = 3.5
  # 
  # ) +
  
  ###########################################################
  # Dunnett stars
  ###########################################################
  
  geom_text(
    
    data =
      plot_data,
    
    aes(
      
      x = day,
      
      y =
        mean +
        se +
        star_offset,
      
      label =
        ifelse(
          day == "0",
          "",
          stars
        ),
      
      group = treatment
      
    ),
    
    position = pd,
    
    size = 4,
    
    fontface = "bold"
    
  ) +
    
    ###########################################################
  # Facets
  ###########################################################
  
  facet_wrap(
    
    ~treatment + consortia,
    ncol = 4,
    
    labeller = as_labeller(
      
      c(
        
        a = "Abiotic",
        
        k = "K-Strat",
        
        m = "Mixed",
        
        r = "R-Strat"
        
      )
      
    )
    
  ) +
    
    ###########################################################
  # Theme
  ###########################################################
  
  scale_x_continuous(
    breaks = c(0, 7, 14, 21, 42)
  ) +
    theme_pubr() +
    theme(
      strip.text = element_text(
        face = "italic",
        size = 12
      )
    ) +
    scale_colour_manual(
      values = treatment_colors, 
      limits = c("f", "c", "cc"),
      name = "Sample", 
      labels = c(
        
        f = "Free",
        
        c = "Extracapsular",
        
        cc = "Capsule")
    ) +
    scale_linetype_manual(
      values = treatment_linetypes, 
      limits = c("f", "c", "cc"),
      name = "Sample", 
      labels = c(
        
        f = "Free",
        
        c = "Extracapsular",
        
        cc = "Capsule")
    ) +
    labs(
      x = "Day",
      y = ylab,
      colour = NULL,
      linetype = NULL
    )+
    
    ###########################################################
  # Colors
  ###########################################################
  
  scale_shape_manual(
    
    values = c(16,1),
    
    labels = c(
      "Detected",
      "<LOD"
    )
    
  ) +
    
    labs(
      
      x = "Day",
      
      y = ylab,
      
      #colour = "Treatment",
      
      shape = NULL
      
    )+
    
    guides(
      colour = guide_legend(
        order = 1,
        keywidth = unit(1.5, "cm"),
        override.aes = list(
          linewidth = 1,
          linetype = c("solid", "dashed", "dotted"),
          shape = 16
        )
      ),
      linetype = "none" ,
      
      shape = guide_legend(
        order = 2,
        keywidth = unit(0.4, "cm")
      )
    )+
    
    theme(
      legend.position = "top",
      #legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.spacing.x = unit(0.4, "cm")
    )
  
}
#plot fluoranthene
plot_pah_facet(
  plot_fla,
  filter(long, compound == "fluoranthene"),
  "Fluoranthene (ng/mL)"
)

# plot nap
plot_pah_facet(
  plot_nap,
  filter(long, compound == "naphthalene"),
  "Naphthalene (ng/mL)"
)

# plot phe
plot_pah_facet(
  plot_phe,
  filter(long, compound == "phenanthrene"),
  "Phenanthrene (ng/mL)"
)
