###############################################################
# Load packages
###############################################################

library(readxl)
library(tidyverse)
library(emmeans)
library(multcomp)
library(ggpubr)
library(ggtext)
library(scales)

c("#61BEA4FF", "#B6E7E0FF", "#AA3F5DFF", "#DAA5ACFF", "#98A54FFF", "#2E92A2FF", "#FFB651FF", "#D85A44FF")
treatment_colors <- c(
  # Free / planktonic
  "free"        = "#D85A44FF",
  "planktonic"  = "#D85A44FF",
  
  # Aqueous / supernatant
  "aqueous"     = "#98A54FFF",
  "super"       = "#98A54FFF",
  "supernatant" = "#98A54FFF",
  
  # Capsule
  "capsule"     = "#2E92A2FF",
  "cap"         = "#2E92A2FF",
  
  # Encapsulated aqueous
  "cap+aq"      = "#61BEA4FF",
  "encapsulated"= "#61BEA4FF"   # if this level exists anywhere
)

treatment_linetypes <- c(
  "free"        = "solid",
  "planktonic"  = "solid",
  
  "aqueous"     = "dashed",
  "super"       = "dashed",
  "supernatant" = "dashed",
  
  "capsule"     = "dotted",
  "cap"         = "dotted",
  
  "cap+aq"      = "dotdash",
  "encapsulated"= "dotdash"
)

###############################################################
# Import
###############################################################

counts <- read_excel(
  here::here(
    "data",
    "Aim 2.1 Plate Counts copy.xlsx"
  )
)

###############################################################
# Clean
###############################################################

clean <- counts %>%
  
  select(
    exp,
    strain,
    sample,
    rep,
    day,
    rx,
    sample_type,
    avg_cfu
  ) %>%
  
  drop_na() %>%
  
  mutate(
    
    strain = factor(
      strain,
      levels = c(
        "abiotic",
        "p.putida",
        "a.venet",
        "n.aroma",
        "n.penta"
      )
    ),
    
    day = factor(
      day,
      levels = c(0,21,42)
    ),
    
    avg_cfu = ifelse(
      avg_cfu == 0,
      0.1,
      avg_cfu
    )
    
  )
###############################################################
# Total CFU per reactor
###############################################################

wide <- clean %>%
  
  select(
    -sample
  ) %>%
  
  pivot_wider(
    
    names_from = sample_type,
    
    values_from = avg_cfu
    
  )

free <- wide %>%
  
  filter(
    rx == "free"
  ) %>%
  
  mutate(
    
    total_cfu = super * 10
    
  )

capsule <- wide %>%
  
  filter(
    rx == "encapsulated"
  ) %>%
  
  mutate(
    
    total_cfu =
      (super * 10) +
      (cap * 2)
    
  )

total <- bind_rows(
  free,
  capsule
)

summary_total <- total %>%
  
  group_by(
    
    strain,
    
    rx,
    
    day
    
  ) %>%
  
  summarise(
    
    mean = mean(total_cfu),
    
    se = sd(total_cfu) /
      sqrt(n()),
    
    .groups = "drop"
    
  )
###############################################################
# Statistics
###############################################################

analyze_cfu <- function(strain_name){
  
  dat <-
    
    total %>%
    
    filter(
      strain == strain_name
    ) %>%
    
    mutate(
      log_cfu = log10(total_cfu)
    )
  
  ###########################################################
  # Two-way ANOVA on log10(CFU)
  ###########################################################
  
  fit <-
    
    aov(
      
      log_cfu ~
        
        rx *
        day,
      
      data = dat
      
    )
  
  print(summary(fit))
  
  ###########################################################
  # Tukey
  ###########################################################
  
  emm <-
    
    emmeans(
      
      fit,
      
      ~ rx * day
      
    )
  
  letters <-
    
    multcomp::cld(
      
      emm,
      
      adjust = "tukey",
      
      Letters = letters
      
    )
  
  list(
    
    fit = fit,
    
    tukey = letters
    
  )
  
}
stats_putida <- analyze_cfu("p.putida")

stats_avenet <- analyze_cfu("a.venet")

stats_naroma <- analyze_cfu("n.aroma")

stats_npenta <- analyze_cfu("n.penta")

#building plotting tables
letters_putida <- stats_putida$tukey %>%
  
  as.data.frame() %>%
  
  mutate(
    
    .group = trimws(.group),
    
    letters = trimws(.group)
    
  )
letters_avenet <- stats_avenet$tukey %>%
  
  as.data.frame() %>%
  
  mutate(
    
    .group = trimws(.group),
    
    letters = trimws(.group)
    
  )
letters_naroma <- stats_naroma$tukey %>%
  
  as.data.frame() %>%
  
  mutate(
    
    .group = trimws(.group),
    
    letters = trimws(.group)
    
  )
letters_npenta <- stats_npenta$tukey %>%
  
  as.data.frame() %>%
  
  mutate(
    
    .group = trimws(.group),
    
    letters = trimws(.group)
    
  )
make_plot_data_cfu <- function(summary_data,
                               letters,
                               strain_name){
  
  summary_data %>%
    
    filter(
      strain == strain_name
    ) %>%
    
    left_join(
      
      letters,
      
      by = c(
        "rx",
        "day"
      )
      
    ) %>%
    
    mutate(
      
      letters = trimws(.group)
      
    )
  
}


plot_cfu <- function(plot_data,
                     raw_data,
                     ylab){
  
  pd <- position_dodge(.3)
  
  ymax <- max(
    plot_data$mean +
      plot_data$se,
    na.rm = TRUE
  )
  
  ggplot() +
    
    geom_point(
      
      data = raw_data,
      
      aes(
        day,
        total_cfu,
        colour = rx
      ),
      
      alpha = .35,
      
      position = pd
      
    ) +
    
    geom_line(
      
      data = plot_data,
      
      aes(
        day,
        mean,
        colour = rx,
        linetype = rx,
        group = rx
      ),
      
      position = pd
      
    ) +
    
    geom_point(
      
      data = plot_data,
      
      aes(
        day,
        mean,
        colour = rx,
        group = rx
      ),
      
      position = pd,
      
      size = 2
      
    ) +
    
    geom_errorbar(
      
      data = plot_data,
      
      aes(
        
        day,
        
        ymin = mean-se,
        
        ymax = mean+se,
        
        colour = rx,
        
        group = rx
        
      ),
      
      width = .15,
      
      position = pd
      
    ) +
    
    geom_text(
      
      data = plot_data,
      
      aes(
        
        day,
        
        ((mean +
          se)*3) , #plot cfu facet offset
        
        label = letters,
        
        group = rx
        
      ),
      
      position = pd,
      
      size = 3.5
      
    ) +
    
    scale_y_log10(
      
      breaks = scales::trans_breaks(
        "log10",
        function(x) 10^x
      ),
      
      labels = scales::trans_format(
        "log10",
        scales::math_format(10^.x)
      )
      
    ) +
    
    facet_wrap(
      
      ~strain,
      
      labeller = as_labeller(
        bac_label
      )
      
    ) +
    
    theme_pubr() +
    theme(
      strip.text = element_text(
        face = "italic",
        size = 12
      )
    ) +
    scale_colour_manual(
      values = treatment_colors
    ) +
    scale_linetype_manual(
      values = treatment_linetypes
    ) +
    labs(
      x = "Day",
      y = ylab,
      colour = NULL,
      linetype = NULL
    )

}
plot_putida <- make_plot_data_cfu(
  summary_total,
  letters_putida,
  "p.putida"
)

plot_avenet <- make_plot_data_cfu(
  summary_total,
  letters_avenet,
  "a.venet"
)

plot_naroma <- make_plot_data_cfu(
  summary_total,
  letters_naroma,
  "n.aroma"
)

plot_npenta <- make_plot_data_cfu(
  summary_total,
  letters_npenta,
  "n.penta"
)

plot_cfu(
  plot_putida,
  filter(total, strain == "p.putida"),
  "Total CFU/mL"
)

plot_cfu(
  plot_avenet,
  filter(total, strain == "a.venet"),
  "Total CFU/mL"
)

plot_cfu(
  plot_naroma,
  filter(total, strain == "n.aroma"),
  "Total CFU/mL"
)

plot_cfu(
  plot_npenta,
  filter(total, strain == "n.penta"),
  "Total CFU/mL"
)
#save as objects
p_putida <- plot_cfu(
  plot_putida,
  filter(total, strain == "p.putida"),
  "Total CFU/mL"
)

p_avenet <- plot_cfu(
  plot_avenet,
  filter(total, strain == "a.venet"),
  "Total CFU/mL"
)

p_naroma <- plot_cfu(
  plot_naroma,
  filter(total, strain == "n.aroma"),
  "Total CFU/mL"
)

p_npenta <- plot_cfu(
  plot_npenta,
  filter(total, strain == "n.penta"),
  "Total CFU/mL"
)

#patch togehter for multipanel figure
library(patchwork)

(p_putida | p_avenet) /
  (p_naroma | p_npenta) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom"
  )
#combine together for facet wrap
plot_total <- bind_rows(
  plot_putida,
  plot_avenet,
  plot_naroma,
  plot_npenta
)

raw_total <- total %>%
  filter(strain != "abiotic")

plot_cfu <- function(plot_data,       #for facet wrap
                     raw_data,
                     ylab){
  
  pd <- position_dodge(.3)
  
  ymax <- max(
    plot_data$mean +
      plot_data$se,
    na.rm = TRUE
  )
  
  ggplot() +
    
    geom_point(
      
      data = raw_data,
      
      aes(
        day,
        total_cfu,
        colour = rx
      ),
      
      alpha = .35,
      
      position = pd
      
    ) +
    
    geom_line(
      
      data = plot_data,
      
      aes(
        day,
        mean,
        colour = rx,
        linetype = rx,
        group = rx
      ),
      
      position = pd
      
    ) +
    
    geom_point(
      
      data = plot_data,
      
      aes(
        day,
        mean,
        colour = rx,
        group = rx
      ),
      
      position = pd,
      
      size = 2
      
    ) +
    
    geom_errorbar(
      
      data = plot_data,
      
      aes(
        
        day,
        
        ymin = mean-se,
        
        ymax = mean+se,
        
        colour = rx,
        
        group = rx
        
      ),
      
      width = .15,
      
      position = pd
      
    ) +
    
    geom_text(
      
      data = plot_data,
      
      aes(
        
        day,
        
        ((mean +
            se)*1.5) , #plot cfu letter spacing
        
        label = letters,
        
        group = rx
        
      ),
      
      position = pd,
      
      size = 3.5
      
    ) +
    
    scale_y_log10(
      
      breaks = scales::trans_breaks(
        "log10",
        function(x) 10^x
      ),
      
      labels = scales::trans_format(
        "log10",
        scales::math_format(10^.x)
      )
      
    ) +
    
    facet_wrap(
      ~ strain,
      labeller = as_labeller(bac_label),
      scales = "free_y"
    ) +
    
    theme_pubr() +
    theme(
      strip.text = element_text(
        face = "italic",
        size = 12
      )
    ) +
    scale_colour_manual(
      values = treatment_colors
    ) +
    scale_linetype_manual(
      values = treatment_linetypes
    ) +
    labs(
      x = "Day",
      y = ylab,
      colour = NULL,
      linetype = NULL
    )
  
}
plot_cfu(
  plot_total,
  raw_total,
  "Total CFU/mL"
)

##component CFU figures
###############################################################
# Component CFU summary
###############################################################

summary_component <- clean %>%
  
  mutate(
    
    sample = case_when(
      
      rx == "free" ~ "planktonic",
      
      rx == "encapsulated" &
        sample_type == "super" ~ "supernatant",
      
      rx == "encapsulated" &
        sample_type == "cap" ~ "capsule"
      
    )
    
  ) %>%
  
  group_by(
    
    strain,
    
    day,
    
    sample
    
  ) %>%
  
  summarise(
    
    mean = mean(avg_cfu),
    
    se = sd(avg_cfu)/sqrt(n()),
    
    .groups = "drop"
    
  )
###############################################################
# Component statistics
###############################################################

analyze_component <- function(strain_name){
  
  dat <-
    
    clean %>%
    
    mutate(
      
      sample = case_when(
        
        rx == "free" ~ "planktonic",
        
        rx == "encapsulated" &
          sample_type == "super" ~ "supernatant",
        
        rx == "encapsulated" &
          sample_type == "cap" ~ "capsule"
        
      ),
      
      log_cfu = log10(avg_cfu)
      
    ) %>%
    
    filter(
      strain == strain_name
    )
  
  ###########################################################
  # Two-way ANOVA on log10(CFU)
  ###########################################################
  
  fit <-
    
    aov(
      
      log_cfu ~
        
        sample *
        
        day,
      
      data = dat
      
    )
  
  print(summary(fit))
  
  emm <-
    
    emmeans(
      
      fit,
      
      ~ sample * day
      
    )
  
  letters <-
    
    multcomp::cld(
      
      emm,
      
      adjust = "tukey",
      
      Letters = letters
      
    )
  
  list(
    
    fit = fit,
    
    emm = emm,
    
    tukey = letters
    
  )
  
}
stats_comp_putida <- analyze_component("p.putida")

stats_comp_avenet <- analyze_component("a.venet")

stats_comp_naroma <- analyze_component("n.aroma")

stats_comp_npenta <- analyze_component("n.penta")

letters_comp_putida <-
  
  stats_comp_putida$tukey %>%
  
  as.data.frame() %>%
  
  mutate(
    
    .group = trimws(.group),
    
    letters = trimws(.group)
    
  )
letters_comp_avenet <-
  
  stats_comp_avenet$tukey %>%
  
  as.data.frame() %>%
  
  mutate(
    
    .group = trimws(.group),
    
    letters = trimws(.group)
    
  )
letters_comp_naroma <-
  
  stats_comp_naroma$tukey %>%
  
  as.data.frame() %>%
  
  mutate(
    
    .group = trimws(.group),
    
    letters = trimws(.group)
    
  )
letters_comp_npenta <-
  
  stats_comp_npenta$tukey %>%
  
  as.data.frame() %>%
  
  mutate(
    
    .group = trimws(.group),
    
    letters = trimws(.group)
    
  )
make_plot_component <- function(summary_data,
                                letters,
                                strain_name){
  
  summary_data %>%
    
    filter(
      strain == strain_name
    ) %>%
    
    left_join(
      
      letters,
      
      by = c(
        
        "sample",
        
        "day"
        
      )
      
    )
  
}
plot_comp_putida <-
  
  make_plot_component(
    
    summary_component,
    
    letters_comp_putida,
    
    "p.putida"
    
  )
plot_comp_avenet <-
  
  make_plot_component(
    
    summary_component,
    
    letters_comp_avenet,
    
    "a.venet"
    
  )
plot_comp_naroma <-
  
  make_plot_component(
    
    summary_component,
    
    letters_comp_naroma,
    
    "n.aroma"
    
  )
plot_comp_npenta <-
  
  make_plot_component(
    
    summary_component,
    
    letters_comp_npenta,
    
    "n.penta"
    
  )
plot_component <- function(plot_data,
                           raw_data,
                           ylab = "CFU/mL"){
  
  pd <- position_dodge(.45)
  
  ymax <- max(
    plot_data$mean +
      plot_data$se,
    na.rm = TRUE
  )
  
  ggplot() +
    
    geom_point(
      
      data = raw_data,
      
      aes(
        
        day,
        
        avg_cfu,
        
        colour = sample
        
      ),
      
      alpha = .35,
      
      position = pd
      
    ) +
    
    geom_line(
      
      data = plot_data,
      
      aes(
        
        day,
        
        mean,
        
        colour = sample,
        
        linetype = sample,
        
        group = sample
        
      ),
      
      position = pd
      
    ) +
    
    geom_point(
      
      data = plot_data,
      
      aes(
        
        day,
        
        mean,
        
        colour = sample,
        
        group = sample
        
      ),
      
      position = pd,
      
      size = 2
      
    ) +
    
    geom_errorbar(
      
      data = plot_data,
      
      aes(
        
        day,
        
        ymin = mean-se,
        
        ymax = mean+se,
        
        colour = sample,
        
        group = sample
        
      ),
      
      width = .15,
      
      position = pd
      
    ) +
    
    geom_text(
      
      data = plot_data,
      
      aes(
        
        day,
        
        ((mean +
          se)*3) ,  ##comp offset
        
        label = letters,
        
        group = sample
        
      ),
      
      position = pd,
      
      size = 3.5
      
    ) +
    
    scale_y_log10(
      
      breaks = trans_breaks(
        
        "log10",
        
        function(x) 10^x
        
      ),
      
      labels = trans_format(
        
        "log10",
        
        math_format(10^.x)
        
      )
      
    ) +
    
    #scale_colour_manual(
      
      #values = c(
        
      # planktonic = "#619CFF",
        
       # supernatant = "#F8766D",
        
     
    #   capsule = "#00BA38"
        
     # )
      
  #  ) +
    
    #scale_linetype_manual(values = c(planktonic = 1,supernatant = 2,capsule = 3)
      
    #) +
    
    facet_wrap(
      
      ~strain,
      
      labeller = as_labeller(
        bac_label
      )
      
    ) +
    
    theme_pubr() +
    theme(
      strip.text = element_text(
        face = "italic",
        size = 12
      )
    ) +
    scale_colour_manual(
      values = treatment_colors
    ) +
    scale_linetype_manual(
      values = treatment_linetypes
    ) +
    labs(
      x = "Day",
      y = ylab,
      colour = NULL,
      linetype = NULL
    )
  
}
plot_component(
  plot_comp_putida,
  filter(clean, strain == "p.putida") %>%
    mutate(
      sample = case_when(
        rx == "free" ~ "planktonic",
        rx == "encapsulated" & sample_type == "super" ~ "supernatant",
        TRUE ~ "capsule"
      )
    )
)

plot_component(
  plot_comp_avenet,
  filter(clean, strain == "a.venet") %>%
    mutate(
      sample = case_when(
        rx == "free" ~ "planktonic",
        rx == "encapsulated" & sample_type == "super" ~ "supernatant",
        TRUE ~ "capsule"
      )
    )
)
plot_component(
  plot_comp_npenta,
  filter(clean, strain == "n.penta") %>%
    mutate(
      sample = case_when(
        rx == "free" ~ "planktonic",
        rx == "encapsulated" & sample_type == "super" ~ "supernatant",
        TRUE ~ "capsule"
      )
    )
)
plot_component(
  plot_comp_naroma,
  filter(clean, strain == "n.aroma") %>%
    mutate(
      sample = case_when(
        rx == "free" ~ "planktonic",
        rx == "encapsulated" & sample_type == "super" ~ "supernatant",
        TRUE ~ "capsule"
      )
    )
)

##prep for facet wrap
plot_component_all <- bind_rows(
  plot_comp_putida,
  plot_comp_avenet,
  plot_comp_naroma,
  plot_comp_npenta
)
raw_component <- clean %>%
  mutate(
    sample = case_when(
      rx == "free" ~ "planktonic",
      rx == "encapsulated" & sample_type == "super" ~ "supernatant",
      rx == "encapsulated" & sample_type == "cap" ~ "capsule"
    )
  ) %>%
  filter(
    strain %in% c(
      "p.putida",
      "a.venet",
      "n.aroma",
      "n.penta"
    )
  )

plot_component <- function(plot_data,              #new function for facet wrapping
                           raw_data,
                           ylab = "CFU/mL"){
  
  pd <- position_dodge(.45)
  
  ymax <- max(
    plot_data$mean +
      plot_data$se,
    na.rm = TRUE
  )
  
  ggplot() +
    
    geom_point(
      
      data = raw_data,
      
      aes(
        
        day,
        
        avg_cfu,
        
        colour = sample
        
      ),
      
      alpha = .35,
      
      position = pd
      
    ) +
    
    geom_line(
      
      data = plot_data,
      
      aes(
        
        day,
        
        mean,
        
        colour = sample,
        
        linetype = sample,
        
        group = sample
        
      ),
      
      position = pd
      
    ) +
    
    geom_point(
      
      data = plot_data,
      
      aes(
        
        day,
        
        mean,
        
        colour = sample,
        
        group = sample
        
      ),
      
      position = pd,
      
      size = 2
      
    ) +
    
    geom_errorbar(
      
      data = plot_data,
      
      aes(
        
        day,
        
        ymin = mean-se,
        
        ymax = mean+se,
        
        colour = sample,
        
        group = sample
        
      ),
      
      width = .15,
      
      position = pd
      
    ) +
    
    geom_text(
      
      data = plot_data,
      
      aes(
        
        day,
        
        ((mean +
            se)*4.5),  ##comp offset
        
        label = letters,
        
        group = sample
        
      ),
      
      position = pd,
      
      size = 3.5
      
    ) +
    
    scale_y_log10(
      
      breaks = trans_breaks(
        
        "log10",
        
        function(x) 10^x
        
      ),
      
      labels = trans_format(
        
        "log10",
        
        math_format(10^.x)
        
      )
      
    ) +
    
    #scale_colour_manual(
    
    #values = c(
    
    # planktonic = "#619CFF",
    
    # supernatant = "#F8766D",
    
    
    #   capsule = "#00BA38"
    
    # )
    
    #  ) +
    
    #scale_linetype_manual(values = c(planktonic = 1,supernatant = 2,capsule = 3)
    
    #) +
    
    facet_wrap(
      ~ strain,
      labeller = as_labeller(bac_label),
      scales = "fixed"
    )+
    
    theme_pubr() +
    theme(
      strip.text = element_text(
        face = "italic",
        size = 12
      )
    ) +
    scale_colour_manual(
      values = treatment_colors
    ) +
    scale_linetype_manual(
      values = treatment_linetypes
    ) +
    labs(
      x = "Day",
      y = ylab,
      colour = NULL,
      linetype = NULL
    )
  
}
plot_component(
  plot_component_all,
  raw_component,
  "CFU/mL"
)
