library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(ggplot2)
library("writexl", )
library(vtable)
library(gtsummary)
library(gridGraphics)
library(ggsignif)
library(multcompView)
library(rstatix)
library(ggpubr)
library(ggbreak)
library(scales)
library(cowplot)
library(ggprism)
library(gridExtra)
library(agricolae) #for assigning Tukey letters
library(ggsignif)
library(broom)
library(ggtext)
library(emmeans)

mono_fla<- readRDS("sum_fla2.1.rds")
clean_mono<- mono_fla %>% filter(strain != "a.venet") %>% filter(strain != "n.aroma")
#consortia_fla <- readRDS("merged.consortia.adj2.2")
cons_long_fla<- readRDS("2.2_long.rds")
clean_cons<- cons_long_fla %>% filter(compound == "fluoranthene") %>% filter(consortia %in% c("a", "m"))

# Make mono consistent with cons structure
mono_fixed <- clean_mono %>%
  rename(mean = fluoranthene_mean,
         se   = fluoranthene_se) %>%
  mutate(
    compound = "fluoranthene",
    type = "mono"
  )

# Add a type column to cons
cons_fixed <- clean_cons %>%
  mutate(type = "consortia")

# Combine
merged_data <- bind_rows(mono_fixed, cons_fixed)

library(dplyr)

merged_data <- merged_data %>%
  mutate(
    organism = case_when(
      type == "mono" ~ strain,
      type == "consortia" ~ consortia,
      TRUE ~ NA_character_
    )
  )
merged_data <- merged_data %>%
  mutate(
    organism_label = case_when(
      organism == "p.putida" ~ "P. putida",
      organism == "n.penta" ~ "N. pentaromativorans",
      organism == "abiotic" & type == "mono" ~ "Abiotic (Monoculture)",
      organism == "a" ~ "Abiotic (Consortia)",
      organism == "m" ~ "Mixed Consortium",
      TRUE ~ organism
    )
  )
merged_data <- merged_data %>%
  mutate(
    measurement = case_when(
      treatment == "f" ~ "Planktonic",
      treatment == "c" ~ "Encapsulated aqueous",
      treatment == "cc" ~ "Encapsulated capsule-bound",
      TRUE ~ treatment
    )
  )
merged_data <- merged_data %>%
  mutate(
    organism_label = factor(
      organism_label,
      levels = c(
        "Abiotic (Monoculture)",
        "P. putida",
        "N. pentaromativorans",
        "Abiotic (Consortia)",
        "Mixed Consortium"
      )
    )
  )
library(ggplot2)

target_day <- 21

plot_df <- merged_data %>%
  filter(day == target_day) %>%
  mutate(
    organism_label = factor(
      organism_label,
      c(
        "Abiotic (mono)",
        "P. putida",
        "N. pentaromativorans",
        "Abiotic (consortia)",
        "Mixed Consortium"
      )
    ))

ggplot(plot_df,
       aes(x = organism_label,
           y = mean,
           fill = measurement)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.65,
           color = "black") +
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se),
                position = position_dodge(width = 0.75),
                width = 0.2) +
  labs(x = NULL,
       y = "Fluoranthene concentration",
       fill = "Measurement type",
       title = paste("Measured PAH concentration — Day", target_day),
       caption = "Capsule-bound (cc) measured only in consortia.") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "top")

# prepare plotting dataframe (no day filter!)
plot_df <- merged_data %>%
  # keep only relevant measurement types if needed:
  filter(measurement %in% c("Planktonic", "Encapsulated aqueous", "Encapsulated capsule-bound")) %>%
  group_by(organism_label, day, measurement) %>%
  summarise(
    mean = mean(mean, na.rm = TRUE),
    se   = ifelse(all(is.na(se)), NA_real_, sqrt(sum(se^2, na.rm = TRUE))/n()),
    .groups = "drop"
  )

p_time <- ggplot(plot_df, aes(x = day, y = mean, color = measurement, linetype = measurement)) +
  geom_line(aes(group = measurement), size = 0.9, na.rm = TRUE) +
  geom_point(size = 2.5, na.rm = TRUE) +
  geom_errorbar(aes(ymin = pmax(0, mean - se), ymax = mean + se),
                width = 0.6, size = 0.6, alpha = 0.9, na.rm = TRUE) +
  facet_wrap(~ organism_label, scales = "fixed", axes = "all_x") +    # change to ncol if you want stacked rows
  scale_x_continuous(breaks = sort(unique(plot_df$day))) +
  labs(
    x = "Day",
    y = "Fluoranthene (ng/mL)",
    color = "Measurement",
    linetype = "Measurement",
    #title = "Measured PAH concentration over time",
  ) +
  theme_pubr(base_size = 13) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "italic"),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )+ 
  scale_y_continuous(trans = "log10")
p_time
# compact PNG sized to be comfortably under 1/4 page
p_time_small <- ggplot(plot_df, aes(x = day, y = mean, color = measurement, linetype = measurement)) +
  geom_line(aes(group = measurement), size = 0.6, na.rm = TRUE) +
  geom_point(size = 1, na.rm = TRUE) +
  geom_errorbar(aes(ymin = pmax(0, mean - se), ymax = mean + se),
                width = 0.6, size = 0.4, alpha = 0.9, na.rm = TRUE) +
  facet_wrap(~ organism_label, scales = "fixed", axes = "all_x") +    # change to ncol if you want stacked rows
  scale_x_continuous(breaks = sort(unique(plot_df$day))) +
  labs(
    x = "Day",
    y = "Fluoranthene (ng/mL)",
    color = "Measurement",
    linetype = "Measurement",
    #title = "Measured PAH concentration over time",
  ) +
  theme_pubr(base_size = 6) +
  theme(
    legend.position = "top",
    legend.box.spacing = unit(0.05, "lines"),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_text(angle = 0, vjust = 0.5)
  )+ 
  scale_y_continuous(trans = "log10")
p_time_small 

ggsave("figure_paht_compact.png", plot = p_time_small, width = 4.25, height = 3.5, units = "in", dpi = 300, bg = "white")
#ggsave(
  filename = "figure_paht_compact.png",
  plot = p_time,                # or final_plot / final if using cowplot
  width = 4.25,                 # inches (about half page width)
  height = 3.5,                 # inches (shorter than half page height)
  units = "in",
  dpi = 300,
  bg = "white"
)


