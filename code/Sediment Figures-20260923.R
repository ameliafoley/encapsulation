## run sediment PAH prelim analysis first
total_summary <- total_summary %>% filter(!is.na(sediment_location))
# total_summary$treatment <- factor(total_summary$treatment, 
#                                   levels = c("None", "Abiotic Capsule", "Free", "Encapsulated"))
 ggplot(
  total_summary,
  aes(
    x = timepoint,
    y = mean_total_pah,
    color = treatment,
    group = treatment
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = mean_total_pah - se_total_pah,
      ymax = mean_total_pah + se_total_pah
    ),
    width = 1
  ) +
  facet_wrap(
    sample_type ~ sediment_location,
    scales = "free_y", 
    ncol = 4
  ) +
  theme_classic() +
  labs(
    x = "Time (days)",
    y = "Mean Total PAH Concentration ± SE",
    color = "Treatment",
    title = "Total PAH Concentration Through Time"
  )

## make it a bar chart
ggplot(
  total_summary,
  aes(
    x = factor(timepoint),
    y = mean_total_pah,
    fill = treatment
  )
) +
  geom_col(
    position = position_dodge(width = 0.9),
    width = 0.8
  ) +
  geom_errorbar(
    aes(
      ymin = mean_total_pah - se_total_pah,
      ymax = mean_total_pah + se_total_pah
    ),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  facet_wrap(
    sample_type ~ sediment_location,
    scales = "fixed",
    ncol = 4
  ) +
  theme_classic() +
  labs(
    x = "Time (days)",
    y = "Mean Total PAH Concentration ± SE",
    fill = "Treatment",
    title = "Total PAH Concentration Through Time"
  ) + 
  scale_fill_manual(values = c(
    "Encapsulated" = "#2E92A2FF",
    "Free" = "#D85A44FF",
    "None" = "black",
    "Abiotic Capsule" = "gray"
  ))

## stats
library(emmeans)
library(dplyr)

total_pah <- total_pah %>%
  mutate(
    treatment = relevel(factor(treatment), ref = "None"),
    timepoint = factor(timepoint)
  )

total_model <- lm(
  total_pah ~ treatment * timepoint * sediment_location * sample_type,
  data = total_pah
)
emm <- emmeans(
  total_model,
  ~ treatment | timepoint * sediment_location * sample_type
)

treatment_vs_none <- contrast(
  emm,
  method = "trt.vs.ctrl",
  ref = "None",
  adjust = "dunnett"
) %>%
  as.data.frame()

treatment_vs_none
##generate labels
treatment_vs_none <- treatment_vs_none %>%
  mutate(
    treatment = sub(" - None", "", contrast),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  )

stat_labels <- treatment_vs_none %>%
  filter(sig != "ns") %>%
  select(
    sample_type,
    sediment_location,
    timepoint,
    treatment,
    p.value,
    sig
  )
plot_data <- total_summary %>%
  mutate(
    timepoint = factor(timepoint)
  )


stat_labels <- stat_labels %>% mutate(timepoint = factor(timepoint))
stat_labels <- stat_labels %>%
  left_join(
    plot_data %>%
      select(
        sample_type,
        sediment_location,
        timepoint,
        treatment,
        mean_total_pah,
        se_total_pah
      ),
    by = c(
      "sample_type",
      "sediment_location",
      "timepoint",
      "treatment"
    )
  ) %>%
  mutate(
    y_position = mean_total_pah + se_total_pah
  )
## add sig to plot
ggplot(
  plot_data,
  aes(
    x = timepoint,
    y = mean_total_pah,
    fill = treatment
  )
) +
  geom_col(
    position = position_dodge(width = 0.9),
    width = 0.8
  ) +
  geom_errorbar(
    aes(
      ymin = mean_total_pah - se_total_pah,
      ymax = mean_total_pah + se_total_pah
    ),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  
  # Statistical significance vs None
  geom_text(
    data = stat_labels,
    aes(
      x = timepoint,
      y = y_position,
      label = sig,
      group = treatment
    ),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    inherit.aes = TRUE,
    size = 4
  ) +
  
  facet_wrap(
    sample_type ~ sediment_location,
    scales = "fixed",
    ncol = 4
  ) +
  
  theme_classic() +
  
  labs(
    x = "Time (days)",
    y = "Mean Total PAH Concentration ± SE",
    fill = "Treatment",
    title = "Total PAH Concentration Through Time"
  ) +
  
  scale_fill_manual(
    values = c(
      "Encapsulated" = "#2E92A2FF",
      "Free" = "#D85A44FF",
      "None" = "black",
      "Abiotic Capsule" = "gray"
    )
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  )

##total PAH is not very informative for Mains, since it has artificially spiked.
##let's only show total PAHs for pescara and republic. 

pesc.rep<- plot_data %>% filter(sediment_location %in% c("Pescara", "Republic"))


pd <- position_dodge(width = 0.9)
stat_labels_plot <- stat_labels %>%
  filter(
    sediment_location %in% c("Pescara", "Republic"),
    sig != "ns"
  ) %>%
  select(
    sample_type,
    sediment_location,
    timepoint,
    treatment,
    sig
  )

pesc.rep2 <- pesc.rep %>%
  left_join(
    stat_labels_plot,
    by = c(
      "sample_type",
      "sediment_location",
      "timepoint",
      "treatment"
    )
  )

pd <- position_dodge(width = 0.9)
pesc.rep2 <- pesc.rep2 %>%
  mutate(
    treatment = factor(
      treatment,
      levels = c(
        "None",
        "Abiotic Capsule",
        "Free",
        "Encapsulated"
      )
    )
  )
ggplot(
  pesc.rep2,
  aes(
    x = timepoint,
    y = mean_total_pah,
    fill = treatment,
    group = treatment
  )
) +
  geom_col(
    position = pd,
    width = 0.8
  ) +
  geom_errorbar(
    aes(
      ymin = mean_total_pah - se_total_pah,
      ymax = mean_total_pah + se_total_pah
    ),
    position = pd,
    width = 0.2
  ) +
  geom_text(
    aes(
      y = mean_total_pah + se_total_pah,
      label = sig
    ),
    position = pd,
    vjust = -0.7,
    na.rm = TRUE,
    size = 4
  ) +
  facet_wrap(
    sample_type ~ sediment_location,
    scales = "fixed",
    ncol = 2
  ) +
  theme_classic() +
  labs(
    x = "Time (days)",
    y = "Mean Total PAH Concentration ± SE",
    fill = "Treatment",
    title = "Total PAH Concentration Through Time"
  ) +
  scale_fill_manual(
    values = c(
      "Encapsulated" = "#2E92A2FF",
      "Free" = "#D85A44FF",
      "None" = "black",
      "Abiotic Capsule" = "gray"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  )
##separate sediment slurry and dissolved capsule plots
sediment_plot_data <- pesc.rep2 %>%
  filter(sample_type == "Sediment Slurry")

sediment_plot <- ggplot(
  sediment_plot_data,
  aes(
    x = timepoint,
    y = mean_total_pah,
    fill = treatment,
    group = treatment
  )
) +
  geom_col(
    position = pd,
    width = 0.8
  ) +
  geom_errorbar(
    aes(
      ymin = mean_total_pah - se_total_pah,
      ymax = mean_total_pah + se_total_pah
    ),
    position = pd,
    width = 0.2
  ) +
  geom_text(
    aes(
      y = mean_total_pah + se_total_pah,
      label = sig
    ),
    position = pd,
    vjust = -0.7,
    na.rm = TRUE,
    size = 4
  ) +
  facet_wrap(
    ~ sediment_location,
    ncol = 2
  ) +
  theme_classic() +
  labs(
    x = "Time (days)",
    y = "Mean Total PAH Concentration ± SE (ng/g)",
    fill = "Treatment",
    title = "Total PAH Concentration in Sediment Slurry"
  ) +
  scale_fill_manual(
    values = c(
      "None" = "black",
      "Abiotic Capsule" = "gray",
      "Free" = "#D85A44FF",
      "Encapsulated" = "#2E92A2FF"
    ),
    breaks = c(
      "None",
      "Abiotic Capsule",
      "Free",
      "Encapsulated"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  )+
  theme_pubr()+
  scale_y_continuous(labels = label_comma())

sediment_plot
dissolved_plot_data <- pesc.rep2 %>%
  filter(sample_type == "Dissolved Capsule")

pd <- position_dodge(width = 0.9)

dissolved_plot <- ggplot(
  dissolved_plot_data,
  aes(
    x = timepoint,
    y = mean_total_pah,
    fill = treatment,
    group = treatment
  )
) +
  
  # Bars only when n > 1
  geom_col(
    data = dissolved_plot_data %>% filter(n > 1),
    position = pd,
    width = 0.8
  ) +
  
  # Error bars only when n > 1
  geom_errorbar(
    data = dissolved_plot_data %>% filter(n > 1),
    aes(
      ymin = mean_total_pah - se_total_pah,
      ymax = mean_total_pah + se_total_pah
    ),
    position = pd,
    width = 0.2
  ) +
  
  # Show individual observation as a point when n = 1
  geom_point(
    data = dissolved_plot_data %>% filter(n == 1),
    #data = dissolved_plot_data,
    aes(
      x = timepoint,
      y = mean_total_pah,
      group = treatment,
      color = treatment
    ),
    position = pd,
    size = 4,
    inherit.aes = FALSE
  ) +
  
  facet_wrap(
    ~ sediment_location,
    ncol = 2
  ) +
  
  theme_classic() +
  
  labs(
    x = "Time (days)",
    y = "Mean Total PAH Concentration ± SE (ng/g)",
    fill = "Treatment",
    #color = NA,
    title = "Total PAH Concentration in Dissolved Capsule Samples"
  ) +
  guides(color = "none")+
  
  scale_fill_manual(
    values = c(
      "Abiotic Capsule" = "gray",
      "Encapsulated" = "#2E92A2FF"
    )
  ) +
  
  scale_color_manual(
    values = c(
      "Abiotic Capsule" = "gray",
      "Encapsulated" = "#2E92A2FF"
    )
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  )+
  theme_pubr()+
  scale_y_continuous(labels = label_comma())

dissolved_plot

##plotting spiked Mains
library(dplyr)
library(ggplot2)
library(emmeans)

# NAP, PHE, and FLA only
spiked_pah <- pah_filtered %>%
  filter(
    sediment_location %in% c("Mains", "Mains Sterile"),
    molecule_name %in% c(
      "Naphthalene",
      "Phenanthrene",
      "Fluoranthene"
    )
  ) %>%
  mutate(
    treatment = factor(
      treatment,
      levels = c(
        "None",
        "Abiotic Capsule",
        "Free",
        "Encapsulated"
      )
    ),
    timepoint = factor(timepoint),
    sediment_location = factor(
      sediment_location,
      levels = c("Mains", "Mains Sterile")
    ),
    PAH = recode(
      molecule_name,
      "Naphthalene" = "NAP",
      "Phenanthrene" = "PHE",
      "Fluoranthene" = "FLA"
    ),
    PAH = factor(
      PAH,
      levels = c("NAP", "PHE", "FLA")
    )
  )

# Mean ± SE for plotting
spiked_summary <- spiked_pah %>%
  group_by(
    sample_type,
    sediment_location,
    PAH,
    treatment,
    timepoint
  ) %>%
  summarise(
    mean_conc = mean(conc_ng_g, na.rm = TRUE),
    sd_conc = sd(conc_ng_g, na.rm = TRUE),
    n = sum(!is.na(conc_ng_g)),
    se_conc = sd_conc / sqrt(n),
    .groups = "drop"
  )
sediment_summary <- spiked_summary %>%
  filter(sample_type == "Sediment Slurry")

sediment_raw <- spiked_pah %>%
  filter(sample_type == "Sediment Slurry")

pd <- position_dodge(width = 0.9)

sediment_spiked_plot <- ggplot(
  sediment_summary,
  aes(
    x = timepoint,
    y = mean_conc,
    fill = treatment,
    group = treatment
  )
) +
  
  # Mean bars only for n > 1
  geom_col(
    data = sediment_summary %>% filter(n > 1),
    position = pd,
    width = 0.8
  ) +
  
  # SE only for n > 1
  geom_errorbar(
    data = sediment_summary %>% filter(n > 1),
    aes(
      ymin = mean_conc - se_conc,
      ymax = mean_conc + se_conc
    ),
    position = pd,
    width = 0.2
  ) +
  
  # Individual reactor observations
  # geom_point(
  #   data = sediment_raw,
  #   aes(
  #     x = timepoint,
  #     y = conc_ng_g,
  #     color = treatment,
  #     group = treatment
  #   ),
  #   position = position_jitterdodge(
  #     jitter.width = 0.08,
  #     dodge.width = 0.9
  #   ),
  #   size = 2,
  #   alpha = 0.8,
  #   inherit.aes = FALSE
  # ) +
  
  facet_grid(
    PAH ~ sediment_location,
    scales = "free_y"
  ) +
  
  theme_classic() +
  
  labs(
    x = "Time (days)",
    y = "PAH Concentration (ng/g)",
    fill = "Treatment",
    color = "Treatment",
    title = "Spiked PAHs in Sediment Slurry",
    caption = "Bars show mean ± SE; points show individual reactor observations."
  ) +
  
  scale_fill_manual(
    values = c(
      "None" = "black",
      "Abiotic Capsule" = "gray",
      "Free" = "#D85A44FF",
      "Encapsulated" = "#2E92A2FF"
    )
  ) +
  
  scale_color_manual(
    values = c(
      "None" = "black",
      "Abiotic Capsule" = "gray",
      "Free" = "#D85A44FF",
      "Encapsulated" = "#2E92A2FF"
    )
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  
  theme(
    legend.position = "right",
    strip.text = element_text(size = 10)
  )

sediment_spiked_plot
dissolved_summary <- spiked_summary %>%
  filter(sample_type == "Dissolved Capsule")

dissolved_raw <- spiked_pah %>%
  filter(sample_type == "Dissolved Capsule")

dissolved_spiked_plot <- ggplot(
  dissolved_summary,
  aes(
    x = timepoint,
    y = mean_conc,
    fill = treatment,
    group = treatment
  )
) +
  
  geom_col(
    data = dissolved_summary %>% filter(n > 1),
    position = pd,
    width = 0.8
  ) +
  
  geom_errorbar(
    data = dissolved_summary %>% filter(n > 1),
    aes(
      ymin = mean_conc - se_conc,
      ymax = mean_conc + se_conc
    ),
    position = pd,
    width = 0.2
  ) +
  
  # geom_point(
  #   data = dissolved_raw,
  #   aes(
  #     x = timepoint,
  #     y = conc_ng_g,
  #     color = treatment,
  #     group = treatment
  #   ),
  #   position = position_jitterdodge(
  #     jitter.width = 0.08,
  #     dodge.width = 0.9
  #   ),
  #   size = 2,
  #   alpha = 0.8,
  #   inherit.aes = FALSE
  # ) +
  
  facet_grid(
    PAH ~ sediment_location,
    scales = "free_y"
  ) +
  
  theme_classic() +
  
  labs(
    x = "Time (days)",
    y = "PAH Concentration (ng/g)",
    fill = "Treatment",
    color = "Treatment",
    title = "Spiked PAHs in Dissolved Capsule Samples",
    caption = "Bars show mean ± SE; points show individual reactor observations."
  ) +
  
  scale_fill_manual(
    values = c(
      "None" = "black",
      "Abiotic Capsule" = "gray",
      "Free" = "#D85A44FF",
      "Encapsulated" = "#2E92A2FF"
    )
  ) +
  
  scale_color_manual(
    values = c(
      "None" = "black",
      "Abiotic Capsule" = "gray",
      "Free" = "#D85A44FF",
      "Encapsulated" = "#2E92A2FF"
    )
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  
  theme(
    legend.position = "right",
    strip.text = element_text(size = 10)
  )

dissolved_spiked_plot
##statistical analysis
sediment_model <- lm(
  conc_ng_g ~ treatment * timepoint * sediment_location * PAH,
  data = sediment_raw
)
sediment_emm <- emmeans(
  sediment_model,
  ~ treatment | PAH * timepoint * sediment_location
)

sediment_stats <- contrast(
  sediment_emm,
  method = "trt.vs.ctrl",
  ref = "None",
  adjust = "dunnett"
) %>%
  as.data.frame()

sediment_stats
dissolved_model <- lm(
  conc_ng_g ~ treatment * timepoint * sediment_location * PAH,
  data = dissolved_raw
)

dissolved_emm <- emmeans(
  dissolved_model,
  ~ treatment | PAH * timepoint * sediment_location
)

dissolved_stats <- contrast(
  dissolved_emm,
  method = "trt.vs.ctrl",
  ref = "Abiotic Capsule",
  adjust = "dunnett"
) %>%
  as.data.frame()

dissolved_stats
##add sig labels
sediment_stats <- sediment_stats %>%
  mutate(
    treatment = sub(" - None", "", contrast),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  )

dissolved_stats <- dissolved_stats %>%
  mutate(
    treatment = sub(" - Abiotic Capsule", "", contrast),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ "ns"
    )
  )
##prepare annotations
sediment_stats <- sediment_stats %>%
  mutate(
    treatment = sub(" - None", "", contrast),
    treatment = factor(
      treatment,
      levels = c(
        "None",
        "Abiotic Capsule",
        "Free",
        "Encapsulated"
      )
    ),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ NA_character_
    )
  )

dissolved_stats <- dissolved_stats %>%
  mutate(
    treatment = sub(" - Abiotic Capsule", "", contrast),
    treatment = factor(
      treatment,
      levels = c(
        "None",
        "Abiotic Capsule",
        "Free",
        "Encapsulated"
      )
    ),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ NA_character_
    )
  )
##join to summary data
sediment_plot_data <- sediment_summary %>%
  left_join(
    sediment_stats %>%
      select(
        PAH,
        timepoint,
        sediment_location,
        treatment,
        p.value,
        sig
      ),
    by = c(
      "PAH",
      "timepoint",
      "sediment_location",
      "treatment"
    )
  )
sediment_plot_data <- sediment_plot_data %>% 
  mutate(
    bar_height = ifelse(n > 1, mean_conc, NA_real_),
    error_low  = ifelse(n > 1, mean_conc - se_conc, NA_real_),
    error_high = ifelse(n > 1, mean_conc + se_conc, NA_real_)
  )
##calculate asterisks heights
sediment_plot_data <- sediment_plot_data %>%
  group_by(PAH, sediment_location) %>%
  mutate(
    upper = mean_conc + ifelse(is.na(se_conc), 0, se_conc),
    
    y_sig = upper +
      0.04 * max(upper, na.rm = TRUE)
  ) %>%
  ungroup()
sediment_plot_data <- sediment_plot_data %>% ##keep annotation layer for every treatment
  mutate(
    sig_plot = ifelse(is.na(sig), "", sig)
  )
sediment_plot_data <- sediment_plot_data %>%
  mutate(
    time_num = as.numeric(timepoint),
    treatment_offset = case_when(
      treatment == "None" ~ -0.30,
      treatment == "Abiotic Capsule" ~ -0.10,
      treatment == "Free" ~ 0.10,
      treatment == "Encapsulated" ~ 0.30
    ),
    x_plot = time_num + treatment_offset
  )

sediment_raw <- sediment_raw %>%
  mutate(
    time_num = as.numeric(timepoint),
    treatment_offset = case_when(
      treatment == "None" ~ -0.30,
      treatment == "Abiotic Capsule" ~ -0.10,
      treatment == "Free" ~ 0.10,
      treatment == "Encapsulated" ~ 0.30
    ),
    x_plot = time_num + treatment_offset
  )
sediment_raw_plot <- sediment_raw %>%
  left_join(
    sediment_plot_data %>%
      select(
        PAH,
        sediment_location,
        timepoint,
        treatment,
        n
      ),
    by = c(
      "PAH",
      "sediment_location",
      "timepoint",
      "treatment"
    )
  ) %>%
  filter(n < 3)
sediment_spiked_plot <- ggplot() +
  
  # Mean bars for n > 1
  geom_col(
    data = sediment_plot_data %>% filter(n > 1),
    aes(
      x = x_plot,
      y = mean_conc,
      fill = treatment
    ),
    width = 0.18
  ) +
  
  # SE
  geom_errorbar(
    data = sediment_plot_data %>% filter(n > 1),
    aes(
      x = x_plot,
      ymin = mean_conc - se_conc,
      ymax = mean_conc + se_conc
    ),
    width = 0.05
  ) +
  
  # Individual reactor observations
  geom_point(
    data = sediment_raw_plot,
    aes(
      x = x_plot,
      y = conc_ng_g,
      color = treatment
    ),
    position = position_jitter(
      width = 0.025,
      height = 0
    ),
    size = 2,
    alpha = 0.8
  ) +
  
  # Statistical significance
  geom_text(
    data = sediment_plot_data %>%
      filter(!is.na(sig)),
    aes(
      x = x_plot,
      y = y_sig,
      label = sig
    ),
    size = 4
  ) +
  
  facet_grid(
    PAH ~ sediment_location,
    scales = "free_y"
  ) +
  
  # Put the timepoint labels back at positions 1, 2, 3
  scale_x_continuous(
    breaks = seq_along(levels(sediment_plot_data$timepoint)),
    labels = levels(sediment_plot_data$timepoint)
  ) +
  
  scale_fill_manual(
    values = c(
      "None" = "black",
      "Abiotic Capsule" = "gray",
      "Free" = "#D85A44FF",
      "Encapsulated" = "#2E92A2FF"
    ),
    drop = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      "None" = "black",
      "Abiotic Capsule" = "gray",
      "Free" = "#D85A44FF",
      "Encapsulated" = "#2E92A2FF"
    ),
    drop = FALSE
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  
  theme_classic() +
  
  labs(
    x = "Time (days)",
    y = "PAH Concentration (ng/g)",
    fill = "Treatment",
    color = "Treatment",
    title = "Spiked PAHs in Sediment Slurry",
    caption = paste(
      "Bars show mean ± SE; points show individual reactor observations.",
      "* p < 0.05, ** p < 0.01, *** p < 0.001 vs. None (Dunnett-adjusted)."
    )
  ) +
  
  theme(
    legend.position = "right",
    strip.text = element_text(size = 10)
  )+
  theme_pubr()+
  scale_y_continuous(labels = label_comma())

sediment_spiked_plot

##plot dissolved capsule data
dissolved_plot_data <- dissolved_summary %>%
  left_join(
    dissolved_stats %>%
      select(
        PAH,
        timepoint,
        sediment_location,
        treatment,
        p.value,
        sig
      ),
    by = c(
      "PAH",
      "timepoint",
      "sediment_location",
      "treatment"
    )
  )
#create plotting positions
dissolved_plot_data <- dissolved_plot_data %>%
  mutate(
    time_num = as.numeric(timepoint),
    
    treatment_offset = case_when(
      treatment == "Abiotic Capsule" ~ -0.20,
      treatment == "Encapsulated" ~ 0.20,
      TRUE ~ NA_real_
    ),
    
    x_plot = time_num + treatment_offset,
    
    # Bars/error bars only when n > 1
    bar_height = ifelse(n > 1, mean_conc, NA_real_),
    error_low  = ifelse(n > 1, mean_conc - se_conc, NA_real_),
    error_high = ifelse(n > 1, mean_conc + se_conc, NA_real_)
  ) %>%
  
  group_by(PAH, sediment_location) %>%
  
  mutate(
    upper = mean_conc +
      ifelse(is.na(se_conc), 0, se_conc),
    
    y_sig = upper +
      0.04 * max(upper, na.rm = TRUE)
  ) %>%
  
  ungroup()
##prepare replicate observations
dissolved_raw_plot <- dissolved_raw %>%
  left_join(
    dissolved_plot_data %>%
      select(
        PAH,
        sediment_location,
        timepoint,
        treatment,
        n
      ),
    by = c(
      "PAH",
      "sediment_location",
      "timepoint",
      "treatment"
    )
  ) %>%
  
  filter(n < 2) %>%
  
  mutate(
    time_num = as.numeric(timepoint),
    
    treatment_offset = case_when(
      treatment == "Abiotic Capsule" ~ -0.20,
      treatment == "Encapsulated" ~ 0.20,
      TRUE ~ NA_real_
    ),
    
    x_plot = time_num + treatment_offset
  )
## plot
dissolved_spiked_plot <- ggplot() +
  
  # Mean bars for n > 1
  geom_col(
    data = dissolved_plot_data,
    aes(
      x = x_plot,
      y = bar_height,
      fill = treatment
    ),
    width = 0.36,
    na.rm = TRUE
  ) +
  
  # SE
  geom_errorbar(
    data = dissolved_plot_data,
    aes(
      x = x_plot,
      ymin = error_low,
      ymax = error_high
    ),
    width = 0.08,
    na.rm = TRUE
  ) +
  
  # Individual observations when n < 3
  geom_point(
    data = dissolved_raw_plot,
    aes(
      x = x_plot,
      y = conc_ng_g,
      color = treatment
    ),
    position = position_jitter(
      width = 0.025,
      height = 0
    ),
    size = 2,
    alpha = 0.8
  ) +
  
  # Statistical significance
  geom_text(
    data = dissolved_plot_data %>%
      filter(!is.na(sig)),
    aes(
      x = x_plot,
      y = y_sig,
      label = sig
    ),
    size = 4
  ) +
  
  facet_grid(
    PAH ~ sediment_location,
    scales = "free_y"
  ) +
  
  scale_x_continuous(
    breaks = seq_along(levels(dissolved_plot_data$timepoint)),
    labels = levels(dissolved_plot_data$timepoint)
  ) +
  
  scale_fill_manual(
    values = c(
      "Abiotic Capsule" = "gray",
      "Encapsulated" = "#2E92A2FF"
    ),
    drop = FALSE
  ) +
  
  scale_color_manual(
    values = c(
      "Abiotic Capsule" = "gray",
      "Encapsulated" = "#2E92A2FF"
    ),
    drop = FALSE
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  
  theme_classic() +
  
  labs(
    x = "Time (days)",
    y = "PAH Concentration (ng/g)",
    fill = "Treatment",
    color = "Treatment",
    title = "Spiked PAHs in Dissolved Capsule Samples",
    caption = paste(
      "Bars show mean ± SE; individual observations are shown when n < 3.",
      "* p < 0.05, ** p < 0.01, *** p < 0.001 vs. Abiotic Capsule (Dunnett-adjusted)."
    )
  ) +
  
  theme(
    legend.position = "right",
    strip.text = element_text(size = 10)
  )+
  theme_pubr()+
  scale_y_continuous(labels = label_comma())

dissolved_spiked_plot

