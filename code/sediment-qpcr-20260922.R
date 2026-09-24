library(emmeans)

stats_data <- qpcr_avg_lod %>%
  filter(
    sample_type == "sediment slurry",
    !is.na(sediment),
    !is.na(strain),
    !is.na(treatment),
    !is.na(day),
    !is.na(gene_copies_per_mL),
    gene_copies_per_mL > 0
  ) %>%
  mutate(
    day = factor(day),
    treatment = factor(treatment),
    log10_copies = log10(gene_copies_per_mL)
  )

# compare treatments against N within each sediment x strain x day
dunnett_results <- stats_data %>%
  group_by(
    sediment,
    strain,
    day
  ) %>%
  nest() %>%
  mutate(
    model = map(
      data,
      ~ lm(
        log10_copies ~ treatment,
        data = .x
      )
    ),
    
    emm = map(
      model,
      ~ emmeans(
        .x,
        ~ treatment
      )
    ),
    
    contrast = map(
      emm,
      ~ contrast(
        .x,
        method = "trt.vs.ctrl",
        ref = "N",
        adjust = "dunnett"
      )
    ),
    
    stats = map(
      contrast,
      ~ as.data.frame(.x)
    )
  ) %>%
  select(
    sediment,
    strain,
    day,
    stats
  ) %>%
  unnest(stats)
dunnett_results %>%
  arrange(
    sediment,
    strain,
    day
  ) %>%
  print(n = Inf)
dunnett_results <- dunnett_results %>%
  mutate(
    treatment = str_remove(
      contrast,
      " - N$"
    ),
    
    sig = case_when(
      p.value < 0.0001 ~ "****",
      p.value < 0.001  ~ "***",
      p.value < 0.01   ~ "**",
      p.value < 0.05   ~ "*",
      TRUE             ~ "ns"
    )
  )
dunnett_results %>% ##visually inspect results
  select(
    sediment,
    strain,
    day,
    contrast,
    estimate,
    SE,
    p.value,
    sig
  ) %>%
  print(n = Inf)
## calculate where annotations should go
annotation_positions <- stats_data %>%
  group_by(
    sediment,
    strain,
    day
  ) %>%
  summarise(
    ymax = max(
      gene_copies_per_mL,
      na.rm = TRUE
    ),
    .groups = "drop"
  )
sig_annotations <- dunnett_results %>%
  left_join(
    annotation_positions,
    by = c(
      "sediment",
      "strain",
      "day"
    )
  ) %>%
  mutate(
    day_numeric =
      as.numeric(
        as.character(day)
      ),
    
    # Separate treatments slightly horizontally
    x_position = case_when(
      treatment == "F" ~ day_numeric - 0.5,
      treatment == "S" ~ day_numeric + 0.5,
      TRUE ~ day_numeric
    ),
    
    y_position =
      ymax * 1.5
  )
#filter out ns annotations
sig_annotations_plot <- dunnett_results %>%
  filter(
    p.value < 0.05
  ) %>%
  left_join(
    annotation_positions,
    by = c(
      "sediment",
      "strain",
      "day"
    )
  ) %>%
  group_by(
    sediment,
    strain,
    day
  ) %>%
  arrange(
    treatment,
    .by_group = TRUE
  ) %>%
  mutate(
    day_numeric = as.numeric(
      as.character(day)
    ),
    
    # Slight horizontal separation
    x_position = case_when(
      treatment == "E" ~ day_numeric - 0.8,
      treatment == "F" ~ day_numeric,
      treatment == "S" ~ day_numeric + 0.8,
      TRUE ~ day_numeric
    ),
    
    # Stack significance labels vertically.
    # Multiplication is appropriate because y is log10 scaled.
    annotation_level = row_number(),
    
    y_position =
      ymax * 10^(0.20 * annotation_level)
  ) %>%
  ungroup()
## add sig to plot
pd <- position_dodge(width = 6)

plot_data <- qpcr_avg_lod %>%
  filter(
    sample_type == "sediment slurry",
    !is.na(sediment),
    !is.na(treatment),
    !is.na(strain)
  ) %>%
  mutate(
    day_numeric = as.numeric(day),
    
    x_plot =
      day_numeric +
      treatment_offsets[as.character(treatment)]
  )
p <- ggplot(
  qpcr_avg_lod %>%
    filter(
      sample_type == "sediment slurry",
      !is.na(sediment),
      !is.na(treatment)
    ),
  aes(
    x = as.numeric(day),
    y = gene_copies_per_mL,
    color = treatment
  )
) +
  
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = treatment),
    position = pd
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    position = pd,
    size = 3
  ) +
  
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.2,
    position = pd
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
  
  facet_grid(
    sediment ~ strain,
    labeller = labeller(
      strain = as_labeller(
        strain_labels,
        label_parsed
      )
    )
  ) +
  
  scale_color_manual(
    values = c(
      "E" = "#2E92A2FF",
      "F" = "#D85A44FF",
      "N" = "black",
      "S" = "gray"
    )
  ) +
  
  labs(
    title = "Mean Gene Copies per mL",
    y = "Gene Copies per mL (log scale)",
    x = "Time (days)",
    color = "Treatment"
  ) +
  
  theme_pubr() +
  
  theme(
    strip.text = element_text(
      face = "italic",
      size = 12
    )
  )
p +
  geom_text(
    data = sig_annotations_plot,
    aes(
      x = x_position,
      y = y_position,
      label = sig
    ),
    inherit.aes = TRUE,
    size = 3.5
  )
##calculate plotted means

plot_means <- qpcr_avg_lod %>%
  filter(
    sample_type == "sediment slurry",
    !is.na(sediment),
    !is.na(strain),
    !is.na(treatment),
    !is.na(day),
    !is.na(gene_copies_per_mL)
  ) %>%
  group_by(
    sediment,
    strain,
    day,
    treatment
  ) %>%
  summarise(
    mean_copies = mean(
      gene_copies_per_mL,
      na.rm = TRUE
    ),
    
    se_copies = sd(
      gene_copies_per_mL,
      na.rm = TRUE
    ) / sqrt(n()),
    
    .groups = "drop"
  ) %>%
  mutate(
    upper_se = mean_copies + se_copies
  )
treatment_offsets <- c(
  "E" = -1.5,
  "F" = 1.5,
  "N" =  -0.5,
  "S" =  .5
)
plot_data <- qpcr_avg_lod %>%
  filter(
    sample_type == "sediment slurry",
    !is.na(sediment),
    !is.na(treatment),
    !is.na(strain)
  ) %>%
  mutate(
    day_numeric = as.numeric(day),
    
    x_plot =
      day_numeric +
      treatment_offsets[
        as.character(treatment)
      ]
  )
sig_annotations <- sig_annotations %>%
  mutate(
    day_numeric =
      as.numeric(
        as.character(day)
      ),
    
    x_plot =
      day_numeric +
      treatment_offsets[
        as.character(treatment)
      ]
  )
ggplot(
  plot_data,
  aes(
    x = x_plot,
    y = gene_copies_per_mL,
    color = treatment
  )
) +
  
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = treatment)
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    aes(group = treatment),
    size = 3
  ) +
  
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    aes(group = treatment),
    width = 0.3
  ) +
  
  geom_text(
    data = sig_annotations,
    aes(
      x = x_plot,
      y = y_position,
      label = sig,
      color = treatment
    ),
    inherit.aes = FALSE,
    size = 3.5,
    show.legend = FALSE
  ) +
  
  scale_x_continuous(
    breaks = c(0, 14, 42),
    labels = c("0", "14", "42"),
    limits = c(-3, 45),
    expand = expansion(mult = 0)
  ) +
  
  scale_y_log10(
    breaks = trans_breaks(
      "log10",
      function(x) 10^x
    ),
    labels = trans_format(
      "log10",
      math_format(10^.x)
    ),
    expand = expansion(
      mult = c(0.05, 0.15)
    )
  ) +
  
  facet_grid(
    sediment ~ strain,
    labeller = labeller(
      strain = as_labeller(
        strain_labels,
        label_parsed
      )
    )
  ) +
  
  scale_color_manual(
    values = c(
      "E" = "#2E92A2FF",
      "F" = "#D85A44FF",
      "N" = "black",
      "S" = "gray"
    )
  ) +
  
  labs(
    title = "Mean Gene Copies per mL",
    y = "Gene Copies per mL (log scale)",
    x = "Time (days)",
    color = "Treatment"
  ) +
  
  theme_pubr() +
  
  theme(
    strip.text = element_text(
      face = "italic",
      size = 12
    )
  ) ## could adjust horizontal dodge more to accomondate annotations

