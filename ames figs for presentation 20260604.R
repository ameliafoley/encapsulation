# ============================================
# Presentation-quality Ames dose-response plot
# ============================================

library(dplyr)
library(ggplot2)

# ---- Filter to 72 hr total ----
plot_pres <- plot_dat %>%
  filter(endpoint == "72 hr total")

# ---- Create annotation labels from regression results ----
sig_labels <- potency_by_endpoint %>%
  filter(endpoint == "72 hr total") %>%
  mutate(
    label = case_when(
      positive ~ paste0(
        "Slope = ", round(slope, 2),
        "\np = ", signif(p_value, 2)
      ),
      TRUE ~ paste0(
        "NS\np = ", signif(p_value, 2)
      )
    )
  )

# ---- Determine annotation positions ----
label_pos <- plot_pres %>%
  group_by(samples, mix, day, strain, s9) %>%
  summarise(
    x_pos = max(dose_value, na.rm = TRUE),
    y_pos = max(mean_rev + se_rev, na.rm = TRUE) * 1.08,
    .groups = "drop"
  )

sig_labels <- sig_labels %>%
  left_join(
    label_pos,
    by = c("samples", "mix", "day", "strain", "s9")
  )

# ============================================
# TA100
# ============================================

plot_ta100 <- ggplot(
  filter(plot_pres, strain == "TA100"),
  aes(x = dose_value,
      y = mean_rev,
      color = samples,
      group = samples)
) +
  
  # DMSO reference
  geom_hline(
    aes(yintercept = dmso_mean),
    color = "black",
    linewidth = 0.7
  ) +
  
  # 2× DMSO threshold
  geom_hline(
    aes(yintercept = dmso_2x),
    color = "red",
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  # connect dose means
  geom_line(linewidth = 1.1) +
  
  # mean points
  geom_point(size = 4) +
  
  # SE bars
  geom_errorbar(
    aes(
      ymin = mean_rev - se_rev,
      ymax = mean_rev + se_rev
    ),
    width = 1,
    linewidth = 0.8
  ) +
  
  # regression line
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1.3
  ) +
  
  # slope/p-value labels
  geom_text(
    data = filter(sig_labels, strain == "TA100"),
    aes(
      x = x_pos,
      y = y_pos,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 1,
    size = 4
  ) +
  
  facet_grid(
    s9 ~ samples + day,
    scales = "free_y"
  ) +
  
  scale_color_manual(
    values = c(
      "abiotic" = "#D55E00",
      "biotic" = "#0072B2"
    )
  ) +
  
  labs(
    title = "TA100: 72-Hour Total Revertants",
    subtitle = "Black = DMSO mean, Red dashed = 2× DMSO threshold",
    x = "Dose (µg/plate)",
    y = "Mean revertants ± SE",
    color = NULL
  ) +
  
  theme_classic(base_size = 16) +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

plot_ta100

# ============================================
# TA98
# ============================================

plot_ta98 <- ggplot(
  filter(plot_pres, strain == "TA98"),
  aes(x = dose_value,
      y = mean_rev,
      color = samples,
      group = samples)
) +
  
  geom_hline(
    aes(yintercept = dmso_mean),
    color = "black",
    linewidth = 0.7
  ) +
  
  geom_hline(
    aes(yintercept = dmso_2x),
    color = "red",
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  geom_line(linewidth = 1.1) +
  geom_point(size = 4) +
  
  geom_errorbar(
    aes(
      ymin = mean_rev - se_rev,
      ymax = mean_rev + se_rev
    ),
    width = 1,
    linewidth = 0.8
  ) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1.3
  ) +
  
  geom_text(
    data = filter(sig_labels, strain == "TA98"),
    aes(
      x = x_pos,
      y = y_pos,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 1,
    size = 4
  ) +
  
  facet_grid(
    s9 ~ samples + day,
    scales = "free_y"
  ) +
  
  scale_color_manual(
    values = c(
      "abiotic" = "#D55E00",
      "biotic" = "#0072B2"
    )
  ) +
  
  labs(
    title = "TA98: 72-Hour Total Revertants",
    subtitle = "Black = DMSO mean, Red dashed = 2× DMSO threshold",
    x = "Dose (µg/plate)",
    y = "Mean revertants ± SE",
    color = NULL
  ) +
  
  theme_classic(base_size = 16) +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

plot_ta98

#separate by mix
make_presentation_plot <- function(strain_name, mix_number) {
    
    plot_sub <- plot_pres %>%
      filter(
        strain == strain_name,
        mix == mix_number
      ) %>%
      mutate(
        s9 = factor(
          s9,
          levels = c("no", "yes"),
          labels = c("-S9", "+S9")
        )
      )
    
    label_sub <- sig_labels %>%
      filter(
        strain == strain_name,
        mix == mix_number
      ) %>%
      mutate(
        s9 = factor(
          s9,
          levels = c("no", "yes"),
          labels = c("- S9", "+ S9")
        )
      )
  
  ggplot(
    plot_sub,
    aes(
      x = dose_value,
      y = mean_rev,
      color = samples,
      group = samples
    )
  ) +
    
    # DMSO mean
    geom_hline(
      aes(yintercept = dmso_mean),
      color = "black",
      linewidth = 0.7
    ) +
    
    # 2x DMSO threshold
    geom_hline(
      aes(yintercept = dmso_2x),
      color = "black",
      linewidth = 1,
      linetype = "dashed"
    ) +
    
    #geom_line(linewidth = 1.2) +
    
    geom_point(size = 4) +
    
    geom_errorbar(
      aes(
        ymin = mean_rev - se_rev,
        ymax = mean_rev + se_rev
      ),
      width = 1,
      linewidth = 0.8
    ) +
    
    geom_smooth(
      method = "lm",
      se = FALSE,
      linewidth = 1.3
    ) +
    
    #geom_text(
 #     data = label_sub,
 #     aes(x = x_pos,y = y_pos,label = label), inherit.aes = FALSE, hjust = 1,size = 4) +
     facet_grid( s9 ~ samples + day,scales = "free_y") +
    
    scale_color_manual(
      values = c(
        "abiotic" = "#D55E00",
        "biotic" = "#0072B2"
      )
    ) +
    
    labs(
      title = paste0(
        strain_name,
        " — PAH Mix ",
        mix_number,
        " (72 hr Total)"
      ),
      subtitle =
        "Black = DMSO mean | Black dashed = 2× DMSO threshold",
      x = "Dose (µL/plate)",
      y = "Mean revertants ± SE",
      color = NULL
    ) +
    
    theme_classic(base_size = 16) +
    theme(
      strip.background =
        element_rect(fill = "grey90"),
      strip.text =
        element_text(face = "bold"),
      plot.title =
        element_text(face = "bold"),
      legend.position = "bottom"
    )
}
p_ta100_mix1 <- make_presentation_plot("TA100", 1)
p_ta100_mix2 <- make_presentation_plot("TA100", 2)

p_ta98_mix1 <- make_presentation_plot("TA98", 1)
p_ta98_mix2 <- make_presentation_plot("TA98", 2)

p_ta100_mix1
p_ta100_mix2
p_ta98_mix1
p_ta98_mix2
