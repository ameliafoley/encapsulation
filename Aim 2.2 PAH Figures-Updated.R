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
library(patchwork)


#load data. 
consortia <- read_excel(here::here("data", "Exp 2.2 PAH Results updated.xlsx"))
consortia <- consortia[ , -c(6,7)]
consortia.code <- read_excel(here::here("data", "Exp 2.2 Sample Code updated.xlsx"))
merged.consortia <- left_join(consortia, consortia.code, by = "sample.no")
glimpse(merged.consortia)
merged.consortia <- merged.consortia %>%
  mutate(
    naphthalene = as.numeric(naphthalene),
    phenanthrene = as.numeric(phenanthrene),
    fluoranthene = as.numeric(fluoranthene), 
    sample.no = as.numeric(sample.no)
  )
#adjust capsule concentrations based on dilution
merged.consortia.adj <- merged.consortia %>%
  mutate(
    across(c(naphthalene, phenanthrene, fluoranthene),
           ~ ifelse(treatment == "cc", .x * 5, .x))
  )
write.csv(merged.consortia.adj, "merged.consortia.adj.csv")
#adjust for values below the MDL
merged.consortia.adj <- merged.consortia.adj %>%
  mutate(
    # Replace NAs with half the MDL
    naphthalene  = ifelse(is.na(naphthalene), 0.01 / 2, naphthalene),
    phenanthrene = ifelse(is.na(phenanthrene), 6.29 / 2, phenanthrene),
    fluoranthene = ifelse(is.na(fluoranthene), 0.33 / 2, fluoranthene),
    
    # Flag if below MDL (either originally missing OR < MDL after replacement)
    naphthalene_bdl  = naphthalene  < 0.01,
    phenanthrene_bdl = phenanthrene < 6.29,
    fluoranthene_bdl = fluoranthene < 0.33
  )

saveRDS(merged.consortia.adj, "merged.consortia.adj2.2")

##full workflow
#keep raw values in long format with flags
long_with_flags <- merged.consortia.adj %>%
  pivot_longer(
    cols = c(naphthalene, phenanthrene, fluoranthene),
    names_to = "compound",
    values_to = "value"
  ) %>%
  mutate(
    bdl_flag = case_when(
      compound == "naphthalene"  ~ naphthalene_bdl,
      compound == "phenanthrene" ~ phenanthrene_bdl,
      compound == "fluoranthene" ~ fluoranthene_bdl
    ),
    type = "raw"   # marker so we know these are raw values
  )
#summarize to means + SE
sum_long <- long_with_flags %>%
  group_by(consortia, day, treatment, compound) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    se   = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
    .groups = "drop"
  ) %>%
  mutate(type = "summary")
#merge raw and summary into one data frame
plot_data <- bind_rows(long_with_flags, sum_long)
#plot raw points and summary lines in one ggplot
ggplot() +
  # raw points with open/closed shapes
  geom_point(
    data = filter(plot_data, type == "raw"),
    aes(x = day, y = value, color = treatment,
        shape = factor(bdl_flag, levels = c(FALSE, TRUE))),
    position = position_dodge(width = 0.5),
    size = 2
  ) +
  scale_shape_manual(
    name = "Detection",
    values = c("FALSE" = 16, "TRUE" = 1),   # filled vs open circles
    labels = c("Detected", "<LOD")
  ) +
  
  # mean lines
  geom_line(
    data = filter(plot_data, type == "summary"),
    aes(x = day, y = mean, color = treatment, group = treatment)
  ) +
  
  # error bars
  geom_errorbar(
    data = filter(plot_data, type == "summary"),
    aes(x = day, ymin = mean - se, ymax = mean + se, color = treatment, group = treatment),
    width = 0.2
  ) +
  
  facet_wrap(~ compound + consortia, ncol = 4, scales = "free_y") +
  theme_classic()
##
saveRDS(sum_long, "2.2_long.rds")
##separate plots
# Make a plotting function for reuse
plot_pah <- function(df, compound_name) {
  ggplot(
    data = filter(df, compound == compound_name),
    aes(
      x = day,
      y = value,
      color = treatment,
      shape = factor(bdl_flag, levels = c(FALSE, TRUE))
    )
  ) +
    geom_point(
      position = position_dodge(width = 0.5),
      size = 2,
      alpha = 0.4  # translucent raw data points
    ) +
    scale_shape_manual(
      name = "Detection",
      values = c("FALSE" = 16, "TRUE" = 1),  # filled = detected, open = <LOD
      labels = c("Detected", "<LOD")
    ) +
    geom_line(
      data = filter(plot_data, type == "summary", compound == compound_name),
      aes(x = day, y = mean, color = treatment, group = treatment)
    ) +
    geom_errorbar(
      data = filter(plot_data, type == "summary", compound == compound_name),
      aes(
        x = day,
        ymin = mean - se,
        ymax = mean + se,
        color = treatment,
        group = treatment
      ),
      width = 0.2
    ) +
    facet_wrap(~consortia, ncol = 4, labeller = as_labeller(c(
      "a" = "Abiotic",
      "k" = "K-strat",
      "m" = "Mixed",
      "r" = "R-strat"
    )) )+
    labs(
      title = str_to_sentence(compound_name),  # capitalized title
      y = "Concentration (ng/mL)",          # y-axis label
      x = "Day"                             # x-axis label
    ) +
    theme_classic()+
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      strip.background = element_rect(fill = "grey90", color = NA)
    )
}

# Generate one plot per compound
plot_naphthalene  <- plot_pah(plot_data, "naphthalene")
plot_phenanthrene <- plot_pah(plot_data, "phenanthrene")
plot_fluoranthene <- plot_pah(plot_data, "fluoranthene")
#plots for patchwork
plot_naphthalene / plot_phenanthrene / plot_fluoranthene
# Combine with a single shared legend
(plot_naphthalene / plot_phenanthrene / plot_fluoranthene) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#duplicate plot without capsule data to emphasize findings
plot_data_nocap <- plot_data %>% filter(treatment != "cc")
#new function
plot_pah <- function(df, compound_name) {
  ggplot(
    data = filter(df, compound == compound_name),
    aes(
      x = day,
      y = value,
      color = treatment,
      shape = factor(bdl_flag, levels = c(FALSE, TRUE))
    )
  ) +
    geom_point(
      position = position_dodge(width = 0.5),
      size = 2,
      alpha = 0.4  # translucent raw data points
    ) +
    scale_shape_manual(
      name = "Detection",
      values = c("FALSE" = 16, "TRUE" = 1),  # filled = detected, open = <LOD
      labels = c("Detected", "<LOD")
    ) +
    geom_line(
      data = filter(plot_data_nocap, type == "summary", compound == compound_name),
      aes(x = day, y = mean, color = treatment, group = treatment)
    ) +
    geom_errorbar(
      data = filter(plot_data_nocap, type == "summary", compound == compound_name),
      aes(
        x = day,
        ymin = mean - se,
        ymax = mean + se,
        color = treatment,
        group = treatment
      ),
      width = 0.2
    ) +
    facet_wrap(~consortia, ncol = 4, labeller = as_labeller(c(
      "a" = "Abiotic",
      "k" = "K-strat",
      "m" = "Mixed",
      "r" = "R-strat"
    )) )+
    labs(
      title = str_to_sentence(compound_name),  # capitalized title
      y = "Concentration (ng/mL)",          # y-axis label
      x = "Day"                             # x-axis label
    ) +
    scale_color_manual(
      name = "treatment",
      values = c(
        "c"  = "#F8766D",
        "cc" = "#00BA38",
        "f"  = "#619CFF"
      ),
      limits = c("c", "cc", "f"),
      drop = FALSE
    )+
    theme_classic()+
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      strip.background = element_rect(fill = "grey90", color = NA)
    )
}
#replot
plot_naphthalene_nocap  <- plot_pah(plot_data_nocap, "naphthalene")
plot_phenanthrene_nocap <- plot_pah(plot_data_nocap, "phenanthrene")
plot_fluoranthene_nocap <- plot_pah(plot_data_nocap, "fluoranthene")
#plots for patchwork
plot_naphthalene_nocap / plot_phenanthrene_nocap / plot_fluoranthene_nocap
# Combine with a single shared legend
(plot_naphthalene_nocap / plot_phenanthrene_nocap / plot_fluoranthene_nocap) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#investigate abiotic
library(ggrepel)

ggplot(
  data = filter(long_with_flags, consortia == "a"),  # adjust condition to match your dataset
  aes(x = day, y = value, color = treatment, 
      shape = factor(bdl_flag, levels = c(FALSE, TRUE)), 
      label = sample)   # assuming your sample ID column is called "sample"
) +
  geom_point(size = 2) +
  geom_text_repel(size = 3, show.legend = FALSE) +   # add labels for points
  scale_shape_manual(
    name = "Detection",
    values = c("FALSE" = 16, "TRUE" = 1),
    labels = c("Detected", "<LOD")
  ) +
  facet_wrap(~compound, scales = "free_y") +
  theme_classic()

# Filter raw + summary data for abiotic only
abiotic_raw <- long_with_flags %>%
  filter(grepl("a", consortia, ignore.case = TRUE))

abiotic_sum <- sum_long %>%
  filter(grepl("a", consortia, ignore.case = TRUE))

# Plot
ggplot() +
  # raw abiotic points with labels
  geom_point(data = abiotic_raw,
             aes(x = day, y = value, color = treatment,
                 shape = factor(bdl_flag, levels = c(FALSE, TRUE))),
             size = 2) +
  geom_text_repel(data = abiotic_raw,
                  aes(x = day, y = value, label = sample, color = treatment),
                  size = 3, show.legend = FALSE) +
  
  # summary lines
  geom_line(data = abiotic_sum,
            aes(x = day, y = mean, color = treatment, group = treatment),
            linewidth = 1) +
  
  # error bars
  geom_errorbar(data = abiotic_sum,
                aes(x = day, ymin = mean - se, ymax = mean + se,
                    color = consortia, group = treatment),
                width = 0.2) +
  
  # facet by compound (separate PAHs)
  facet_wrap(~compound, scales = "free_y") +
  
  scale_shape_manual(
    name = "Detection",
    values = c("FALSE" = 16, "TRUE" = 1),  # filled vs open
    labels = c("Detected", "<LOD")
  ) +
  theme_classic()

##

##PERCENT REMOVAL
# Choose which compounds to calculate removal for
compounds <- c("naphthalene", "phenanthrene", "fluoranthene")

# Calculate % removal
removal_df <- merged.consortia.adj %>%
  filter(day %in% c(0, 21)) %>%
  group_by(consortia, treatment, day) %>%
  summarise(across(all_of(compounds), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = day, values_from = all_of(compounds), names_prefix = "day") %>%
  mutate(
    perc_removal_naphthalene   = 100 * (1 - (naphthalene_day21   / naphthalene_day0)),
    perc_removal_phenanthrene  = 100 * (1 - (phenanthrene_day21  / phenanthrene_day0)),
    perc_removal_fluoranthene  = 100 * (1 - (fluoranthene_day21  / fluoranthene_day0))
  )

# View results
print(removal_df)

# Convert to long format for ggplot
removal_long <- removal_df %>%
  dplyr::select(consortia, treatment,
                perc_removal_naphthalene,
                perc_removal_phenanthrene,
                perc_removal_fluoranthene) %>%
  pivot_longer(cols = starts_with("perc_removal"),
               names_to = "compound",
               values_to = "perc_removal") %>%
  mutate(compound = str_replace(compound, "perc_removal_", ""))

# Plot 
ggplot(removal_long, aes(x = interaction(consortia, treatment),
                         y = perc_removal,
                         fill = compound)) +
  geom_col(position = position_dodge()) +
  labs(x = "Consortium × Treatment",
       y = "% Removal (Day 0 → 21)",
       fill = "Compound") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#faceted
ggplot(removal_long, aes(x = interaction(consortia, treatment),
                         y = perc_removal,
                         fill = treatment)) +
  geom_col() +
  facet_wrap(~ compound + consortia, scales = "free") +
  labs(x = "Consortium × Treatment",
       y = "% Removal (Day 0 → 21)",
       fill = "Consortium") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##Percent removal with error bars
# Calculate replicate-level % removal
removal_reps <- merged.consortia.adj %>%
  filter(day %in% c(0, 21)) %>%
  dplyr::select(sample, consortia, treatment, day, all_of(compounds)) %>%
  pivot_wider(names_from = day, values_from = all_of(compounds), names_prefix = "day") %>%
  mutate(
    perc_removal_naphthalene   = 100 * (1 - (naphthalene_day21   / naphthalene_day0)),
    perc_removal_phenanthrene  = 100 * (1 - (phenanthrene_day21  / phenanthrene_day0)),
    perc_removal_fluoranthene  = 100 * (1 - (fluoranthene_day21  / fluoranthene_day0))
  )
#summarize
removal_summary <- removal_reps %>%
  pivot_longer(cols = starts_with("perc_removal"),
               names_to = "compound",
               values_to = "perc_removal") %>%
  mutate(compound = str_replace(compound, "perc_removal_", "")) %>%
  group_by(consortia, treatment, compound) %>%
  summarise(
    mean_removal = mean(perc_removal, na.rm = TRUE),
    se_removal   = sd(perc_removal, na.rm = TRUE) / sqrt(sum(!is.na(perc_removal))),
    .groups = "drop"
  )
#plot
ggplot(removal_summary, aes(x = interaction(consortia, treatment),
                            y = mean_removal,
                            fill = compound)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_removal - se_removal,
                    ymax = mean_removal + se_removal),
                position = position_dodge(width = 0.9),
                width = 0.3) +
  labs(x = "Consortium × Treatment",
       y = "% Removal (Day 0 → 21)",
       fill = "Compound") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#faceted version
ggplot(removal_summary, aes(x = treatment,
                            y = mean_removal,
                            fill = treatment)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_removal - se_removal,
                    ymax = mean_removal + se_removal),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  facet_wrap(~ compound + consortia, scales = "free_y") +
  labs(x = "Treatment",
       y = "% Removal (Day 0 → 21)",
       fill = "Treatment") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##split plots to control axes
# Naphthalene plot
plot_naph <- ggplot(filter(removal_summary, compound == "naphthalene"),
                    aes(x = consortia, y = mean_removal, fill = treatment)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_removal - se_removal,
                    ymax = mean_removal + se_removal,
                    group = treatment),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  labs(x = "Consortium", y = "% Removal (Day 0 → 21)", title = "Naphthalene") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Phenanthrene plot
plot_phe <- ggplot(filter(removal_summary, compound == "phenanthrene"),
                   aes(x = consortia, y = mean_removal, fill = treatment)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_removal - se_removal,
                    ymax = mean_removal + se_removal,
                    group = treatment),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  labs(x = "Consortium", y = "% Removal (Day 0 → 21)", title = "Phenanthrene") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Fluoranthene plot
plot_fluor <- ggplot(filter(removal_summary, compound == "fluoranthene"),
                     aes(x = consortia, y = mean_removal, fill = treatment)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_removal - se_removal,
                    ymax = mean_removal + se_removal,
                    group = treatment),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  labs(x = "Consortium", y = "% Removal (Day 0 → 21)", title = "Fluoranthene") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#combine plots
plot_naph / plot_phe / plot_fluor

##truncate plot to make it easier to read
# Main figure: zoomed-in version with raw replicate points
ggplot(removal_summary, aes(x = treatment, y = mean_removal, fill = treatment)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_removal - se_removal,
                    ymax = mean_removal + se_removal,
                    group = treatment),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  
  # Overlay replicate points
  geom_jitter(data = removal_reps %>%
                pivot_longer(cols = starts_with("perc_removal"),
                             names_to = "compound",
                             values_to = "perc_removal") %>%
                mutate(compound = str_replace(compound, "perc_removal_", "")),
              aes(x = treatment, y = perc_removal, color = treatment),
              width = 0.2, alpha = 0.6, inherit.aes = FALSE) +
  
  facet_wrap(~compound + consortia, scales = "free_y") +
  labs(x = "Consortium", y = "% Removal (Day 0 → 21)", fill = "Treatment") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # Zoom in to ignore extreme outlier in main figure
  coord_cartesian(ylim = c(-200, 120))
# supplementary plot with full scale
ggplot(removal_summary, aes(x = treatment, y = mean_removal, fill = treatment)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_removal - se_removal,
                    ymax = mean_removal + se_removal,
                    group = treatment),
                width = 0.3,
                position = position_dodge(width = 0.9)) +
  geom_jitter(data = removal_reps %>%
                pivot_longer(cols = starts_with("perc_removal"),
                             names_to = "compound",
                             values_to = "perc_removal") %>%
                mutate(compound = str_replace(compound, "perc_removal_", "")),
              aes(x = treatment, y = perc_removal, color = treatment),
              width = 0.2, alpha = 0.6, inherit.aes = FALSE) +
  facet_wrap(~compound + consortia, scales = "free_y") +
  labs(x = "Consortium", y = "% Removal (Day 0 → 21)", fill = "Treatment") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##STATS
library(lme4)
library(lmerTest)

#global anova - based on significance, can now separate into categories and run anova/tukey
# -------POST-HOC TESTING ----------

##anova factor fix
lmm_posthoc_by_compound <- function(df, cmpd) {
  dat <- df %>%
    filter(compound == cmpd) %>%
    mutate(
      treatment = factor(treatment),
      consortia = factor(consortia),
      day = factor(day, levels = c("0","7","14","21", "42")),  # enforce categorical days
      sample = factor(sample)
    )
  
  # Fit model
  model <- lmer(value ~ treatment * consortia * day + (1|sample), data = dat)
  
  # --- ANOVA table with sig stars ---
  anova_tbl <- anova(model, type = 3) %>%
    as.data.frame() %>%
    rownames_to_column("Effect") %>%
    mutate(
      compound = cmpd,
      sig = case_when(
        `Pr(>F)` < 0.001 ~ "***",
        `Pr(>F)` < 0.01  ~ "**",
        `Pr(>F)` < 0.05  ~ "*",
        `Pr(>F)` < 0.1   ~ ".",
        TRUE             ~ ""
      )
    )
  
  # --- Post-hoc testing (3-way breakdown) ---
  emm_3way <- emmeans(model, ~ treatment * consortia | day)
  contrasts <- pairs(emm_3way, adjust = "tukey")
  
  # Return everything as a list
  list(
    model        = model,
    emm          = emm_3way,
    anova_tbl    = anova_tbl,
    emm_df       = as.data.frame(emm_3way) %>% mutate(compound = cmpd),
    contrasts_df = as.data.frame(contrasts) %>% mutate(compound = cmpd)
  )
}

# Run across compounds
posthoc_results <- lapply(compounds, function(cmpd) lmm_posthoc_by_compound(long_with_flags, cmpd))

# Extract tidy dataframes
anova_all     <- bind_rows(lapply(posthoc_results, function(x) x$anova_tbl))
emm_all       <- bind_rows(lapply(posthoc_results, function(x) x$emm_df))
contrasts_all <- bind_rows(lapply(posthoc_results, function(x) x$contrasts_df))

# View results
print(anova_all)      # global ANOVA with sig stars, by PAH
print(emm_all)        # estimated marginal means (per treatment × consortia × day × PAH)
print(contrasts_all)  # pairwise contrasts with p-values, by PAH

contrasts_all <- contrasts_all %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ "ns"
    )
  )

##POST HOCS

# Example for PHE
model.phe <- lmm_posthoc_by_compound(long_with_flags, "phenanthrene")
contrast(
  model.phe$emm,
  method = "pairwise",
  by = c("treatment","day")
)

contrast(
  model.phe$emm,
  method = "pairwise",
  by = c("consortia","day")
)


##NAP
model.nap <- lmm_posthoc_by_compound(long_with_flags, "naphthalene")

# Consortia differences within each treatment and day
contrast(
  model.nap$emm,
  method = "pairwise",
  by = c("treatment", "day")
)

# Treatment differences within each consortia and day
contrast(
  model.nap$emm,
  method = "pairwise",
  by = c("consortia", "day")
)

##FLA
model.fla <- lmm_posthoc_by_compound(long_with_flags, "fluoranthene")

# Consortia differences within each treatment and day
contrast(
  model.fla$emm,
  method = "pairwise",
  by = c("treatment", "day")
)

# Treatment differences within each consortia and day
contrast(
  model.fla$emm,
  method = "pairwise",
  by = c("consortia", "day")
)

########removal stats
removal_anova_tidy <- lapply(compounds, function(cmpd) {
  colname <- paste0("perc_removal_", cmpd)
  
  dat <- removal_reps %>%
    dplyr::select(any_of(c("sample","consortia","treatment", colname))) %>%
    dplyr::rename(removal = !!sym(colname))
  
  fit <- aov(removal ~ treatment * consortia, data = dat)
  
  broom::tidy(fit) %>%
    mutate(
      compound = cmpd,
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".",
        TRUE            ~ ""
      )
    )
}) %>% bind_rows()

print(removal_anova_tidy)

########## CODE ARCHIVE
#summarize stats
sum_compounds <- merged.consortia.adj %>%
  group_by(consortia, day, treatment) %>%
  summarise(
    naphthalene_mean = mean(naphthalene, na.rm = TRUE),
    naphthalene_se   = sd(naphthalene, na.rm = TRUE) / sqrt(n()),
    
    phenanthrene_mean = mean(phenanthrene, na.rm = TRUE),
    phenanthrene_se   = sd(phenanthrene, na.rm = TRUE) / sqrt(n()),
    
    fluoranthene_mean = mean(fluoranthene, na.rm = TRUE),
    fluoranthene_se   = sd(fluoranthene, na.rm = TRUE) / sqrt(n())
  )

#wide to long
sum_long <- sum_compounds %>%
  pivot_longer(
    cols = ends_with(c("_mean", "_se")),
    names_to = c("compound", ".value"),
    names_pattern = "(.*)_(mean|se)"
  )
##flags redo
merged.consortia.adj <- merged.consortia.adj %>%
  mutate(
    naphthalene_bdl  = is.na(naphthalene)  | naphthalene  < 0.005,
    phenanthrene_bdl = is.na(phenanthrene) | phenanthrene < 3.145,
    fluoranthene_bdl = is.na(fluoranthene) | fluoranthene < 0.165,
    
    naphthalene  = ifelse(is.na(naphthalene), 0.005 / 2, naphthalene),
    phenanthrene = ifelse(is.na(phenanthrene), 3.145 / 2, phenanthrene),
    fluoranthene = ifelse(is.na(fluoranthene), 0.165 / 2, fluoranthene)
  )
long_with_flags <- merged.consortia.adj %>%
  pivot_longer(
    cols = c(naphthalene, phenanthrene, fluoranthene),
    names_to = "compound",
    values_to = "value"
  ) %>%
  mutate(
    bdl_flag = case_when(
      compound == "naphthalene"  ~ naphthalene_bdl,
      compound == "phenanthrene" ~ phenanthrene_bdl,
      compound == "fluoranthene" ~ fluoranthene_bdl
    )
  )
#plot
ggplot(long_with_flags,
       aes(x = day, y = value, color = treatment,
           shape = factor(bdl_flag, levels = c(FALSE, TRUE)))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  scale_shape_manual(
    name = "Detection",
    values = c("FALSE" = 16, "TRUE" = 1),   # filled vs open circles
    labels = c("Detected", "<LOD")
  ) +
  facet_wrap(~compound + consortia, ncol = 4)

ggplot(data = sum_long,
       aes(x = day, y = mean, color = consortia, group = treatment)) +
  geom_line(aes(linetype = treatment)) +
  geom_point(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se,
                    group = interaction(consortia, treatment)),
                position = position_dodge(.1, preserve = "single"),
                width = 3, alpha = 1) +
  theme_pubr() +
  xlab("Day") +
  ylab("Concentration (ng/mL)") +
  facet_grid(~consortia + compound, ncol = 2) +
  scale_linetype_manual(
    name = "Treatment",
    values = linetype,
    labels = c("Capsule", "Planktonic")
  ) +
  guides(color = "none") +
  scale_x_continuous(breaks = seq(0, 42, 21))
##
ggplot(data = sum_long,
       aes(x = day, y = mean, color = consortia, group = treatment)) +
  geom_line(aes(linetype = treatment)) +
  geom_point(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se,
                    group = interaction(consortia, treatment)),
                position = position_dodge(.1, preserve = "single"),
                width = 3, alpha = 1) +
  theme_pubr() +
  xlab("Day") +
  ylab("Concentration (ng/mL)") +
  facet_wrap(~ consortia + compound, ncol = 3) +   #4 across, 3 down
  scale_linetype_manual(
    name = "Treatment",
    values = linetype,
    labels = c("Capsule", "Planktonic")
  ) +
  guides(color = "none") +
  scale_x_continuous(breaks = seq(0, 42, 21))
#color instead of linetype
ggplot(data = sum_long,
       aes(x = day, y = mean, color = treatment, group = treatment)) +
  geom_line() +
  geom_point(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se,
                    group = interaction(consortia, treatment)),
                position = position_dodge(.1, preserve = "single"),
                width = 3, alpha = 1) +
  theme_pubr() +
  xlab("Day") +
  ylab("Concentration (ng/mL)") +
  facet_wrap(~ compound + consortia, ncol = 4) +
  guides(color = guide_legend(title = "Treatment")) +   # legend for treatment
  scale_x_continuous(breaks = seq(0, 42, 21))

##mixed model full workflow
long_with_flags <- long_with_flags %>%
  mutate(
    treatment = as.factor(treatment),
    day       = as.factor(day),
    consortia = as.factor(consortia),
    sample    = as.factor(sample),
    compound  = as.factor(compound)
  )
glimpse(long_with_flags)

# Function for one compound
lmm_anova_by_compound <- function(df, cmpd) {
  dat <- filter(df, compound == cmpd) %>%
    mutate(
      treatment = factor(treatment),
      consortia = factor(consortia),
      # Force factor with explicit levels in order
      day = factor(day, levels = c("0","7","14","21")),
      sample = factor(sample)
    )
  
  # Mixed model: includes 3-way interaction
  model <- lmer(value ~ treatment * consortia * day + (1|sample), data = dat)
  
  # ANOVA table (Type III tests)
  anova_tbl <- anova(model, type = 3) %>%
    as.data.frame() %>%
    rownames_to_column("Effect") %>%
    mutate(compound = cmpd)
  
  return(anova_tbl)
}

# Run across compounds
compounds <- unique(long_with_flags$compound)

anova_results <- bind_rows(
  lapply(compounds, function(cmpd) lmm_anova_by_compound(long_with_flags, cmpd))
) %>%
  # add stars column based on p-value
  mutate(sig = case_when(
    `Pr(>F)` < 0.001 ~ "***",
    `Pr(>F)` < 0.01  ~ "**",
    `Pr(>F)` < 0.05  ~ "*",
    `Pr(>F)` < 0.1   ~ ".",
    TRUE             ~ ""
  ))

# View results
print(anova_results)