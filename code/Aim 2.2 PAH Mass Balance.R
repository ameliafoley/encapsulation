##Run Aim 2.2 PAH Figures first because this file depends on objects created in that one! 
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# constants
V_AQ <- 10   # mL aqueous per reactor
M_CAP <- 2   # g capsule per reactor

# prepare data: raw rows only, ensure types
df <- plot_data %>%
  filter(type == "raw") %>%
  mutate(
    day = as.character(day),
    value = as.numeric(value),
    treatment = as.character(treatment),
    consortia = as.character(consortia),
    sample = as.character(sample)
  )

library(dplyr)
library(tidyr)
library(stringr)

# df is your raw data: sample, compound, day, consortia, treatment, value (numeric)
# V_AQ and M_CAP already defined

# 1) split sample into parts and create sample_base (reactor id)
df2 <- df %>%
  mutate(sample = as.character(sample)) %>%
  separate(sample, into = c("p1", "p2", "p3"), sep = "-", fill = "right", remove = FALSE) %>%
  mutate(
    sample_base = case_when(
      !is.na(p3) & p3 == "c" ~ paste(p1, p2, sep = "-"),  # c-a-c -> c-a
      !is.na(p2)              ~ paste(p1, p2, sep = "-"),  # f-a -> f-a ; f-c -> f-c
      TRUE                    ~ p1
    ),
    sample_orig = .data$sample
  ) %>%
  select(-p1, -p2, -p3)

# 2) create one sample_orig_list per reactor (sample_base × compound × day × consortia)
orig_lists <- df2 %>%
  group_by(sample_base, compound, day, consortia) %>%
  summarise(sample_orig_list = list(unique(sample_orig)), .groups = "drop")

# 3) pivot values wider by sample_base (DO NOT include sample_orig_list in id_cols)
wide_vals <- df2 %>%
  select(sample_base, compound, day, consortia, treatment, value) %>%
  pivot_wider(
    id_cols = c(sample_base, compound, day, consortia),
    names_from = treatment,
    values_from = value,
    values_fn = list(value = mean),
    values_fill = NA_real_
  )

# 4) attach the precomputed sample_orig_list (one per reactor)
wide_by_reactor <- wide_vals %>%
  left_join(orig_lists, by = c("sample_base", "compound", "day", "consortia")) %>%
  rename(sample = sample_base)

# 5) diagnostic: show rows where cc exists (should have corresponding c or NA if missing)
check_cc <- wide_by_reactor %>%
  filter(!is.na(cc)) %>%
  select(sample, sample_orig_list, compound, day, consortia, c, cc, f)

if (nrow(check_cc) == 0) {
  message("No cc rows found (no capsule-material measurements).")
} else {
  message("Rows where cc is present (c should be on same row if supernatant exists):")
  print(check_cc %>% arrange(sample, compound, day) %>% slice_head(n = 80))
}

# 6) compute aqueous/capsule conc, masses and total mass
wide_by_reactor <- wide_by_reactor %>%
  mutate(
    aqueous_conc = coalesce(f, c),   # ng/mL: prefer f (free) else c (supernatant)
    capsule_conc = cc,               # ng/g
    mass_aqueous = if_else(!is.na(aqueous_conc), aqueous_conc * V_AQ, 0),
    mass_capsule = if_else(!is.na(capsule_conc), capsule_conc * M_CAP, 0),
    total_mass = mass_aqueous + mass_capsule,
    treatment_combined = case_when(
      !is.na(capsule_conc) | !is.na(c) | !is.na(cc) ~ "capsule",
      !is.na(f)                                        ~ "f",
      TRUE                                             ~ "other"
    )
  )

# 7) quick sample of the wide_by_reactor for manual inspection
print(wide_by_reactor %>% select(sample, sample_orig_list, compound, day, consortia, f, c, cc, aqueous_conc, capsule_conc, total_mass) %>% slice_head(n = 60))



# Pivot/aggregate to one row per reactor (sample × compound × day × consortia)
# For each reactor: compute mean aqueous conc (from treatment 'f' or 'c' rows if present),
# and mean capsule conc (from treatment 'cc' or possibly 'c' row if that is where capsule was measured).
#wide <- df %>%
#  mutate(
#    aqueous_conc = if_else(treatment %in% c("f", "c"), value, NA_real_),
#    capsule_conc = if_else(treatment == "cc", value, NA_real_)
#  ) %>%
#  group_by(sample, compound, day, consortia) %>%
#  summarise(
#    # average observed aqueous measurements for that reactor/day (if any)
 #   aqueous_conc = if (all(is.na(aqueous_conc))) NA_real_ else mean(aqueous_conc, na.rm = TRUE),
#    # average observed capsule measurements for that reactor/day (if any)
#    capsule_conc = if (all(is.na(capsule_conc))) NA_real_ else mean(capsule_conc, na.rm = TRUE),
    # list treatments seen for the reactor (useful to detect capsule presence)
#    treatments_seen = list(unique(treatment)),
#    .groups = "drop"
  )

# compute masses per reactor (ng)
#wide <- wide %>%
#  mutate(
#    mass_aqueous = if_else(!is.na(aqueous_conc), aqueous_conc * V_AQ, 0),     # ng
#    mass_capsule = if_else(!is.na(capsule_conc), capsule_conc * M_CAP, 0),   # ng (capsule_conc in ng/g)
#    total_mass = mass_aqueous + mass_capsule,
#    # collapse treatments: if reactor had any 'c' or 'cc' rows, call it "capsule"
#    treatment_combined = case_when(
#      map_lgl(treatments_seen, ~ any(.x %in% c("c", "cc"))) ~ "capsule",
#      map_lgl(treatments_seen, ~ any(.x == "f"))                 ~ "f",
#      TRUE                                                       ~ "other"
#    )
 # ) %>%
#  select(-treatments_seen)

# compute Day 0 baseline and percent degraded per reactor-group
mass_balance <- wide_by_reactor %>%
  group_by(compound, consortia, treatment_combined) %>%
  mutate(
    total_mass_day0 = mean(total_mass[day == "0"], na.rm = TRUE),
    frac_remaining = total_mass / total_mass_day0,
    perc_remaining = frac_remaining * 100,
    perc_degraded = (1 - frac_remaining) * 100
  ) %>%
  ungroup()

# summarize for plotting/statistics
mass_summary <- mass_balance %>%
  group_by(compound, consortia, treatment_combined, day) %>%
  summarise(
    mean_perc_remaining = mean(perc_remaining, na.rm = TRUE),
    se_perc_remaining   = ifelse(n() > 1, sd(perc_remaining, na.rm = TRUE) / sqrt(n()), NA_real_),
    mean_perc_degraded  = mean(perc_degraded, na.rm = TRUE),
    se_perc_degraded    = ifelse(n() > 1, sd(perc_degraded, na.rm = TRUE) / sqrt(n()), NA_real_),
    n = n(),
    .groups = "drop"
  )

# Plot (shows 'capsule' vs 'f' and will include abiotic consortia 'a' if present)
ggplot(mass_summary,
       aes(x = as.numeric(day), y = mean_perc_remaining,
           color = treatment_combined, group = treatment_combined)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_perc_remaining - se_perc_remaining,
                    ymax = mean_perc_remaining + se_perc_remaining),
                width = 0.8) +
  facet_wrap(~ compound + consortia) +
  scale_x_continuous(breaks = sort(unique(as.numeric(mass_summary$day)))) +
  labs(
    y = "Percent of Total PAH Mass Remaining (%)",
    x = "Day",
    color = "Treatment",
    title = "Mass balance over time (capsule combined)"
  ) +
  theme_classic(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

#PLot with formatting to match other figure
my_colors <- c(
  "capsule" = "#F8766D",  # salmon (capsule)
  "f"       = "#619CFF",  # blue (free)
  "a"       = "grey50",   # abiotic (if present)
  "other"   = "black"
)
# position dodge for points/lines/errorbars
pd <- position_dodge(width = 0.6)

# Plot: compound rows x consortia columns to match original layout
mass_summary$compound <- factor(
  mass_summary$compound,
  levels = c("naphthalene", "phenanthrene", "fluoranthene")
)
ggplot(mass_summary, 
       aes(x = as.numeric(day), 
           y = mean_perc_remaining,
           color = treatment_combined, group = treatment_combined)) +
   #raw replicate points (if you want to show them; requires mass_balance with reactor rows)
   geom_point(data = mass_balance, 
             aes(x = as.numeric(day), y = perc_remaining, color = treatment_combined),
             position = pd, alpha = 0.35, size = 1.5, show.legend = FALSE) +
   #summary line + points
  geom_line(size = 0.9, position = pd) +
  geom_point(size = 2.5, position = pd) +
  # error bars (summary)
  geom_errorbar(aes(ymin = mean_perc_remaining - se_perc_remaining,
                    ymax = mean_perc_remaining + se_perc_remaining),
                width = 0.6, position = pd) +
  # fixed y-scale across all facets so panels are directly comparable
  facet_grid(
    rows = vars(compound),
    cols = vars(consortia),
    scales = "fixed",
    switch = "both",
    labeller = labeller(
      consortia = as_labeller(c(
        "a" = "Abiotic",
        "k" = "K-strat",
        "m" = "Mixed",
        "r" = "R-strat"
      )),
      compound = function(x) stringr::str_to_sentence(x)
    )) +
  scale_color_manual(values = my_colors, na.value = "black", name = "Treatment") +
  scale_x_continuous(breaks = sort(unique(as.numeric(mass_summary$day))),
                     expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    title = NULL,   # keep per-plot titles off; compound row labels serve as titles
    x = "Day",
    y = "Percent of Total PAH Mass Remaining (%)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title.y = element_text(margin = margin(r = 8)),
    panel.spacing = unit(1, "lines"),
    # This forces axis lines, ticks, and text for every facet
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    axis.text.x = element_text(),
    axis.title.x = element_text(),
    # Ensures each facet shows its own x-axis at the bottom
    strip.placement = "outside"
  )

##new plots
library(ggplot2)
library(patchwork)
library(stringr)
library(dplyr)

# Colors for treatments
pal <- c("capsule" = "#F8766D", "f" = "#619CFF")

# Ensure correct compound order
compound_levels <- c("naphthalene", "phenanthrene", "fluoranthene")
mass_summary2 <- mass_summary %>%
  filter(treatment_combined %in% c("capsule", "f")) %>%
  mutate(compound = factor(compound, levels = compound_levels))

# Set consistent y-axis limits
ymin <- min(mass_summary2$mean_perc_remaining - mass_summary2$se_perc_remaining, na.rm = TRUE)
ymax <- max(mass_summary2$mean_perc_remaining + mass_summary2$se_perc_remaining, na.rm = TRUE)
pad <- (ymax - ymin) * 0.08
#y_limits <- c(max(0, ymin - pad), ymax + pad)
y_limits <- c(0, 150)

# Consortia order and labels
cons_order <- c("a","k","m","r")
cons_labels <- c("a" = "Abiotic", "k" = "K-strat", "m" = "Mixed", "r" = "R-strat")

# Function: make one PAH plot
plot_mass_compound <- function(cmpd) {
  df <- mass_summary2 %>% filter(compound == cmpd)
  
  ggplot(df, aes(x = as.numeric(day), y = mean_perc_remaining,
                 color = treatment_combined, group = treatment_combined)) +
    geom_line(size = 0.9, position = position_dodge(width = 0.5)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = mean_perc_remaining - se_perc_remaining,
                      ymax = mean_perc_remaining + se_perc_remaining),
                  width = 0.5, position = position_dodge(width = 0.5)) +
    facet_wrap(~ consortia, ncol = 4,
               labeller = labeller(consortia = cons_labels)) +
    scale_color_manual(values = pal, name = "Treatment",
                       labels = c("capsule" = "capsule", "f" = "free")) +
    coord_cartesian(ylim = y_limits) +
    scale_x_continuous(breaks = sort(unique(as.numeric(df$day)))) +
    labs(
      title = str_to_sentence(cmpd),
      x = "Day",
      y = NULL  # remove per-plot y label; add shared one later
    ) +
    theme_classic(base_size = 12) +
    theme(
      strip.background = element_rect(fill = "grey90", color = NA),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0, vjust = 1.5, size = 12),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      panel.spacing = unit(1, "lines"),
      axis.title.x = element_text(margin = margin(t = 8))
    )
}
#

# Build three plots
p_nap  <- plot_mass_compound("naphthalene")
p_phen <- plot_mass_compound("phenanthrene")
p_flua <- plot_mass_compound("fluoranthene")

# Combine vertically with shared legend
combined <- (p_nap / p_phen / p_flua) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Add a single centered y-axis label
final_plot <- combined +
  plot_annotation(
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    tag_levels = NULL
  ) &
  labs(y = "Percent of Total PAH Mass Remaining (%)") &
  theme(
    axis.title.y = element_text(angle = 90, vjust = 0.5, size = 12)
  )

# Display
print(final_plot)


##fix overlapping y-axis labels!

library(patchwork)
library(cowplot)   # for ggdraw() + draw_label()

# 1) Make versions of each plot with no per-plot y label and keep legend on one plot only
p_nap_no_y  <- p_nap  + labs(y = NULL) + theme(legend.position = "none")
p_phen_no_y <- p_phen + labs(y = NULL) + theme(legend.position = "none")
p_flua_no_y <- p_flua + labs(y = NULL) + theme(legend.position = "none")

# pick one plot to keep the legend (or build combined with guides="collect" if all have legends)
# If you used guides="collect" previously, make sure at least one plot still has a legend:
p_flua_legend <- p_flua + theme(legend.position = "bottom")  # use bottom legend from bottom plot

# 2) Combine plots with a single collected legend (use the one plot that keeps legend)
combined_pw <- (p_nap_no_y / p_phen_no_y / p_flua_legend) + plot_layout(guides = "collect")&
  theme(legend.position = "bottom")

# 3) Draw a single left-side vertical y-axis label for the whole assembled figure
shared_y_label <- "Percent of Total PAH Mass Remaining (%)"

# use ggdraw to add the vertical label on the left
final <- ggdraw(combined_pw) +
  draw_label(shared_y_label,
             x = 0.001,    # distance from left edge; tweak (0.01-0.05) if needed
             y = 0.5,
             angle = 90,
             vjust = 0.5,
             size = 12)
# widen the left margin so label has space
final <- final + theme(plot.margin = margin(5, 5, 5, 20))  # 40 pts on left; tweak as needed


# 4) show it
print(final)

# Optional: save to file
# ggsave("mass_balance_final_with_shared_y_and_legend.png", final, width = 12, height = 9, dpi = 300)



# Day 21 summary table
mass_day21 <- mass_summary %>%
  filter(day == "21") %>%
  arrange(compound, consortia, treatment_combined)

print(mass_day21)

##STATS ON MASS BALANCE ##
# =====================================================================
# Statistical analysis on mass balance data
# =====================================================================

library(lme4)
library(emmeans)
library(dplyr)
library(tibble)

# Helper: run LMM + post-hoc per compound
lmm_posthoc_mass <- function(mass_balance, cmpd) {
  dat <- mass_balance %>%
    filter(compound == cmpd) %>%
    mutate(
      treatment_combined = factor(treatment_combined, levels = c("f", "capsule")),
      consortia = factor(consortia),
      day = factor(day, levels = sort(unique(day))),
      sample = factor(sample)
    )
  
  # Fit mixed model: % remaining depends on treatment × consortia × day
  # Random intercept for each reactor (sample)
  model <- lmer(perc_remaining ~ treatment_combined * consortia * day + (1 | sample), data = dat)
  
  # --- ANOVA table ---
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
  
  # --- Estimated marginal means ---
  emm <- emmeans(model, ~ treatment_combined * consortia | day)
  
  # --- Pairwise contrasts (capsule vs free) ---
  contrasts <- pairs(emm, adjust = "tukey")
  
  list(
    model = model,
    anova_tbl = anova_tbl,
    emm_df = as.data.frame(emm) %>% mutate(compound = cmpd),
    contrasts_df = as.data.frame(contrasts) %>% mutate(compound = cmpd)
  )
}

# =====================================================================
# Run across all compounds
# =====================================================================
compounds <- unique(mass_balance$compound)

mass_results <- lapply(compounds, function(cmpd) lmm_posthoc_mass(mass_balance, cmpd))

anova_all      <- bind_rows(lapply(mass_results, \(x) x$anova_tbl))
emmeans_all    <- bind_rows(lapply(mass_results, \(x) x$emm_df))
contrasts_all  <- bind_rows(lapply(mass_results, \(x) x$contrasts_df))

# =====================================================================
# Inspect key outputs
# =====================================================================
print(anova_all)       # overall effects (Treatment × Consortia × Day)
print(emmeans_all)     # marginal means (useful for plotting stats)
print(contrasts_all)   # pairwise contrasts (capsule vs free, per consortia × day)

library(dplyr)
library(tidyr)
library(stringr)

# 1) Basic check
if (!exists("contrasts_all")) stop("contrasts_all not found. Run contrasts step first.")

# 2) Parse contrast strings robustly
contrasts_parsed <- contrasts_all %>%
  mutate(contrast = as.character(contrast),
         day = as.character(day)) %>%
  # split around the " - " (allow spaces around dash)
  separate(contrast, into = c("left_raw", "right_raw"), sep = "\\s*-\\s*", fill = "right", extra = "merge") %>%
  # extract tokens: assume format "<treatment> <consortia>" on each side
  mutate(
    left_raw = str_squish(left_raw),
    right_raw = str_squish(right_raw),
    left_treatment = str_extract(left_raw, "^[^\\s]+"),
    left_consortia = str_extract(left_raw, "[^\\s]+$"),
    right_treatment = str_extract(right_raw, "^[^\\s]+"),
    right_consortia = str_extract(right_raw, "[^\\s]+$")
  ) %>%
  # normalize treatment labels to "capsule" vs "f"
  mutate(
    left_t_simple = case_when(
      str_detect(left_treatment, regex("^(c$|c$|cc$|cap|capsule|C$|CC$)", ignore_case = TRUE)) ~ "capsule",
      str_detect(left_treatment, regex("^(f$|free$)", ignore_case = TRUE)) ~ "f",
      TRUE ~ tolower(left_treatment)
    ),
    right_t_simple = case_when(
      str_detect(right_treatment, regex("^(c$|cc$|cap|capsule|C$|CC$)", ignore_case = TRUE)) ~ "capsule",
      str_detect(right_treatment, regex("^(f$|free$)", ignore_case = TRUE)) ~ "f",
      TRUE ~ tolower(right_treatment)
    )
  ) %>%
  # significance stars
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  )

# 3) Keep only capsule vs f contrasts that are within the same consortia
contrasts_capsule_vs_free <- contrasts_parsed %>%
  filter(
    !is.na(left_consortia) & !is.na(right_consortia) & (left_consortia == right_consortia),
    ((left_t_simple == "capsule" & right_t_simple == "f") |
       (left_t_simple == "f"       & right_t_simple == "capsule"))
  ) %>%
  # standardize a column for consortia and a consistent direction (estimate = capsule - f)
  mutate(
    consortia = left_consortia,
    # ensure estimate is capsule minus f (if not, flip sign)
    # note: 'estimate' column from emmeans pairs may represent left - right depending on how pairs() was made
    # We will compute a consistent 'est_capsule_minus_f' using left/right mapping when available:
    est_capsule_minus_f = case_when(
      left_t_simple == "capsule" & right_t_simple == "f" ~ estimate,
      left_t_simple == "f" & right_t_simple == "capsule" ~ -estimate,
      TRUE ~ estimate
    ),
    # direction label for interpretation
    direction = case_when(
      est_capsule_minus_f > 0 ~ "capsule > free",
      est_capsule_minus_f < 0 ~ "capsule < free",
      TRUE ~ "no diff"
    )
  ) %>%
  arrange(compound, as.numeric(day), consortia)

# 4) Subset Day 21 (or "21") and significant ones
contrasts_day21 <- contrasts_capsule_vs_free %>% filter(day %in% c("21", 21))
contrasts_day21_sig <- contrasts_day21 %>% filter(p.value < 0.05)

# 5) Create a small annotation table for plotting (one label per compound × consortia)
# we pick label = sig (***, **, *, .) and numeric p.value too
annotation_table <- contrasts_day21 %>%
  mutate(
    sig_label = ifelse(p.value < 0.05, sig, ""),   # show only significant stars
    p_label = sprintf("p = %.3g", p.value),
    plot_label = ifelse(sig_label == "", p_label, paste0(sig_label, " (", p_label, ")"))
  ) %>%
  select(compound, consortia, day, est_capsule_minus_f, p.value, sig_label, plot_label)

# 6) Print summaries
message("All capsule vs free contrasts (sample):")
print(contrasts_capsule_vs_free %>% select(compound, day, consortia, left_treatment, right_treatment, est_capsule_minus_f, p.value, sig) %>% slice_head(n = 30))

message("Significant contrasts at Day 21:")
print(contrasts_day21_sig %>% select(compound, day, consortia, est_capsule_minus_f, p.value, sig) %>% arrange(compound, consortia))

# 7) assign to global env for later use
contrasts_parsed <- contrasts_parsed
contrasts_capsule_vs_free <- contrasts_capsule_vs_free
contrasts_day21 <- contrasts_day21
contrasts_day21_sig <- contrasts_day21_sig
annotation_table <- annotation_table


## ADD ABIOTIC-CONTROL COMPARISONS ##

# ---- Identify comparisons against abiotic control ----
contrasts_vs_abiotic <- contrasts_parsed %>%
  filter(
    # keep only comparisons where one side is abiotic (a) and the other is biotic (k, m, r)
    (left_consortia == "a" & right_consortia %in% c("k", "m", "r")) |
      (right_consortia == "a" & left_consortia %in% c("k", "m", "r"))
  ) %>%
  mutate(
    # consortia_biotic: which consortia the biotic treatment belongs to
    consortia_biotic = if_else(left_consortia == "a", right_consortia, left_consortia),
    # ensure estimate is "biotic - abiotic"
    est_biotic_minus_abiotic = if_else(left_consortia == "a", -estimate, estimate),
    direction = case_when(
      est_biotic_minus_abiotic > 0 ~ "biotic > abiotic",
      est_biotic_minus_abiotic < 0 ~ "biotic < abiotic",
      TRUE ~ "no diff"
    )
  ) %>%
  arrange(compound, as.numeric(day), consortia_biotic)

# ---- Filter significant comparisons (p < 0.05) ----
contrasts_vs_abiotic_sig <- contrasts_vs_abiotic %>%
  filter(p.value < 0.05)

# ---- Day 21 subset ----
contrasts_vs_abiotic_day21 <- contrasts_vs_abiotic %>%
  filter(day %in% c("21", 21))

# ---- Print summaries ----
message("Significant differences from Abiotic control (p < 0.05):")
print(contrasts_vs_abiotic_sig %>%
        select(compound, day, consortia_biotic,
               left_treatment, right_treatment, est_biotic_minus_abiotic,
               p.value, sig, direction) %>%
        arrange(compound, day, consortia_biotic))

message("Day 21 biotic vs abiotic comparisons:")
print(contrasts_vs_abiotic_day21 %>%
        select(compound, day, consortia_biotic,
               left_treatment, right_treatment,
               est_biotic_minus_abiotic, p.value, sig, direction) %>%
        arrange(compound, consortia_biotic))

# ---- Export useful data frames ----
assign("contrasts_vs_abiotic", contrasts_vs_abiotic, envir = .GlobalEnv)
assign("contrasts_vs_abiotic_sig", contrasts_vs_abiotic_sig, envir = .GlobalEnv)
assign("contrasts_vs_abiotic_day21", contrasts_vs_abiotic_day21, envir = .GlobalEnv)

##ABIOTIC - ONLY WITHIN TREATMENT COMPARISONS##
library(dplyr)
library(tidyr)
library(stringr)

# sanity
if (!exists("contrasts_all")) stop("contrasts_all not found. Run contrasts step first.")

# Filter: same-treatment abiotic vs biotic comparisons (abiotic = 'a', biotic ∈ {k,m,r})
contrasts_abiotic_same_treatment <- contrasts_parsed %>%
  filter(
    # one side must be abiotic and the other biotic
    ((left_consortia == "a" & right_consortia %in% c("k","m","r")) |
       (right_consortia == "a" & left_consortia %in% c("k","m","r"))),
    # AND the treatment type must be the same on both sides (both 'capsule' OR both 'f')
    left_t_simple == right_t_simple
  ) %>%
  mutate(
    # which biotic consortia (k/m/r) is being compared
    consortia_biotic = if_else(left_consortia == "a", right_consortia, left_consortia),
    treatment_type = left_t_simple,
    # compute a consistent estimate = biotic - abiotic
    est_biotic_minus_abiotic = case_when(
      # if left is abiotic and right is biotic, then estimate = right - left -> already right - left
      left_consortia == "a" & right_consortia %in% c("k","m","r") ~ estimate,
      # if right is abiotic and left is biotic, then left - right -> but estimate is left - right so keep it
      right_consortia == "a" & left_consortia %in% c("k","m","r") ~ estimate,
      TRUE ~ estimate
    ),
    direction = case_when(
      est_biotic_minus_abiotic > 0 ~ "biotic > abiotic",
      est_biotic_minus_abiotic < 0 ~ "biotic < abiotic",
      TRUE ~ "no diff"
    )
  ) %>%
  arrange(compound, as.numeric(day), treatment_type, consortia_biotic)

# Day-21 subset (string or numeric '21')
contrasts_abiotic_same_treatment_day21 <- contrasts_abiotic_same_treatment %>%
  filter(day %in% c("21", 21))

# Significant ones (p < 0.05)
contrasts_abiotic_same_treatment_sig <- contrasts_abiotic_same_treatment %>%
  filter(p.value < 0.05)

# Annotation table for plotting (one row per compound × consortia_biotic × treatment_type)
# show stars only if significant; otherwise optionally show p-value
annotation_table <- contrasts_abiotic_same_treatment_day21 %>%
  mutate(
    sig_label = ifelse(p.value < 0.05, sig, ""),
    p_label = sprintf("p=%.3g", p.value),
    plot_label = ifelse(sig_label == "", p_label, paste0(sig_label, " (", p_label, ")"))
  ) %>%
  select(compound, consortia = consortia_biotic, treatment_type, day, est_biotic_minus_abiotic, p.value, sig_label, plot_label)

# Assign outputs to global env for convenience
assign("contrasts_abiotic_same_treatment", contrasts_abiotic_same_treatment, envir = .GlobalEnv)
assign("contrasts_abiotic_same_treatment_day21", contrasts_abiotic_same_treatment_day21, envir = .GlobalEnv)
assign("contrasts_abiotic_same_treatment_sig", contrasts_abiotic_same_treatment_sig, envir = .GlobalEnv)
assign("annotation_table_abiotic_same_treatment", annotation_table, envir = .GlobalEnv)

# Print brief summaries
message("Examples of abiotic vs biotic (same treatment) contrasts (sample):")
print(contrasts_abiotic_same_treatment %>% select(compound, day, treatment_type, consortia_biotic, estimate, SE, t.ratio, p.value, sig) %>% slice_head(n = 30))

message("Significant same-treatment abiotic vs biotic contrasts (p<0.05):")
print(contrasts_abiotic_same_treatment_sig %>% select(compound, day, treatment_type, consortia_biotic, est_biotic_minus_abiotic, p.value, sig) %>% arrange(compound, day, consortia_biotic))

message("Annotation table (Day 21) preview:")
print(annotation_table %>% arrange(compound, consortia, treatment_type))


##compare day 0 vs 21 in abiotic controls
library(dplyr)
library(broom)   # for tidy summaries
library(tibble)

# Filter to abiotic controls only
abiotic_df <- mass_balance %>%
  filter(consortia == "a", day %in% c("0","21")) %>%
  mutate(
    day = factor(day, levels = c("0","21")),
    treatment_combined = factor(treatment_combined, levels = c("f","capsule"))
  )

# Helper: run model and extract p for day effect per treatment × compound
compare_day0_day21 <- function(df, cmpd, treat) {
  dat <- df %>%
    filter(compound == cmpd, treatment_combined == treat)
  if (nrow(dat) < 3) return(NULL)
  
  # simple model: percent remaining ~ day
  mod <- lm(perc_remaining ~ day, data = dat)
  an <- anova(mod)
  res <- tidy(an)
  
  tibble(
    compound = cmpd,
    treatment_combined = treat,
    df = res$df[1],
    F_value = res$statistic[1],
    p_value = res$p.value[1]
  )
}

# Apply across compounds and treatments
daychange_results <- bind_rows(lapply(unique(abiotic_df$compound), function(cmpd) {
  bind_rows(lapply(unique(abiotic_df$treatment_combined), function(trt) {
    compare_day0_day21(abiotic_df, cmpd, trt)
  }))
})) %>%
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE ~ "ns"
    )
  )

print(daychange_results)

# Optional: pairwise means to show direction
means_by_day <- abiotic_df %>%
  group_by(compound, treatment_combined, day) %>%
  summarise(mean_perc_remaining = mean(perc_remaining, na.rm = TRUE),
            se = sd(perc_remaining, na.rm = TRUE)/sqrt(n()),
            n = n(), .groups = "drop")

print(means_by_day)



