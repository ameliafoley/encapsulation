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