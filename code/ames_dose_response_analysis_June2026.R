# Ames assay dose-response analysis
# Prepared for: Ames Assay Results 20260601 copy.xlsx
# Purpose: compare linear dose-response relationships for revertants counted at 48 and 72 hours.
# Method mirrors DeMarini et al. in spirit: use linear regression slopes as mutagenic potency,
# report slope +/- SE, p-value, R^2, and a simple >=2-fold response flag.

# -----------------------------
# 0) Packages
# -----------------------------
packages <- c("readxl", "dplyr", "tidyr", "janitor", "ggplot2", "broom", "purrr", "stringr", "readr")
missing <- packages[!packages %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)
invisible(lapply(packages, library, character.only = TRUE))

# -----------------------------
# 1) User inputs
# -----------------------------
input_file <- "Ames Assay Results 20260601 copy.xlsx"
output_dir <- "ames_outputs"
dir.create(output_dir, showWarnings = FALSE)

# Replace dose_value with the numeric concentrations from your dose-preparation table.
# Keep dose labels exactly as they appear in the Excel file.
# Example: dose_key <- tibble(dose = c("a", "b", "c", "d"), dose_value = c(0, 10, 25, 50))
dose_key <- tibble::tribble(
  ~dose, ~dose_value,
  "a",   50, #53 uL
  "b",   25, #26 uL
  "c",   13, #13.3 uL
  "d",   5 #5 uL 
)

# Set this to TRUE only if you want DMSO controls included as dose 0.
# In the uploaded sheet, DMSO rows do not have mix/day/sample grouping values, so the default is FALSE.
include_dmso_as_zero <- FALSE

# -----------------------------
# 2) Read and clean data
# -----------------------------
#raw <- readxl::read_excel(input_file, sheet = 1) |> janitor::clean_names()
raw <- read_excel(here::here("data", "Ames Assay Results 20260601 copy.xlsx"))|> janitor::clean_names()

# Expected cleaned column names:
# sample_no, samples, dose, mix, day, strain, s9, x48_hr, x72_hr, total
names(raw) <- stringr::str_replace_all(names(raw), "^x48_hr$", "hr48")
names(raw) <- stringr::str_replace_all(names(raw), "^x72_hr$", "hr72")

names_needed <- c("sample_no", "samples", "dose", "mix", "day", "strain", "s9", "hr48", "hr72", "total")
stopifnot(all(names_needed %in% names(raw)))

assay <- raw |>
  select(all_of(names_needed)) |>
  mutate(
    samples = stringr::str_to_lower(as.character(samples)),
    dose = stringr::str_to_lower(as.character(dose)),
    strain = as.character(strain),
    s9 = stringr::str_to_lower(as.character(s9)),
    mix = as.factor(mix),
    day = as.factor(day)
  )

# Treatment rows with labeled doses a-d.
treatment <- assay |>
  filter(dose %in% dose_key$dose) |>
  left_join(dose_key, by = "dose") |>
  mutate(dose = factor(dose, levels = dose_key$dose))

if (any(is.na(treatment$dose_value))) {
  stop("At least one treatment row did not receive a numeric dose_value. Check dose_key.")
}

# Optional controls: useful for background summaries and fold-change reference.
dmso <- assay |>
  filter(samples == "dmso") |>
  mutate(dose = "dmso", dose_value = 0)

# Long format for 48h, 72h additional, and true 72h total.
# hr72 is the additional day-3 count only.
# total is the true 72h count = hr48 + hr72.

long <- treatment |>
  mutate(
    total = if_else(
      is.na(total),
      hr48 + hr72,
      total
    )
  ) |>
  pivot_longer(
    cols = c(hr48, total),
    names_to = "endpoint",
    values_to = "revertants"
  ) |>
  filter(!is.na(revertants)) |>
  mutate(
    endpoint = recode(
      endpoint,
      hr48 = "48 hr",
      total = "72 hr total"
    ),
    endpoint = factor(
      endpoint,
      levels = c("48 hr", "72 hr total")
    )
  )

# -----------------------------
# 3) Replicate summaries
# -----------------------------
summary_by_dose <- long |>
  group_by(samples, mix, day, strain, s9, endpoint, dose, dose_value) |>
  summarise(
    n = n(),
    mean_rev = mean(revertants, na.rm = TRUE),
    sd_rev = sd(revertants, na.rm = TRUE),
    se_rev = sd_rev / sqrt(n),
    .groups = "drop"
  )

readr::write_csv(summary_by_dose, file.path(output_dir, "summary_by_dose_endpoint.csv"))

# -----------------------------
# 4) Linear regressions by endpoint
# -----------------------------
dmso_ref <- assay |>
  filter(tolower(samples) == "dmso") |>
  mutate(
    total = if_else(is.na(total), hr48 + hr72, total)
  ) |>
  pivot_longer(
    cols = c(hr48, total),
    names_to = "endpoint",
    values_to = "dmso_revertants"
  ) |>
  mutate(
    endpoint = recode(
      endpoint,
      hr48 = "48 hr",
      total = "72 hr total"
    )
  ) |>
  group_by(strain, s9, endpoint) |>
  summarise(
    dmso_mean = mean(dmso_revertants, na.rm = TRUE),
    .groups = "drop"
  )
long <- long |>
  left_join(
    dmso_ref,
    by = c("strain", "s9", "endpoint")
  )

# Slope = revertants per unit dose_value. If dose_value is ug/plate, slope is rev/ug.
fit_one <- function(df) {
  
  df <- df |> filter(!is.na(dose_value), !is.na(revertants))
  
  if (nrow(df) < 2 || length(unique(df$dose_value)) < 2) {
    return(tibble(
      intercept = NA_real_,
      slope = NA_real_,
      slope_se = NA_real_,
      p_value = NA_real_,
      r_squared = NA_real_,
      n = nrow(df),
      max_fold_dmso = NA_real_,
      positive = FALSE
    ))
  }
  
  fit <- lm(revertants ~ dose_value, data = df)
  
  td <- broom::tidy(fit)
  gl <- broom::glance(fit)
  
  slope_row <- td |> filter(term == "dose_value")
  
  dmso_mean <- unique(df$dmso_mean)[1]
  
  max_fold_dmso <- max(df$revertants, na.rm = TRUE) / dmso_mean
  
  tibble(
    intercept = td$estimate[td$term == "(Intercept)"][1],
    slope = slope_row$estimate[1],
    slope_se = slope_row$std.error[1],
    p_value = slope_row$p.value[1],
    r_squared = gl$r.squared[1],
    n = nrow(df),
    max_fold_dmso = max_fold_dmso,
    
    # Ames criterion: significant trend AND ≥2-fold induction
    positive = !is.na(p_value) &&
      p_value <= 0.05 &&
      max_fold_dmso >= 2
  )
}

potency_by_endpoint <- long |>
  group_by(samples, mix, day, strain, s9, endpoint) |>
  nest() |>
  mutate(model = purrr::map(data, fit_one)) |>
  select(-data) |>
  unnest(model)

readr::write_csv(potency_by_endpoint, file.path(output_dir, "linear_potency_by_endpoint.csv"))

# -----------------------------
# 5) Compare 48h vs 72h slopes using an interaction model
# -----------------------------
# The endpoint:dose_value interaction p-value tests whether the dose-response slope differs by count time.
compare_endpoints <- function(df) {
  
  if (n_distinct(df$endpoint) < 2) {
    return(tibble(
      slope_difference = NA_real_,
      p_value = NA_real_
    ))
  }
  
  fit <- lm(revertants ~ dose_value * endpoint, data = df)
  
  broom::tidy(fit) |>
    filter(grepl("dose_value:endpoint", term)) |>
    transmute(
      slope_difference = estimate,
      p_value = p.value
    )
}

slope_comparison <- long |>
  group_by(samples, mix, day, strain, s9) |>
  nest() |>
  mutate(model = map(data, compare_endpoints)) |>
  select(-data) |>
  unnest(model) |>
  arrange(samples, strain, s9, mix, day)

readr::write_csv(slope_comparison, file.path(output_dir, "compare_48h_vs_72h_slopes.csv"))

# -----------------------------
# 6) Optional: identify the initial linear portion with highest R^2
# -----------------------------
# This mirrors the paper's approach more closely: sort doses low-to-high, then fit prefixes
# of 3, 4, ... dose levels and choose the prefix with the highest R^2.
fit_initial_linear <- function(dat) {
  dose_order <- sort(unique(dat$dose_value))
  if (length(dose_order) < 3) return(NULL)
  candidates <- purrr::map_dfr(3:length(dose_order), function(k) {
    keep <- dose_order[seq_len(k)]
    d <- filter(dat, dose_value %in% keep)
    fit <- lm(revertants ~ dose_value, data = d)
    slope <- broom::tidy(fit) |> filter(term == "dose_value")
    gl <- broom::glance(fit)
    tibble(
      doses_used = paste(keep, collapse = ";"),
      max_dose_used = max(keep),
      n_plates = nrow(d),
      slope = slope$estimate,
      slope_se = slope$std.error,
      slope_p = slope$p.value,
      r_squared = gl$r.squared,
      adj_r_squared = gl$adj.r.squared
    )
  })
  candidates |> arrange(desc(r_squared), desc(n_plates)) |> slice(1)
}

initial_linear_results <- long |>
  group_by(samples, mix, day, strain, s9, endpoint) |>
  nest() |>
  mutate(model = map(data, fit_initial_linear)) |>
  select(-data) |>
  unnest(model) |>
  arrange(samples, strain, s9, mix, day, endpoint)

readr::write_csv(initial_linear_results, file.path(output_dir, "initial_linear_highest_r2.csv"))

# -----------------------------
# 7) Plots
# -----------------------------
p <- ggplot(long, aes(x = dose_value, y = revertants)) +
  geom_point(aes(color = samples), size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_grid(endpoint ~ samples + strain + s9 + day + mix, scales = "free_y") +
  labs(
    title = "Ames assay dose-response by count time",
    x = "Dose (replace dose_key values with actual units)",
    y = "Revertants per plate"
  ) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
#ggsave(file.path(output_dir, "dose_response_by_endpoint.png"), p, width = 16, height = 9, dpi = 300)

#dmso reference
dmso_ref <- assay |>
  filter(samples == "dmso") |>
  group_by(strain, s9) |>
  summarise(
    dmso_mean = mean(total, na.rm = TRUE),
    dmso_2x = 2 * dmso_mean,
    .groups = "drop"
  )

#mean and SE
plot_dat <- long |>
  group_by(
    samples,
    mix,
    day,
    strain,
    s9,
    endpoint,
    dose,
    dose_value
  ) |>
  summarise(
    mean_rev = mean(revertants, na.rm = TRUE),
    se_rev = sd(revertants, na.rm = TRUE) /
      sqrt(sum(!is.na(revertants))),
    n = sum(!is.na(revertants)),
    .groups = "drop"
  )
plot_dat <- plot_dat |>
  left_join(dmso_ref, by = c("strain", "s9"))

p <- ggplot(
  plot_dat,
  aes(x = dose_value, y = mean_rev)
) +
  geom_point(
    aes(color = samples),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = mean_rev - se_rev,
      ymax = mean_rev + se_rev,
      color = samples
    ),
    width = 0.05
  ) +
  geom_smooth(
    aes(color = samples),
    method = "lm",
    se = FALSE
  ) +
  facet_grid(
    endpoint ~ samples + strain + s9 + day + mix,
    scales = "free_y"
  ) +
  labs(
    title = "Ames assay dose-response by count time",
    x = "Dose",
    y = "Mean revertants per plate ± SE"
  ) +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p

p <- ggplot(
  plot_dat,
  aes(x = dose_value, y = mean_rev)
) +
  geom_hline(
    aes(yintercept = dmso_mean),
    linetype = "dashed",
    color = "blue"
  ) +
  geom_hline(
    aes(yintercept = dmso_2x),
    linetype = "dotted",
    color = "red"
  ) +
  geom_point(
    aes(color = samples),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = mean_rev - se_rev,
      ymax = mean_rev + se_rev,
      color = samples
    ),
    width = 0.05
  ) +
  geom_smooth(
    aes(color = samples),
    method = "lm",
    se = TRUE
  ) +
  facet_grid(
    endpoint ~ samples + strain + s9 + day + mix,
    scales = "free_y"
  ) +
  theme_bw()
p
#72 hr only
plot_dat_72_mix1<- plot_dat %>% filter(endpoint == "72 hr total") %>% filter(mix == 1)
p <- ggplot(
  plot_dat_72_mix1,
  aes(x = dose_value, y = mean_rev)
) +
  geom_hline(
    aes(yintercept = dmso_mean),
    linetype = "dashed",
    color = "blue"
  ) +
  geom_hline(
    aes(yintercept = dmso_2x),
    linetype = "dotted",
    color = "red"
  ) +
  geom_point(
    aes(color = samples),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = mean_rev - se_rev,
      ymax = mean_rev + se_rev,
      color = samples
    ),
    width = 0.05
  ) +
  geom_smooth(
    aes(color = samples),
    method = "lm",
    se = TRUE
  ) +
  facet_grid(strain ~ samples + s9 + day,
    scales = "free_y"
  ) +
  theme_bw()
p ##mix 1

##mix 2
plot_dat_72_mix2<- plot_dat %>% filter(endpoint == "72 hr total") %>% filter(mix == 2)
p <- ggplot(
  plot_dat_72_mix2,
  aes(x = dose_value, y = mean_rev)
) +
  geom_hline(
    aes(yintercept = dmso_mean),
    linetype = "dashed",
    color = "blue"
  ) +
  geom_hline(
    aes(yintercept = dmso_2x),
    linetype = "dotted",
    color = "red"
  ) +
  geom_point(
    aes(color = samples),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = mean_rev - se_rev,
      ymax = mean_rev + se_rev,
      color = samples
    ),
    width = 0.05
  ) +
  geom_smooth(
    aes(color = samples),
    method = "lm",
    se = TRUE
  ) +
  facet_grid(strain ~ samples + s9 + day,
             scales = "free_y"
  ) +
  theme_bw()
p #mix 2


control_samples <- c("dsmo", "nf", "sa", "2aa")

controls <- assay |>
  filter(samples %in% c("DMSO", "NF", "SA", "2AA")) |>
  mutate(
    total = if_else(
      is.na(total),
      hr48 + hr72,
      total
    )
  ) |>
  pivot_longer(
    cols = c(hr48, total),
    names_to = "endpoint",
    values_to = "revertants"
  ) |>
  mutate(
    endpoint = recode(
      endpoint,
      hr48 = "48 hr",
      total = "72 hr total"
    )
  )
ggplot(
  controls,
  aes(x = samples,
      y = revertants,
      fill = samples)
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 2) +
  facet_grid(strain + s9 ~ endpoint) +
  theme_bw() +
  labs(
    x = "Control",
    y = "Revertants per plate",
    title = "Control responses"
  )
#controls mean and SE
control_summary <- controls |>
  group_by(samples, strain, s9, endpoint) |>
  summarise(
    mean_rev = mean(revertants, na.rm = TRUE),
    se_rev = sd(revertants, na.rm = TRUE) /
      sqrt(sum(!is.na(revertants))),
    .groups = "drop"
  )

ggplot(
  control_summary,
  aes(x = samples,
      y = mean_rev,
      fill = samples)
) +
  geom_col() +
  geom_errorbar(
    aes(
      ymin = mean_rev - se_rev,
      ymax = mean_rev + se_rev
    ),
    width = 0.2
  ) +
  facet_grid(strain + s9 ~ endpoint) +
  theme_bw() +
  labs(
    x = "Control",
    y = "Mean revertants ± SE"
  )

message("Done. Outputs written to: ", normalizePath(output_dir))
message("Key files: linear_potency_by_endpoint.csv, compare_48h_vs_72h_slopes.csv, initial_linear_highest_r2.csv")

##archive
ggplot(
  long,
  aes(x = dose_value,
      y = revertants,
      color = endpoint)
) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(strain + s9 ~ samples + mix + day,
             scales = "free_y") +
  theme_bw() +
  labs(
    x = "Dose",
    y = "Revertants per plate"
  )