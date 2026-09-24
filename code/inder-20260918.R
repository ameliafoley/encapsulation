# =============================================================================
# qPCR Cq determination with chipPCR
#
# Method:
#   Raw fluorescence
#       -> Friedman's SuperSmoother
#       -> chipPCR::inder()
#       -> Second Derivative Maximum (SDM) Cq
#
# Input:
#   object "amplification" created by the qPCR import script
#
# Required columns:
#   plate_id
#   fluor
#   well
#   cycle
#   fluorescence
#
# The input fluorescence should NOT already be baseline corrected.
# =============================================================================


# ---- packages ----------------------------------------------------------------

library(tidyverse)
library(chipPCR)


# ---- analysis settings -------------------------------------------------------

SUPSMU_SPAN <- 0.1

INDER_NIP <- 4


# ---- identify individual amplification curves -------------------------------

curve_keys <- c(
  "plate_id",
  "fluor",
  "well"
)


# ---- check input -------------------------------------------------------------

required_cols <- c(
  curve_keys,
  "cycle",
  "fluorescence"
)

missing_cols <- setdiff(
  required_cols,
  names(amplification)
)

if (length(missing_cols) > 0) {
  
  stop(
    "amplification is missing required columns: ",
    paste(
      missing_cols,
      collapse = ", "
    )
  )
}


# Make sure cycle and fluorescence are numeric

amp_qpcr <- amplification %>%
  mutate(
    cycle = as.numeric(cycle),
    fluorescence = as.numeric(fluorescence)
  ) %>%
  arrange(
    plate_id,
    fluor,
    well,
    cycle
  )


# ---- check for duplicate cycles ---------------------------------------------

duplicate_cycles <- amp_qpcr %>%
  count(
    plate_id,
    fluor,
    well,
    cycle,
    name = "n"
  ) %>%
  filter(n > 1)

if (nrow(duplicate_cycles) > 0) {
  
  stop(
    "Duplicate cycle measurements were found within one or more ",
    "plate/fluor/well amplification curves."
  )
}


# =============================================================================
# Function to process one amplification curve
# =============================================================================

process_qpcr_curve <- function(df) {
  
  df <- df %>%
    arrange(cycle)
  
  x <- df$cycle
  y <- df$fluorescence
  
  
  # ---- basic curve QC --------------------------------------------------------
  
  finite <- is.finite(x) & is.finite(y)
  
  if (sum(finite) < 7) {
    
    return(
      list(
        
        curve = df %>%
          mutate(
            fluorescence_smoothed = NA_real_
          ),
        
        result = tibble(
          processing_ok = FALSE,
          processing_message =
            "Fewer than 7 finite cycle/fluorescence observations",
          
          cq_sdm = NA_real_,
          cq_fdm = NA_real_
        )
      )
    )
  }
  
  
  # ---- SuperSmoother ---------------------------------------------------------
  #
  # CPP = FALSE is important:
  #
  # We want smoothing only.
  # We do NOT want CPP background correction or normalization.
  
  smoothed <- tryCatch(
    
    chipPCR::smoother(
      x,
      y,
      
      method = list(
        supsmu = list(
          span = SUPSMU_SPAN
        )
      ),
      
      CPP = FALSE
    ),
    
    error = function(e) e
  )
  
  
  if (inherits(smoothed, "error")) {
    
    return(
      list(
        
        curve = df %>%
          mutate(
            fluorescence_smoothed = NA_real_
          ),
        
        result = tibble(
          processing_ok = FALSE,
          processing_message =
            conditionMessage(smoothed),
          
          cq_sdm = NA_real_,
          cq_fdm = NA_real_
        )
      )
    )
  }
  
  
  # smoother() returns a matrix
  y_smooth <- as.numeric(
    smoothed[, 1]
  )
  
  
  # ---- inder -----------------------------------------------------------------
  
  ind <- tryCatch(
    
    chipPCR::inder(
      x,
      y_smooth,
      Nip = INDER_NIP
    ),
    
    error = function(e) e
  )
  
  
  if (inherits(ind, "error")) {
    
    return(
      list(
        
        curve = df %>%
          mutate(
            fluorescence_smoothed =
              y_smooth
          ),
        
        result = tibble(
          processing_ok = FALSE,
          processing_message =
            conditionMessage(ind),
          
          cq_sdm = NA_real_,
          cq_fdm = NA_real_
        )
      )
    )
  }
  
  
  # ---- extract inder summary -------------------------------------------------
  
  ind_summary <- summary(
    ind,
    print = FALSE
  )
  
  
  # chipPCR inder summary:
  #
  # [1] = FDM
  # [2] = SDM
  
  cq_fdm <- as.numeric(
    ind_summary[1]
  )
  
  cq_sdm <- as.numeric(
    ind_summary[2]
  )
  
  
  # ---- return results --------------------------------------------------------
  
  list(
    
    curve = df %>%
      mutate(
        fluorescence_smoothed =
          y_smooth
      ),
    
    result = tibble(
      
      processing_ok = TRUE,
      
      processing_message =
        NA_character_,
      
      cq_sdm = cq_sdm,
      
      cq_fdm = cq_fdm
    ),
    
    inder = ind
  )
}


# =============================================================================
# Process every amplification curve
# =============================================================================

qpcr_nested <- amp_qpcr %>%
  group_by(
    plate_id,
    fluor,
    well
  ) %>%
  nest() %>%
  ungroup()


qpcr_processed <- qpcr_nested %>%
  mutate(
    analysis = map(
      data,
      process_qpcr_curve
    )
  )


# =============================================================================
# Cycle-level amplification data
# =============================================================================

amplification_processed <- qpcr_processed %>%
  transmute(
    
    plate_id,
    fluor,
    well,
    
    curve = map(
      analysis,
      "curve"
    )
  ) %>%
  unnest(curve)


# =============================================================================
# One-row-per-amplification-curve results
# =============================================================================

cq_results <- qpcr_processed %>%
  transmute(
    
    plate_id,
    fluor,
    well,
    
    result = map(
      analysis,
      "result"
    )
  ) %>%
  unnest(result)


# =============================================================================
# Attach sample metadata
# =============================================================================

metadata_columns <- intersect(
  
  c(
    "plate_id",
    "well",
    "fluor",
    "strain",
    "sample",
    "day",
    "replicate",
    "sediment",
    "treatment",
    "sample_type"
  ),
  
  names(amplification)
)


sample_metadata <- amplification %>%
  select(
    all_of(metadata_columns)
  ) %>%
  distinct()


cq_results <- cq_results %>%
  left_join(
    sample_metadata,
    by = curve_keys
  ) %>%
  relocate(
    any_of(
      c(
        "plate_id",
        "well",
        "fluor",
        "sample",
        "strain",
        "day",
        "replicate",
        "sediment",
        "treatment",
        "sample_type"
      )
    )
  )


# =============================================================================
# Basic QC flags
# =============================================================================

cq_results <- cq_results %>%
  mutate(
    
    # Cqs exactly at the acquisition boundaries deserve inspection.
    cq_boundary =
      cq_sdm <= min(amp_qpcr$cycle) |
      cq_sdm >= max(amp_qpcr$cycle),
    
    cq_missing =
      is.na(cq_sdm),
    
    qc_flag =
      !processing_ok |
      cq_missing |
      cq_boundary
  )

cq_results <- cq_results %>%
  mutate(
    standard =
      str_detect(sample, "^std-"),
    
    standard_number =
      if_else(
        standard,
        as.numeric(str_remove(sample, "^std-")),
        NA_real_
      ),
    
    use_for_standard_curve =
      standard &
      standard_number >= 1
  )

# =============================================================================
# Processing summary
# =============================================================================

processing_summary <- cq_results %>%
  summarise(
    
    total_curves = n(),
    
    successfully_processed =
      sum(processing_ok),
    
    sdm_cq_obtained =
      sum(!is.na(cq_sdm)),
    
    boundary_cq =
      sum(
        cq_boundary,
        na.rm = TRUE
      ),
    
    flagged_curves =
      sum(
        qc_flag,
        na.rm = TRUE
      )
  )

print(processing_summary)


# =============================================================================
# Standard-curve QC
# =============================================================================
#
# This does NOT assume which standards should be retained.
# It simply summarizes the observed SDM Cqs.

standard_qc <- cq_results %>%
  filter(
    str_detect(
      sample,
      "^std-"
    )
  ) %>%
  group_by(
    plate_id,
    fluor,
    sample
  ) %>%
  summarise(
    
    n = n(),
    
    mean_cq = mean(
      cq_sdm,
      na.rm = TRUE
    ),
    
    sd_cq = sd(
      cq_sdm,
      na.rm = TRUE
    ),
    
    min_cq = min(
      cq_sdm,
      na.rm = TRUE
    ),
    
    max_cq = max(
      cq_sdm,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


print(
  standard_qc,
  n = Inf
)


# =============================================================================
# Plot standards
# =============================================================================

standard_plot <- cq_results %>%
  filter(
    str_detect(
      sample,
      "^std-"
    )
  ) %>%
  ggplot(
    aes(
      x = sample,
      y = cq_sdm
    )
  ) +
  
  geom_point(
    position =
      position_jitter(
        width = 0.05
      )
  ) +
  
  facet_grid(
    fluor ~ plate_id
  ) +
  
  theme_minimal() +
  
  labs(
    title =
      "qPCR standards: chipPCR SDM Cq",
    
    subtitle =
      paste0(
        "SuperSmoother span = ",
        SUPSMU_SPAN,
        "; inder Nip = ",
        INDER_NIP
      ),
    
    x = "Standard",
    
    y = "SDM Cq"
  )


standard_plot


# =============================================================================
# Function for visual QC of an individual amplification curve
# =============================================================================

plot_qpcr_curve <- function(
    plate_check,
    fluor_check,
    well_check
) {
  
  d <- amplification_processed %>%
    filter(
      plate_id == plate_check,
      fluor == fluor_check,
      well == well_check
    ) %>%
    arrange(cycle)
  
  
  result <- cq_results %>%
    filter(
      plate_id == plate_check,
      fluor == fluor_check,
      well == well_check
    ) %>%
    slice(1)
  
  
  ggplot(
    d,
    aes(x = cycle)
  ) +
    
    geom_point(
      aes(
        y = fluorescence
      ),
      alpha = 0.45
    ) +
    
    geom_line(
      aes(
        y = fluorescence_smoothed
      ),
      linewidth = 1
    ) +
    
    geom_vline(
      xintercept =
        result$cq_sdm,
      
      linetype = 2
    ) +
    
    theme_minimal() +
    
    labs(
      
      title = paste(
        plate_check,
        fluor_check,
        well_check
      ),
      
      subtitle = paste0(
        "SDM Cq = ",
        round(
          result$cq_sdm,
          2
        )
      ),
      
      x = "Cycle",
      
      y = "Fluorescence"
    )
}


#Example:

plot_qpcr_curve(
  "1",
  "Cy5",
  "A01"
)


# =============================================================================
# Optional: retain complete inder objects for detailed QC
# =============================================================================

inder_results <- qpcr_processed %>%
  transmute(
    
    plate_id,
    fluor,
    well,
    
    inder = map(
      analysis,
      ~ .x$inder %||% NULL
    )
  )


# =============================================================================
# Optional export
# =============================================================================

# write_csv(
#   cq_results,
#   "chipPCR_SDM_Cq_results.csv"
# )

# write_csv(
#   amplification_processed,
#   "chipPCR_smoothed_amplification.csv"
# )

# write_csv(
#   standard_qc,
#   "chipPCR_standard_QC.csv"
# )
unknown_cq <- cq_results %>%
  filter(
    !standard
  ) %>%
  arrange(
    plate_id,
    fluor,
    well
  )
unknown_cq %>%
  select(
    plate_id,
    fluor,
    well,
    sample,
    strain,
    day,
    replicate,
    sediment,
    treatment,
    sample_type,
    cq_sdm,
    cq_fdm,
    processing_ok,
    qc_flag
  ) %>%
  print(n = 50)
ggplot(
  unknown_cq,
  aes(x = cq_sdm)
) +
  geom_histogram(
    binwidth = 1
  ) +
  facet_wrap(
    ~ fluor,
    scales = "free_y"
  ) +
  theme_minimal() +
  labs(
    title = "SDM Cq distribution of unknown samples",
    subtitle =
      "SuperSmoother span = 0.01; inder Nip = 4",
    x = "SDM Cq",
    y = "Number of amplification curves"
  )
ggplot(
  unknown_cq,
  aes(
    x = factor(plate_id),
    y = cq_sdm
  )
) +
  geom_point(
    position = position_jitter(
      width = 0.15
    ),
    alpha = 0.5
  ) +
  facet_wrap(
    ~ fluor
  ) +
  theme_minimal() +
  labs(
    title = "Unknown-sample SDM Cqs across plates",
    x = "Plate",
    y = "SDM Cq"
  )
##inspecting one good and one bad triplicate
inspect_samples <- c(
  "1-F-A",
  "1-S-C"
)

unknown_curve_check <- qpcr_processed %>%
  filter(
    plate_id == "1",
    fluor == "Cy5"
  ) %>%
  filter(
    map_lgl(
      data,
      ~ first(.x$sample) %in% inspect_samples
    )
  )
unknown_curve_plot <- unknown_curve_check %>%
  transmute(
    plate_id,
    fluor,
    well,
    curve = map(
      analysis,
      "curve"
    )
  ) %>%
  unnest(curve)
unknown_curve_plot %>%
  select(
    plate_id,
    fluor,
    well,
    sample,
    cycle,
    fluorescence,
    fluorescence_smoothed
  ) %>%
  head()
ggplot(
  unknown_curve_plot,
  aes(x = cycle)
) +
  geom_point(
    aes(y = fluorescence),
    alpha = 0.35
  ) +
  geom_line(
    aes(y = fluorescence_smoothed),
    linewidth = 0.9
  ) +
  facet_grid(
    sample ~ well,
    scales = "free_y"
  ) +
  theme_minimal() +
  labs(
    title = "Raw and SuperSmoother amplification curves",
    subtitle = "Cy5, plate 1",
    x = "Cycle",
    y = "Fluorescence"
  )
unknown_derivative_plot <- unknown_curve_check %>%
  transmute(
    well,
    sample = map_chr(
      data,
      ~ first(.x$sample)
    ),
    inder = map(
      analysis,
      "inder"
    )
  ) %>%
  mutate(
    derivative = map(
      inder,
      as.data.frame
    )
  ) %>%
  select(
    sample,
    well,
    derivative
  ) %>%
  unnest(derivative)
unknown_derivative_plot %>%
  head()
ggplot(
  unknown_derivative_plot,
  aes(
    x = x,
    y = d2y
  )
) +
  geom_line() +
  geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  facet_grid(
    sample ~ well,
    scales = "free_y"
  ) +
  theme_minimal() +
  labs(
    title = "Second derivative after SuperSmoother",
    subtitle = "span = 0.01; inder Nip = 4",
    x = "Cycle",
    y = "Second derivative"
  )
unknown_qc <- unknown_cq %>%
  mutate(
    derivative_order_ok =
      cq_sdm < cq_fdm,
    
    derivative_gap =
      cq_fdm - cq_sdm
  )
unknown_qc %>%
  count(
    derivative_order_ok
  )
unknown_qc %>%
  group_by(
    derivative_order_ok
  ) %>%
  summarise(
    n = n(),
    mean_cq = mean(cq_sdm, na.rm = TRUE),
    median_cq = median(cq_sdm, na.rm = TRUE),
    .groups = "drop"
  )
unknown_replicate_qc <- unknown_qc %>%
  group_by(
    plate_id,
    fluor,
    sample
  ) %>%
  summarise(
    n_total = n(),
    
    n_derivative_valid =
      sum(
        derivative_order_ok,
        na.rm = TRUE
      ),
    
    mean_cq_all =
      mean(
        cq_sdm,
        na.rm = TRUE
      ),
    
    sd_cq_all =
      sd(
        cq_sdm,
        na.rm = TRUE
      ),
    
    mean_cq_valid =
      mean(
        cq_sdm[
          derivative_order_ok
        ],
        na.rm = TRUE
      ),
    
    sd_cq_valid =
      sd(
        cq_sdm[
          derivative_order_ok
        ],
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )
unknown_replicate_qc %>%
  arrange(
    desc(sd_cq_all)
  ) %>%
  print(n = 50)

##testing baselining first
good_curve <- amplification %>%
  filter(
    plate_id == "1",
    fluor == "Cy5",
    well == "A04"
  ) %>%
  arrange(cycle)
bad_curve <- amplification %>%
  filter(
    plate_id == "1",
    fluor == "Cy5",
    well == "A07"
  ) %>%
  arrange(cycle)
cpp_baseline_test <- function(df) {
  
  cpp <- chipPCR::CPP(
    df$cycle,
    df$fluorescence,
    
    trans = FALSE,
    method.norm = "none"
  )
  
  tibble(
    cycle = df$cycle,
    raw = df$fluorescence,
    cpp_baseline = as.numeric(cpp$y.norm[, 1])
  )
}
tmp <- chipPCR::CPP(
  good_curve$cycle,
  good_curve$fluorescence,
  trans = FALSE,
  smoother = FALSE,
  method.norm = "none"
)

names(tmp)
str(tmp)
head(
  tibble(
    cycle = good_curve$cycle,
    raw = good_curve$fluorescence,
    baseline = as.numeric(tmp$y.norm[, 1])
  ),
  15
)
test_wells <- c(
  "A04", "A05", "A06",
  "A07", "A08", "A09"
)

# =============================================================================
# Amplification-curve QC metrics
# =============================================================================

curve_qc <- amplification_processed %>%
  group_by(
    plate_id,
    fluor,
    well
  ) %>%
  summarise(
    
    # Total fluorescence change
    fluorescence_range =
      max(fluorescence_smoothed, na.rm = TRUE) -
      min(fluorescence_smoothed, na.rm = TRUE),
    
    # Change from beginning to end
    fluorescence_change =
      last(fluorescence_smoothed) -
      first(fluorescence_smoothed),
    
    # Early fluorescence variability
    early_sd =
      sd(
        fluorescence_smoothed[
          cycle <= 10
        ],
        na.rm = TRUE
      ),
    
    # Late minus early fluorescence
    early_mean =
      mean(
        fluorescence_smoothed[
          cycle <= 10
        ],
        na.rm = TRUE
      ),
    
    late_mean =
      mean(
        fluorescence_smoothed[
          cycle >= 35
        ],
        na.rm = TRUE
      ),
    
    signal_increase =
      late_mean - early_mean,
    
    .groups = "drop"
  )
derivative_qc <- qpcr_processed %>%
  transmute(
    plate_id,
    fluor,
    well,
    
    inder = map(
      analysis,
      ~ .x$inder %||% NULL
    )
  ) %>%
  mutate(
    
    metrics = map(
      inder,
      ~ {
        
        if (is.null(.x)) {
          
          return(
            tibble(
              max_d1 = NA_real_,
              max_d2 = NA_real_,
              min_d2 = NA_real_
            )
          )
        }
        
        d <- as.data.frame(.x)
        
        tibble(
          max_d1 =
            max(d$d1y, na.rm = TRUE),
          
          max_d2 =
            max(d$d2y, na.rm = TRUE),
          
          min_d2 =
            min(d$d2y, na.rm = TRUE)
        )
      }
    )
  ) %>%
  select(
    plate_id,
    fluor,
    well,
    metrics
  ) %>%
  unnest(metrics)
cq_qc <- cq_results %>%
  left_join(
    curve_qc,
    by = c(
      "plate_id",
      "fluor",
      "well"
    )
  ) %>%
  left_join(
    derivative_qc,
    by = c(
      "plate_id",
      "fluor",
      "well"
    )
  ) %>%
  mutate(
    
    derivative_gap =
      cq_fdm - cq_sdm,
    
    derivative_order_ok =
      cq_sdm < cq_fdm,
    
    # Signal relative to early-cycle variability
    signal_to_early_noise =
      signal_increase /
      early_sd
  )
cq_qc %>%
  filter(
    plate_id == "1",
    fluor == "Cy5",
    sample %in% c(
      "1-F-A",
      "1-S-C"
    )
  ) %>%
  select(
    sample,
    well,
    cq_sdm,
    cq_fdm,
    derivative_gap,
    derivative_order_ok,
    signal_increase,
    early_sd,
    signal_to_early_noise,
    max_d1,
    max_d2
  )
cq_qc <- cq_qc %>%
  mutate(
    curve_class = case_when(
      
      str_detect(
        sample,
        "^std-"
      ) ~ "standard",
      
      TRUE ~ "unknown"
    )
  )
cq_qc %>%
  group_by(
    fluor,
    curve_class
  ) %>%
  summarise(
    n = n(),
    
    median_signal =
      median(
        signal_increase,
        na.rm = TRUE
      ),
    
    median_snr =
      median(
        signal_to_early_noise,
        na.rm = TRUE
      ),
    
    median_max_d1 =
      median(
        max_d1,
        na.rm = TRUE
      ),
    
    median_max_d2 =
      median(
        max_d2,
        na.rm = TRUE
      ),
    
    fraction_derivative_order_ok =
      mean(
        derivative_order_ok,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )
ggplot(
  cq_qc,
  aes(
    x = signal_increase,
    y = max_d1
  )
) +
  geom_point(
    aes(
      shape = curve_class
    ),
    alpha = 0.6
  ) +
  facet_wrap(
    ~ fluor,
    scales = "free"
  ) +
  theme_minimal() +
  labs(
    title = "Amplification signal versus derivative strength",
    x = "Late - early fluorescence",
    y = "Maximum first derivative"
  )
ggplot(
  cq_qc,
  aes(
    x = signal_to_early_noise,
    y = cq_sdm
  )
) +
  geom_point(
    aes(
      shape = curve_class
    ),
    alpha = 0.6
  ) +
  facet_wrap(
    ~ fluor,
    scales = "free_x"
  ) +
  theme_minimal() +
  labs(
    title = "SDM Cq versus amplification signal-to-noise",
    x = "Signal increase / early-cycle SD",
    y = "SDM Cq"
  )
