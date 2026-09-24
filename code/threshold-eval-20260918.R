## evaluate thresholds based on standards and NTC

BG_RANGE <- c(1, 3)

preprocess_thcyc <- function(df) {
  
  df <- df %>%
    arrange(cycle)
  
  cpp <- chipPCR::CPP(
    df$cycle,
    df$fluorescence,
    
    smoother = TRUE,
    method = "supsmu",
    
    trans = FALSE,
    method.norm = "none",
    
    bg.outliers = TRUE,
    
    manual = TRUE,
    bg.range = BG_RANGE
  )
  
  tibble(
    cycle = df$cycle,
    fluorescence_processed =
      as.numeric(cpp$y.norm[, 1])
  )
}
BG_RANGE <- c(1, 3)
threshold_validation_curves <- amplification %>%
  filter(
    sample == "NTC" |
      str_detect(sample, "^std-")
  ) %>%
  group_by(
    plate_id,
    fluor,
    well,
    sample
  ) %>%
  nest() %>%
  mutate(
    processed = map(
      data,
      preprocess_thcyc
    ),
    
    max_signal = map_dbl(
      processed,
      ~ max(
        .x$fluorescence_processed,
        na.rm = TRUE
      )
    )
  ) %>%
  ungroup()
ggplot(
  threshold_validation_curves,
  aes(
    x = sample,
    y = max_signal
  )
) +
  geom_point(
    position =
      position_jitter(width = 0.1)
  ) +
  facet_wrap(
    ~ fluor,
    scales = "free_y"
  ) +
  theme_minimal() +
  labs(
    title = "Maximum baseline-corrected signal",
    subtitle = "Standards and NTCs; background cycles 1-3",
    x = "Sample",
    y = "Maximum baseline-corrected fluorescence"
  )
ntc_summary <- threshold_validation_curves %>%
  filter(
    sample == "NTC"
  ) %>%
  group_by(fluor) %>%
  summarise(
    n = n(),
    
    min_signal =
      min(max_signal, na.rm = TRUE),
    
    median_signal =
      median(max_signal, na.rm = TRUE),
    
    mean_signal =
      mean(max_signal, na.rm = TRUE),
    
    max_signal =
      max(max_signal, na.rm = TRUE),
    
    q95_signal =
      quantile(
        max_signal,
        0.95,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

ntc_summary
positive_standard_summary <-
  threshold_validation_curves %>%
  filter(
    sample %in% paste0(
      "std-",
      1:6
    )
  ) %>%
  group_by(
    fluor,
    sample
  ) %>%
  summarise(
    n = n(),
    
    min_signal =
      min(max_signal, na.rm = TRUE),
    
    median_signal =
      median(max_signal, na.rm = TRUE),
    
    max_signal =
      max(max_signal, na.rm = TRUE),
    
    .groups = "drop"
  )

positive_standard_summary
ntc_by_plate <- threshold_validation_curves %>%
  filter(
    sample == "NTC"
  ) %>%
  group_by(
    plate_id,
    fluor
  ) %>%
  summarise(
    n_ntc = n(),
    
    ntc_min =
      min(max_signal, na.rm = TRUE),
    
    ntc_median =
      median(max_signal, na.rm = TRUE),
    
    ntc_max =
      max(max_signal, na.rm = TRUE),
    
    .groups = "drop"
  )
std1_by_plate <- threshold_validation_curves %>%
  filter(
    sample == "std-1"
  ) %>%
  group_by(
    plate_id,
    fluor
  ) %>%
  summarise(
    n_std1 = n(),
    
    std1_min =
      min(max_signal, na.rm = TRUE),
    
    std1_median =
      median(max_signal, na.rm = TRUE),
    
    std1_max =
      max(max_signal, na.rm = TRUE),
    
    .groups = "drop"
  )
threshold_separation <- ntc_by_plate %>%
  left_join(
    std1_by_plate,
    by = c(
      "plate_id",
      "fluor"
    )
  ) %>%
  mutate(
    
    # Positive value means there is a clean RFU interval
    # between the highest NTC and lowest std-1.
    separation =
      std1_min - ntc_max
  )

threshold_separation %>%
  arrange(
    fluor,
    plate_id
  ) %>%
  print(n = Inf)
ggplot(
  threshold_validation_curves,
  aes(
    x = sample,
    y = max_signal
  )
) +
  geom_point(
    position =
      position_jitter(width = 0.08)
  ) +
  facet_grid(
    fluor ~ plate_id,
    scales = "free_y"
  ) +
  theme_minimal() +
  labs(
    title =
      "Maximum baseline-corrected signal by plate",
    
    subtitle =
      "CPP background cycles 1-3",
    
    x = "Sample",
    
    y =
      "Maximum baseline-corrected fluorescence"
  )

threshold_validation_curves <- threshold_validation_curves %>%
  mutate(
    exclude_control = case_when(
      plate_id == "1" &
        sample == "NTC" &
        well == "H03" ~ TRUE,
      
      TRUE ~ FALSE
    ),
    
    exclusion_reason = case_when(
      exclude_control ~
        "Known pipetting error: standard added to NTC well",
      
      TRUE ~ NA_character_
    )
  )
##compare NTCs to lowest standard on each plate
threshold_windows <- threshold_validation_curves %>%
  filter(!exclude_control) %>%
  mutate(
    lowest_standard = case_when(
      plate_id %in% c("1", "2") ~ "std-2",
      TRUE ~ "std-0"
    )
  ) %>%
  group_by(
    plate_id,
    fluor
  ) %>%
  summarise(
    
    # Highest endpoint fluorescence among valid NTCs
    ntc_max = max(
      max_signal[sample == "NTC"],
      na.rm = TRUE
    ),
    
    # Identity of lowest standard on this plate
    lowest_std = first(lowest_standard),
    
    # Lowest endpoint fluorescence among triplicates
    # of the lowest standard
    lowest_std_min = min(
      max_signal[
        sample == first(lowest_standard)
      ],
      na.rm = TRUE
    ),
    
    # Maximum endpoint fluorescence observed among
    # all standards on this plate/channel
    standard_max = max(
      max_signal[
        str_detect(sample, "^std-")
      ],
      na.rm = TRUE
    ),
    
    # Candidate threshold: 10% of maximum standard fluorescence
    ten_percent_max = 0.10 * standard_max,
    
    # Available separation between valid NTCs
    # and the lowest standard
    separation =
      lowest_std_min - ntc_max,
    
    .groups = "drop"
  )
threshold_windows %>%
  arrange(
    plate_id,
    fluor
  ) %>%
  print(n = Inf)
threshold_windows <- threshold_windows %>%
  mutate(
    ten_percent_above_ntc =
      ten_percent_max > ntc_max,
    
    ten_percent_below_lowest_std =
      ten_percent_max < lowest_std_min,
    
    ten_percent_in_window =
      ten_percent_above_ntc &
      ten_percent_below_lowest_std
  )
# =============================================================================
# Final threshold r for each fluorophore
# Mean of plate-specific 10%-of-maximum values
# =============================================================================

r_values <- threshold_windows %>%
  group_by(fluor) %>%
  summarise(
    n_plates = n(),
    
    r = mean(
      ten_percent_max,
      na.rm = TRUE
    ),
    
    r_sd = sd(
      ten_percent_max,
      na.rm = TRUE
    ),
    
    r_min = min(
      ten_percent_max,
      na.rm = TRUE
    ),
    
    r_max = max(
      ten_percent_max,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

r_values
threshold_windows <- threshold_windows %>%
  left_join(
    r_values %>%
      select(
        fluor,
        r
      ),
    by = "fluor"
  )
threshold_windows <- threshold_windows %>%
  mutate(
    r_above_ntc =
      r > ntc_max,
    
    r_below_lowest_std =
      r < lowest_std_min,
    
    r_in_window =
      r_above_ntc &
      r_below_lowest_std
  )
threshold_windows %>%
  select(
    plate_id,
    fluor,
    ntc_max,
    lowest_std,
    lowest_std_min,
    ten_percent_max,
    r,
    r_above_ntc,
    r_below_lowest_std,
    r_in_window
  ) %>%
  arrange(
    fluor,
    plate_id
  ) %>%
  print(n = Inf)
R_BY_FLUOR <- r_values %>%
  select(
    fluor,
    r
  ) %>%
  deframe()

R_BY_FLUOR

