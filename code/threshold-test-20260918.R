test_thcyc_curve <- function(df) {
  
  df <- df %>%
    arrange(cycle)
  
  cpp <- chipPCR::CPP(
    df$cycle,
    df$fluorescence,
    
    # baseline subtraction, but no linear trend extrapolation
    trans = FALSE,
    
    # modest smoothing
    smoother = TRUE,
    method = "supsmu",
    
    # no scaling yet
    method.norm = "none",
    
    bg.outliers = TRUE
  )
  
  tibble(
    cycle = df$cycle,
    raw = df$fluorescence,
    processed = as.numeric(cpp$y.norm),
    background =
      df$cycle %in% cpp$BG
  )
}
a04 <- amplification %>%
  filter(
    plate_id == "1",
    fluor == "Cy5",
    well == "A04"
  )

a07 <- amplification %>%
  filter(
    plate_id == "1",
    fluor == "Cy5",
    well == "A07"
  )

a04_cpp <- test_thcyc_curve(a04)
a07_cpp <- test_thcyc_curve(a07)
bind_rows(
  A04 = a04_cpp,
  A07 = a07_cpp,
  .id = "well"
) %>%
  ggplot(
    aes(cycle, processed)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  geom_line() +
  facet_wrap(
    ~well,
    scales = "free_y"
  ) +
  theme_minimal() +
  labs(
    title = "CPP-preprocessed curves for threshold Cq",
    subtitle = "trans = FALSE; method.norm = none",
    x = "Cycle",
    y = "Baseline-corrected fluorescence"
  )
##derive candidate thresholds from standards
std_cpp <- amplification %>%
  filter(
    plate_id == "1",
    fluor == "Cy5",
    str_detect(sample, "^std-")
  ) %>%
  group_by(
    sample,
    well
  ) %>%
  nest() %>%
  mutate(
    processed = map(
      data,
      test_thcyc_curve
    )
  ) %>%
  select(
    sample,
    well,
    processed
  ) %>%
  unnest(processed)
ggplot(
  std_cpp,
  aes(
    x = cycle,
    y = processed,
    group = well,
    color = sample
  )
) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Plate 1 Cy5 standards after CPP baselining",
    subtitle = "No normalization; trans = FALSE",
    x = "Cycle",
    y = "Baseline-corrected fluorescence"
  )
## try early baseline range
test_thcyc_curve_manual <- function(df) {
  
  df <- df %>%
    arrange(cycle)
  
  cpp <- chipPCR::CPP(
    df$cycle,
    df$fluorescence,
    
    trans = FALSE,
    
    smoother = TRUE,
    method = "supsmu",
    
    method.norm = "none",
    
    bg.outliers = TRUE,
    
    manual = TRUE,
    bg.range = c(2, 5)
  )
  
  tibble(
    cycle = df$cycle,
    raw = df$fluorescence,
    processed = as.numeric(cpp$y.norm)
  )
}
std_cpp_manual <- amplification %>%
  filter(
    plate_id == "1",
    fluor == "Cy5",
    str_detect(sample, "^std-")
  ) %>%
  group_by(
    sample,
    well
  ) %>%
  nest() %>%
  mutate(
    processed = map(
      data,
      test_thcyc_curve_manual
    )
  ) %>%
  select(
    sample,
    well,
    processed
  ) %>%
  unnest(processed)
ggplot(
  std_cpp_manual,
  aes(
    cycle,
    processed,
    color = sample,
    group = well
  )
) +
  geom_line() +
  geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  theme_minimal() +
  labs(
    title = "Plate 1 Cy5 standards",
    subtitle = "CPP manual background cycles 2–5",
    x = "Cycle",
    y = "Baseline-corrected fluorescence"
  )
thresholds <- c(
  20,
  30,
  40,
  50,
  60,
  75,
  100
)
threshold_screen <- std_cpp_manual %>%
  group_by(
    sample,
    well
  ) %>%
  nest() %>%
  crossing(
    threshold = thresholds
  ) %>%
  mutate(
    cq = map2_dbl(
      data,
      threshold,
      ~ {
        result <- tryCatch(
          chipPCR::th.cyc(
            .x$cycle,
            .x$processed,
            r = .y,
            auto = FALSE, ##switched to false
            linear = TRUE
          ),
          error = function(e) NULL
        )
        
        if (is.null(result)) {
          return(NA_real_)
        }
        
        as.numeric(result[1])
      }
    )
  )
threshold_summary <- threshold_screen %>%
  group_by(
    threshold,
    sample
  ) %>%
  summarise(
    mean_cq = mean(
      cq,
      na.rm = TRUE
    ),
    
    sd_cq = sd(
      cq,
      na.rm = TRUE
    ),
    
    n_cq = sum(
      !is.na(cq)
    ),
    
    .groups = "drop"
  )

threshold_summary %>%
  print(n = Inf)
ggplot(
  threshold_summary,
  aes(
    x = threshold,
    y = mean_cq,
    group = sample
  )
) +
  geom_line() +
  geom_point() +
  facet_wrap(
    ~ sample,
    scales = "free_y"
  ) +
  theme_minimal() +
  labs(
    title = "Sensitivity of th.cyc Cq to fluorescence threshold",
    subtitle = "Plate 1 Cy5; CPP background cycles 2–5",
    x = "Threshold (baseline-corrected RFU)",
    y = "Mean Cq"
  )
unknown_cpp_test <- amplification %>%
  filter(
    plate_id == "1",
    fluor == "Cy5",
    well %in% c(
      "A04", "A05", "A06",
      "A07", "A08", "A09"
    )
  ) %>%
  group_by(
    sample,
    well
  ) %>%
  nest() %>%
  mutate(
    processed = map(
      data,
      test_thcyc_curve_manual
    )
  ) %>%
  select(
    sample,
    well,
    processed
  ) %>%
  unnest(processed)
unknown_threshold_screen <- unknown_cpp_test %>%
  group_by(
    sample,
    well
  ) %>%
  nest() %>%
  crossing(
    threshold = thresholds
  ) %>%
  mutate(
    cq = map2_dbl(
      data,
      threshold,
      ~ {
        result <- tryCatch(
          chipPCR::th.cyc(
            .x$cycle,
            .x$processed,
            r = .y,
            auto = TRUE,
            linear = TRUE
          ),
          error = function(e) NULL
        )
        
        if (is.null(result)) {
          return(NA_real_)
        }
        
        as.numeric(result[1])
      }
    )
  )
unknown_threshold_screen %>%
  select(
    sample,
    well,
    threshold,
    cq
  ) %>%
  arrange(
    threshold,
    sample,
    well
  ) %>%
  print(n = Inf)

##shorten background to get std-6 positive
std6_bg_test <- function(bg_range) {
  
  cpp <- chipPCR::CPP(
    std6_A01$cycle,
    std6_A01$fluorescence,
    
    smoother = FALSE,
    trans = FALSE,
    
    method.norm = "none",
    
    manual = TRUE,
    bg.range = bg_range
  )
  
  tibble(
    cycle = std6_A01$cycle,
    fluorescence = std6_A01$fluorescence,
    corrected = as.numeric(cpp$y.norm)
  )
}
bg12 <- std6_bg_test(c(1, 2))
bg13 <- std6_bg_test(c(1, 3))
bg25 <- std6_bg_test(c(2, 5))

bind_rows(
  `1-2` = bg12,
  `1-3` = bg13,
  `2-5` = bg25,
  .id = "background_range"
) %>%
  ggplot(
    aes(
      x = cycle,
      y = corrected
    )
  ) +
  geom_line() +
  facet_wrap(
    ~ background_range,
    scales = "free_y"
  ) +
  theme_minimal() +
  labs(
    title = "Effect of background range on std-6 A01",
    x = "Cycle",
    y = "Baseline-corrected fluorescence"
  )
## check against all standards
bg_ranges <- list(
  `1-2` = c(1, 2),
  `1-3` = c(1, 3),
  `1-4` = c(1, 4),
  `2-5` = c(2, 5)
)
process_standard_bg <- function(df, bg_range) {
  
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
    bg.range = bg_range
  )
  
  tibble(
    cycle = df$cycle,
    
    raw = df$fluorescence,
    
    processed = as.numeric(
      cpp$y.norm[, 1]
    )
  )
}
## run all four ranges
standard_bg_screen <- amplification %>%
  filter(
    plate_id == "1",
    fluor == "Cy5",
    str_detect(sample, "^std-")
  ) %>%
  group_by(
    sample,
    well
  ) %>%
  nest() %>%
  crossing(
    bg_range_name = names(bg_ranges)
  ) %>%
  mutate(
    bg_range = map(
      bg_range_name,
      ~ bg_ranges[[.x]]
    ),
    
    processed = map2(
      data,
      bg_range,
      process_standard_bg
    )
  ) %>%
  select(
    sample,
    well,
    bg_range_name,
    processed
  ) %>%
  unnest(processed)
ggplot(
  standard_bg_screen,
  aes(
    x = cycle,
    y = processed,
    color = sample,
    group = interaction(sample, well)
  )
) +
  geom_line() +
  geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  facet_wrap(
    ~ bg_range_name,
    ncol = 2
  ) +
  theme_minimal() +
  labs(
    title = "Effect of background range on standard curves",
    subtitle = "CPP + SuperSmoother; trans = FALSE; no normalization",
    x = "Cycle",
    y = "Baseline-corrected fluorescence",
    color = "Standard"
  )
thresholds <- c(
  20,
  30,
  40,
  50,
  60
)
standard_bg_threshold <- standard_bg_screen %>%
  group_by(
    bg_range_name,
    sample,
    well
  ) %>%
  nest() %>%
  crossing(
    threshold = thresholds
  ) %>%
  mutate(
    cq = map2_dbl(
      data,
      threshold,
      ~ {
        result <- tryCatch(
          chipPCR::th.cyc(
            .x$cycle,
            .x$processed,
            r = .y,
            auto = TRUE,
            linear = TRUE
          ),
          error = function(e) NULL
        )
        
        if (is.null(result)) {
          return(NA_real_)
        }
        
        as.numeric(result[1])
      }
    )
  )
standard_bg_threshold <- standard_bg_screen %>%
  group_by(
    bg_range_name,
    sample,
    well
  ) %>%
  nest() %>%
  crossing(
    threshold = thresholds
  ) %>%
  mutate(
    cq = map2_dbl(
      data,
      threshold,
      ~ {
        result <- tryCatch(
          chipPCR::th.cyc(
            .x$cycle,
            .x$processed,
            r = .y,
            auto = FALSE,
            linear = TRUE
          ),
          error = function(e) NULL
        )
        
        if (is.null(result)) {
          return(NA_real_)
        }
        
        as.numeric(result[1])
      }
    )
  )
standard_bg_threshold_summary <- standard_bg_threshold %>%
  group_by(
    bg_range_name,
    threshold,
    sample
  ) %>%
  summarise(
    n = sum(!is.na(cq)),
    
    mean_cq = mean(
      cq,
      na.rm = TRUE
    ),
    
    sd_cq = sd(
      cq,
      na.rm = TRUE
    ),
    
    min_cq = min(
      cq,
      na.rm = TRUE
    ),
    
    max_cq = max(
      cq,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )
ggplot(
  standard_bg_threshold_summary,
  aes(
    x = threshold,
    y = mean_cq,
    group = sample
  )
) +
  geom_line() +
  geom_point() +
  facet_grid(
    bg_range_name ~ sample,
    scales = "free_y"
  ) +
  theme_minimal() +
  labs(
    title = "Cq sensitivity to background range and threshold",
    x = "Threshold RFU",
    y = "Mean th.cyc Cq"
  )
##now test background and threshold against my problem samples
test_unknown_thcyc <- amplification %>%
  filter(
    plate_id == "1",
    fluor == "Cy5",
    well %in% c(
      "A04", "A05", "A06",
      "A07", "A08", "A09"
    )
  ) %>%
  group_by(
    sample,
    well
  ) %>%
  nest() %>%
  mutate(
    processed = map(
      data,
      ~ {
        
        cpp <- chipPCR::CPP(
          .x$cycle,
          .x$fluorescence,
          
          smoother = TRUE,
          method = "supsmu",
          
          trans = FALSE,
          method.norm = "none",
          
          bg.outliers = TRUE,
          
          manual = TRUE,
          bg.range = c(1, 3)
        )
        
        tibble(
          cycle = .x$cycle,
          fluorescence =
            as.numeric(cpp$y.norm[, 1])
        )
      }
    )
  )
thresholds <- c(
  20,
  30,
  40,
  50,
  60
)

test_unknown_thcyc <- test_unknown_thcyc %>%
  crossing(
    threshold = thresholds
  ) %>%
  mutate(
    cq = map2_dbl(
      processed,
      threshold,
      ~ {
        
        result <- tryCatch(
          
          chipPCR::th.cyc(
            .x$cycle,
            .x$fluorescence,
            
            r = .y,
            
            auto = FALSE,
            linear = TRUE
          ),
          
          error = function(e) NULL
        )
        
        if (is.null(result)) {
          return(NA_real_)
        }
        
        as.numeric(result[1])
      }
    )
  )
test_unknown_thcyc %>%
  select(
    sample,
    well,
    threshold,
    cq
  ) %>%
  arrange(
    threshold,
    sample,
    well
  ) %>%
  print(n = Inf)
test_unknown_thcyc <- test_unknown_thcyc %>%
  mutate(
    max_signal = map_dbl(
      processed,
      ~ max(
        .x$fluorescence,
        na.rm = TRUE
      )
    ),
    
    crosses_threshold =
      max_signal > threshold
  )
test_unknown_thcyc %>%
  select(
    sample,
    well,
    threshold,
    max_signal,
    crosses_threshold,
    cq
  ) %>%
  arrange(
    threshold,
    sample,
    well
  ) %>%
  print(n = Inf)
amplification %>%
  distinct(
    sample,
    sample_type
  ) %>%
  arrange(
    sample_type,
    sample
  ) %>%
  print(n = Inf)
