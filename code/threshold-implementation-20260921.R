##processing unknowns with new baseline and threshold methods 9/21/26

# =============================================================================
# Strain color palette
# MoMA "Lupi"
# =============================================================================

strain_colors <- c(
  # r-strategists
  "a.venet"    = "#D85A44FF",  # vermilion
  "p.resin"    = "#DAA5ACFF",  # coral
  "p.putida"   = "#FFB651FF",  # gold
  
  # K-strategists
  "a.faecalis" = "#B6E7E0FF",  # light cyan
  "sphingo.sp" = "#98A54FFF",  # olive/teal
  "n.penta"    = "#2E92A2FF"   # deep blue
)

process_thcyc_curve <- function(
    df,
    threshold,
    bg_range = c(1, 3)
) {
  
  df <- df %>%
    arrange(cycle)
  
  # ---------------------------------------------------------------------------
  # CPP preprocessing
  # ---------------------------------------------------------------------------
  
  cpp <- tryCatch(
    
    chipPCR::CPP(
      df$cycle,
      df$fluorescence,
      
      # SuperSmoother
      smoother = TRUE,
      method = "supsmu",
      
      # No linear baseline-trend extrapolation
      trans = FALSE,
      
      # Keep fluorescence on its RFU scale
      method.norm = "none",
      
      bg.outliers = TRUE,
      
      # Fixed background region
      manual = TRUE,
      bg.range = bg_range
    ),
    
    error = function(e) e
  )
  
  
  # ---------------------------------------------------------------------------
  # CPP failure
  # ---------------------------------------------------------------------------
  
  if (inherits(cpp, "error")) {
    
    return(
      list(
        
        curve = df %>%
          mutate(
            fluorescence_processed = NA_real_
          ),
        
        result = tibble(
          processing_ok = FALSE,
          processing_message = conditionMessage(cpp),
          
          threshold = threshold,
          max_signal = NA_real_,
          crosses_threshold = FALSE,
          
          cq = NA_real_
        )
      )
    )
  }
  
  
  y_processed <- as.numeric(
    cpp$y.norm[, 1]
  )
  
  max_signal <- max(
    y_processed,
    na.rm = TRUE
  )
  
  
  # ---------------------------------------------------------------------------
  # Does the curve reach the threshold?
  # ---------------------------------------------------------------------------
  
  crosses_threshold <-
    max_signal > threshold
  
  
  # Do NOT ask th.cyc to calculate a Cq if the
  # fluorescence never reaches the threshold.
  
  if (!crosses_threshold) {
    
    return(
      list(
        
        curve = df %>%
          mutate(
            fluorescence_processed =
              y_processed
          ),
        
        result = tibble(
          processing_ok = TRUE,
          processing_message = NA_character_,
          
          threshold = threshold,
          max_signal = max_signal,
          crosses_threshold = FALSE,
          
          cq = NA_real_
        )
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # th.cyc
  # ---------------------------------------------------------------------------
  
  tc <- tryCatch(
    
    chipPCR::th.cyc(
      df$cycle,
      y_processed,
      
      r = threshold,
      
      # Use our explicitly supplied threshold
      auto = FALSE,
      
      # Local linear regression
      linear = TRUE
    ),
    
    error = function(e) e
  )
  
  
  # ---------------------------------------------------------------------------
  # th.cyc failure
  # ---------------------------------------------------------------------------
  
  if (inherits(tc, "error")) {
    
    return(
      list(
        
        curve = df %>%
          mutate(
            fluorescence_processed =
              y_processed
          ),
        
        result = tibble(
          processing_ok = FALSE,
          processing_message = conditionMessage(tc),
          
          threshold = threshold,
          max_signal = max_signal,
          crosses_threshold = TRUE,
          
          cq = NA_real_
        )
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Successful Cq
  # ---------------------------------------------------------------------------
  
  cq <- as.numeric(
    tc[1]
  )
  
  
  list(
    
    curve = df %>%
      mutate(
        fluorescence_processed =
          y_processed
      ),
    
    result = tibble(
      processing_ok = TRUE,
      processing_message = NA_character_,
      
      threshold = threshold,
      max_signal = max_signal,
      crosses_threshold = TRUE,
      
      cq = cq
    )
  )
}
unknown_nested <- amplification %>%
  filter(
    sample != "NTC",
    !str_detect(sample, "^std-")
  ) %>%
  group_by(
    plate_id,
    fluor,
    well
  ) %>%
  nest() %>%
  ungroup()
unknown_nested <- unknown_nested %>%
  left_join(
    r_values %>%
      select(
        fluor,
        r
      ),
    by = "fluor"
  )
unknown_nested %>%
  count(
    fluor,
    is.na(r)
  )
##process uknowns
unknown_processed <- unknown_nested %>%
  mutate(
    analysis = map2(
      data,
      r,
      ~ process_thcyc_curve(
        df = .x,
        threshold = .y,
        bg_range = c(1, 3)
      )
    )
  )
## extract Cq table
unknown_cq <- unknown_processed %>%
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
## restore sample data
unknown_metadata <- amplification %>%
  filter(
    sample != "NTC",
    !str_detect(sample, "^std-")
  ) %>%
  select(
    plate_id,
    fluor,
    well,
    any_of(
      c(
        "sample",
        "strain",
        "day",
        "replicate",
        "sediment",
        "treatment",
        "sample_type"
      )
    )
  ) %>%
  distinct()
# join Cq and metadata
unknown_cq <- unknown_cq %>%
  left_join(
    unknown_metadata,
    by = c(
      "plate_id",
      "fluor",
      "well"
    )
  ) %>%
  relocate(
    plate_id,
    fluor,
    well,
    sample,
    any_of(
      c(
        "strain",
        "day",
        "replicate",
        "sediment",
        "treatment",
        "sample_type"
      )
    ),
    threshold,
    max_signal,
    crosses_threshold,
    cq
  )
unknown_cq %>%
  arrange(
    plate_id,
    fluor,
    well
  ) %>%
  print(n = 100)
# give every well a status
unknown_cq <- unknown_cq %>%
  mutate(
    cq_status = case_when(
      
      !processing_ok ~
        "processing_failed",
      
      !crosses_threshold ~
        "below_threshold",
      
      is.na(cq) ~
        "Cq_failed",
      
      TRUE ~
        "Cq_detected"
    )
  )
unknown_cq %>%
  count(
    fluor,
    cq_status
  )
## check technical triplicates
unknown_triplicate_summary <- unknown_cq %>%
  group_by(
    plate_id,
    fluor,
    sample
  ) %>%
  summarise(
    n_wells = n(),
    
    n_detected =
      sum(
        cq_status == "Cq_detected"
      ),
    
    mean_cq =
      mean(
        cq[
          cq_status == "Cq_detected"
        ],
        na.rm = TRUE
      ),
    
    sd_cq =
      sd(
        cq[
          cq_status == "Cq_detected"
        ],
        na.rm = TRUE
      ),
    
    min_cq =
      suppressWarnings(
        min(
          cq[
            cq_status == "Cq_detected"
          ],
          na.rm = TRUE
        )
      ),
    
    max_cq =
      suppressWarnings(
        max(
          cq[
            cq_status == "Cq_detected"
          ],
          na.rm = TRUE
        )
      ),
    
    .groups = "drop"
  ) %>%
  mutate(
    mean_cq =
      if_else(
        n_detected == 0,
        NA_real_,
        mean_cq
      ),
    
    sd_cq =
      if_else(
        n_detected < 2,
        NA_real_,
        sd_cq
      ),
    
    min_cq =
      if_else(
        n_detected == 0,
        NA_real_,
        min_cq
      ),
    
    max_cq =
      if_else(
        n_detected == 0,
        NA_real_,
        max_cq
      )
  )
unknown_triplicate_summary %>%
  arrange(
    plate_id,
    fluor,
    sample
  ) %>%
  print(n = 100)
## revisit example wells
unknown_cq %>%
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
    threshold,
    max_signal,
    crosses_threshold,
    cq,
    cq_status
  )
## process standard curves
standard_nested <- amplification %>%
  filter(
    str_detect(sample, "^std-")
  ) %>%
  group_by(
    plate_id,
    fluor,
    well
  ) %>%
  nest() %>%
  ungroup()
standard_nested <- standard_nested %>%
  left_join(
    r_values %>%
      select(
        fluor,
        r
      ),
    by = "fluor"
  )
standard_nested %>%
  count(
    fluor,
    is.na(r)
  )
standard_processed <- standard_nested %>%
  mutate(
    analysis = map2(
      data,
      r,
      ~ process_thcyc_curve(
        df = .x,
        threshold = .y,
        bg_range = c(1, 3)
      )
    )
  )
standard_cq <- standard_processed %>%
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
standard_metadata <- amplification %>%
  filter(
    str_detect(sample, "^std-")
  ) %>%
  select(
    plate_id,
    fluor,
    well,
    sample
  ) %>%
  distinct()
standard_cq <- standard_cq %>%
  left_join(
    standard_metadata,
    by = c(
      "plate_id",
      "fluor",
      "well"
    )
  ) %>%
  relocate(
    plate_id,
    fluor,
    well,
    sample,
    threshold,
    max_signal,
    crosses_threshold,
    cq
  )
standard_cq %>%
  arrange(
    plate_id,
    fluor,
    sample,
    well
  ) %>%
  print(n = Inf)
standard_cq <- standard_cq %>%
  mutate(
    log10_quantity =
      as.numeric(
        str_remove(
          sample,
          "^std-"
        )
      ),
    
    starting_quantity =
      10^log10_quantity
  )
standard_cq_summary <- standard_cq %>%
  group_by(
    plate_id,
    fluor,
    sample,
    log10_quantity
  ) %>%
  summarise(
    n_wells = n(),
    
    n_detected =
      sum(crosses_threshold),
    
    mean_cq =
      mean(
        cq,
        na.rm = TRUE
      ),
    
    sd_cq =
      sd(
        cq,
        na.rm = TRUE
      ),
    
    min_cq =
      min(
        cq,
        na.rm = TRUE
      ),
    
    max_cq =
      max(
        cq,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )
standard_cq_summary %>%
  arrange(
    plate_id,
    fluor,
    log10_quantity
  ) %>%
  print(n = Inf)
ggplot(
  standard_cq,
  aes(
    x = log10_quantity,
    y = cq
  )
) +
  geom_point() +
  facet_grid(
    fluor ~ plate_id,
    scales = "free"
  ) +
  theme_minimal() +
  labs(
    title = "qPCR standard curves",
    subtitle = "CPP background cycles 1–3 + fluorophore-specific th.cyc threshold",
    x = expression(log[10] * "(starting quantity)"),
    y = "Cq"
  )
##now that standards look good, begin calibration
standard_cq <- standard_cq %>%
  mutate(
    use_for_calibration =
      sample != "std-0" &
      !is.na(cq) &
      crosses_threshold
  )
standard_models <- standard_cq %>%
  filter(
    use_for_calibration
  ) %>%
  group_by(
    plate_id,
    fluor
  ) %>%
  nest() %>%
  mutate(
    
    model = map(
      data,
      ~ lm(
        cq ~ log10_quantity,
        data = .x
      )
    ),
    
    intercept = map_dbl(
      model,
      ~ unname(coef(.x)[1])
    ),
    
    slope = map_dbl(
      model,
      ~ unname(coef(.x)[2])
    ),
    
    r_squared = map_dbl(
      model,
      ~ summary(.x)$r.squared
    ),
    
    efficiency_percent = map_dbl(
      model,
      ~ {
        s <- unname(coef(.x)[2])
        
        (
          10^(-1 / s) - 1
        ) * 100
      }
    )
  ) %>%
  ungroup()
standard_model_summary <- standard_models %>%
  select(
    plate_id,
    fluor,
    intercept,
    slope,
    r_squared,
    efficiency_percent
  ) %>%
  arrange(
    plate_id,
    fluor
  )

standard_model_summary %>%
  print(n = Inf)
ggplot(
  standard_cq %>%
    filter(use_for_calibration),
  aes(
    x = log10_quantity,
    y = cq
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  facet_grid(
    fluor ~ plate_id,
    scales = "free"
  ) +
  theme_minimal() +
  labs(
    title = "qPCR calibration curves",
    subtitle = "std-0 excluded from calibration",
    x = expression(log[10] * "(starting quantity)"),
    y = "Cq"
  )
## use standard curves to quantify unknowns based on Cq
unknown_quantified <- unknown_cq %>%
  left_join(
    standard_models %>%
      select(
        plate_id,
        fluor,
        intercept,
        slope,
        r_squared,
        efficiency_percent
      ),
    by = c(
      "plate_id",
      "fluor"
    )
  ) %>%
  mutate(
    
    log10_sq = case_when(
      cq_status == "Cq_detected" ~
        (cq - intercept) / slope,
      
      TRUE ~
        NA_real_
    ),
    
    starting_quantity_sq =
      10^log10_sq
  )
unknown_quantified %>%
  select(
    plate_id,
    fluor,
    well,
    sample,
    cq,
    log10_sq,
    starting_quantity_sq,
    cq_status
  ) %>%
  arrange(
    plate_id,
    fluor,
    well
  ) %>%
  print(n = 100)
## average technical triplicates
qpcr_avg <- unknown_quantified %>%
  group_by(
    fluor,
    sample,
    day,
    replicate,
    sediment,
    treatment,
    strain,
    sample_type
  ) %>%
  summarise(
    
    mean_sq = mean(
      starting_quantity_sq,
      na.rm = TRUE
    ),
    
    sd_sq = sd(
      starting_quantity_sq,
      na.rm = TRUE
    ),
    
    n_wells = n(),
    
    n_detected = sum(
      !is.na(starting_quantity_sq)
    ),
    
    mean_cq = mean(
      cq,
      na.rm = TRUE
    ),
    
    sd_cq = sd(
      cq,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    
    # Avoid NaN for samples with no detected wells
    mean_sq = if_else(
      n_detected == 0,
      NA_real_,
      mean_sq
    ),
    
    mean_cq = if_else(
      n_detected == 0,
      NA_real_,
      mean_cq
    ),
    
    sd_sq = if_else(
      n_detected < 2,
      NA_real_,
      sd_sq
    ),
    
    sd_cq = if_else(
      n_detected < 2,
      NA_real_,
      sd_cq
    )
  )
## apply correction for sample volume and elution volume
qpcr_avg <- qpcr_avg %>%
  mutate(
    
    sample_volume_mL = case_when(
      sample_type == "sediment slurry" ~ 1.45,
      sample_type == "dissolved capsule" ~ 0.2,
      TRUE ~ NA_real_
    ),
    
    gene_copies_per_mL =
      mean_sq * 50 / sample_volume_mL,
    
    gene_copies_sd_per_mL =
      sd_sq * 50 / sample_volume_mL
  )
## preliminary plotting
qpcr_avg_plot <- qpcr_avg %>%
  mutate(
    day = as.numeric(day)
  )
library(tidyverse)
library(ggpubr)
library(scales)

# Dodge position shared by means and error bars
pd <- position_dodge(width = 0.3)

qpcr_plot_data <- qpcr_avg %>%
  filter(
    !is.na(sediment),
    !is.na(treatment),
    !is.na(sample_type),
    !is.na(gene_copies_per_mL),
    gene_copies_per_mL > 0
  ) %>%
  mutate(
    day = as.numeric(day)
  )


ggplot(
  qpcr_plot_data,
  aes(
    x = day,
    y = gene_copies_per_mL,
    color = strain
  )
) +
  
  # Individual biological replicates
  geom_point(
    alpha = 0.4,
    size = 1.5,
    position = position_jitter(
      width = 0.1,
      height = 0
    )
  ) +
  
  # Mean across biological replicates
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = strain),
    position = pd
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    position = pd
  ) +
  
  # Mean +/- SE across biological replicates
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.2,
    position = pd
  ) +
  
  # Log10 y axis
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
  
  # Match your previous panel arrangement
  facet_grid(
    sediment ~ treatment + sample_type
  ) +
  
  labs(
    title = "Mean Gene Copies per mL",
    y = "Gene Copies per mL (log scale)",
    x = "Time (days)",
    color = "Strain"
  ) +
  
  theme_pubr()
## redo with LOD
qpcr_lod_full <- unknown_quantified %>%
  
  # Flag combinations manually classified as false positives
  left_join(
    false_positives %>%
      mutate(false_positive = TRUE),
    by = c(
      "sediment",
      "strain",
      "treatment",
      "day"
    )
  ) %>%
  
  mutate(
    false_positive =
      replace_na(
        false_positive,
        FALSE
      ),
    
    # -------------------------------------------------------------------------
    # Strain-specific LOD on starting-quantity scale
    # -------------------------------------------------------------------------
    
    LOD = case_when(
      strain %in% c(
        "n.penta",
        "p.resin"
      ) ~ 50,
      
      strain %in% c(
        "p.putida",
        "sphingo.sp"
      ) ~ 50,
      
      TRUE ~ NA_real_
    ),
    
    # -------------------------------------------------------------------------
    # Detection / quantification flags
    # -------------------------------------------------------------------------
    
    # No quantity returned from the th.cyc workflow
    nondetect =
      is.na(starting_quantity_sq),
    
    # A quantity was returned, but it is below the defined LOD
    below_LOD =
      !is.na(starting_quantity_sq) &
      !is.na(LOD) &
      starting_quantity_sq < LOD,
    
    # Anything that will be substituted with LOD
    LOD_substituted =
      false_positive |
      nondetect |
      below_LOD,
    
    # -------------------------------------------------------------------------
    # LOD-substituted quantity
    # -------------------------------------------------------------------------
    
    sq_lod = case_when(
      LOD_substituted ~ LOD,
      TRUE ~ starting_quantity_sq
    )
  )
qpcr_avg_lod <- qpcr_lod_full %>%
  group_by(
    fluor,
    sample,
    day,
    replicate,
    sediment,
    treatment,
    strain,
    sample_type
  ) %>%
  summarise(
    mean_sq = mean(
      sq_lod,
      na.rm = TRUE
    ),
    
    sd_sq = sd(
      sq_lod,
      na.rm = TRUE
    ),
    
    n = n(),
    
    # Number with an actual calculated quantity
    n_detected =
      sum(!nondetect),
    
    # No Cq / no calculated quantity
    n_nondetect =
      sum(nondetect),
    
    # Cq obtained, but calculated quantity < LOD
    n_below_LOD =
      sum(below_LOD),
    
    # Manually classified false positives
    n_false_positive =
      sum(false_positive),
    
    # Total wells whose value was replaced by LOD
    n_LOD_substituted =
      sum(LOD_substituted),
    
    .groups = "drop"
  )
qpcr_avg_lod <- qpcr_avg_lod %>%
  mutate(
    sample_volume_mL = case_when(
      sample_type == "sediment slurry" ~ 1.45,
      sample_type == "dissolved capsule" ~ 0.2,
      TRUE ~ NA_real_
    ),
    
    gene_copies_per_mL =
      mean_sq * 50 / sample_volume_mL,
    
    gene_copies_sd_per_mL =
      sd_sq * 50 / sample_volume_mL
  )
## replot
pd <- position_dodge(width = 0.3)
#flag lods
qpcr_avg_lod <- qpcr_avg_lod %>%
  mutate(
    LOD_status = case_when(
      n_LOD_substituted == 0 ~ "No LOD substitution",
      n_LOD_substituted == n ~ "All wells at LOD",
      n_LOD_substituted > 0 ~ "Some wells at LOD",
      TRUE ~ NA_character_
    )
  )


ggplot(
  qpcr_avg_lod %>%
    filter(
      !is.na(sediment),
      !is.na(treatment),
      !is.na(sample_type)
    ),
  aes(
    x = as.numeric(day),
    y = gene_copies_per_mL,
    color = strain
  )
) +
  
  # Mean trajectory
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = strain),
    position = pd
  ) +
  
  # Mean points
  stat_summary(
    fun = mean,
    geom = "point",
    position = pd
  ) +
  
  # SE among biological replicates
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.2,
    position = pd
  ) +
  
  # Individual biological samples, with LOD status flagged
  geom_point(
    aes(
      shape = LOD_status
    ),
    size = 2,
    alpha = 0.75,
    position = position_jitter(
      width = 0.08,
      height = 0
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "No LOD substitution" = 16,
      "Some wells at LOD"   = 17,
      "All wells at LOD"    = 4
    )
  ) +
  
  scale_color_manual(
    values = strain_colors
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
    sediment ~ treatment + sample_type
  ) +
  
  labs(
    title = "Mean Gene Copies per mL",
    y = "Gene Copies per mL (log scale)",
    x = "Time (days)",
    color = "Strain",
    shape = "LOD status"
  ) +
  
  theme_pubr()
##reference N values
qpcr_relative <- qpcr_avg_lod %>%
  mutate(
    control_treatment = case_when(
      sample_type == "sediment slurry" ~ "N",
      sample_type == "dissolved capsule" ~ "S",
      TRUE ~ NA_character_
    )
  )
control_reference <- qpcr_relative %>%
  filter(
    treatment == control_treatment
  ) %>%
  group_by(
    strain,
    sediment,
    sample_type,
    day
  ) %>%
  summarise(
    control_mean = mean(
      gene_copies_per_mL,
      na.rm = TRUE
    ),
    
    control_sd = sd(
      gene_copies_per_mL,
      na.rm = TRUE
    ),
    
    control_n = n(),
    
    .groups = "drop"
  )
qpcr_relative <- qpcr_relative %>%
  left_join(
    control_reference,
    by = c(
      "strain",
      "sediment",
      "sample_type",
      "day"
    )
  ) %>%
  mutate(
    fold_change_vs_control =
      gene_copies_per_mL / control_mean,
    
    log2_fold_change_vs_control =
      log2(fold_change_vs_control)
  )
qpcr_relative_treated <- qpcr_relative %>%
  filter(
    treatment != control_treatment,
    !is.na(log2_fold_change_vs_control)
  )
pd <- position_dodge(width = 0.3)

ggplot(
  qpcr_relative_treated,
  aes(
    x = as.numeric(day),
    y = log2_fold_change_vs_control,
    color = strain
  )
) +
  
  # Individual biological replicates
  geom_point(
    alpha = 0.4,
    size = 1.5,
    position = position_jitter(
      width = 0.1,
      height = 0
    )
  ) +
  
  # Mean across biological replicates
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = strain),
    position = pd
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    position = pd
  ) +
  
  # SE across biological replicates
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.2,
    position = pd
  ) +
  
  # Matched control = 0 on log2 scale
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  facet_grid(
    sediment ~ treatment + sample_type
  ) +
  
  labs(
    title = "Gene Copy Abundance Relative to Matched Control",
    subtitle = "Sediment slurry relative to N; dissolved capsule relative to sterile-capsule control S",
    x = "Time (days)",
    y = expression(log[2] * " fold change vs. matched control"),
    color = "Strain"
  ) +
  
  theme_pubr()+
  scale_color_manual(
    values = strain_colors
  )
##test as bar charts
gene_copy_bar <- qpcr_avg_lod %>%
  filter(
    !is.na(sediment),
    !is.na(treatment),
    !is.na(sample_type),
    !is.na(gene_copies_per_mL)
  ) %>%
  mutate(
    day = as.numeric(day)
  ) %>%
  group_by(
    day,
    strain,
    sediment,
    treatment,
    sample_type
  ) %>%
  summarise(
    mean_gene_copies = mean(
      gene_copies_per_mL,
      na.rm = TRUE
    ),
    
    se_gene_copies =
      sd(
        gene_copies_per_mL,
        na.rm = TRUE
      ) / sqrt(n()),
    
    n = n(),
    
    .groups = "drop"
  )
ggplot(
  gene_copy_bar,
  aes(
    x = factor(day),
    y = mean_gene_copies,
    fill = strain
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = mean_gene_copies - se_gene_copies,
      ymax = mean_gene_copies + se_gene_copies
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks(
      "log10",
      function(x) 10^x
    ),
    labels = scales::trans_format(
      "log10",
      scales::math_format(10^.x)
    )
  ) +
  facet_grid(
    sediment ~ treatment + sample_type
  ) +
  labs(
    title = "Mean Gene Copies per mL",
    x = "Time (days)",
    y = "Gene Copies per mL (log scale)",
    fill = "Strain"
  ) +
  theme_pubr()+
  scale_fill_manual(
    values = strain_colors
  )
fold_change_bar <- qpcr_relative_treated %>%
  filter(
    !is.na(sediment),
    !is.na(treatment),
    !is.na(sample_type),
    !is.na(log2_fold_change_vs_control)
  ) %>%
  mutate(
    day = as.numeric(day)
  ) %>%
  group_by(
    day,
    strain,
    sediment,
    treatment,
    sample_type
  ) %>%
  summarise(
    mean_log2FC = mean(
      log2_fold_change_vs_control,
      na.rm = TRUE
    ),
    
    se_log2FC =
      sd(
        log2_fold_change_vs_control,
        na.rm = TRUE
      ) / sqrt(n()),
    
    n = n(),
    
    .groups = "drop"
  )
ggplot(
  fold_change_bar,
  aes(
    x = factor(day),
    y = mean_log2FC,
    fill = strain
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = mean_log2FC - se_log2FC,
      ymax = mean_log2FC + se_log2FC
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  facet_grid(
    sediment ~ treatment + sample_type
  ) +
  labs(
    title = "Gene Copy Abundance Relative to Matched Control",
    subtitle = "Sediment slurry relative to N; dissolved capsule relative to sterile-capsule control S",
    x = "Time (days)",
    y = expression(log[2] * " fold change vs. matched control"),
    fill = "Strain"
  ) +
  theme_pubr()+
  scale_fill_manual(
    values = strain_colors
  )
## with replicates
ggplot(
  fold_change_bar,
  aes(
    x = factor(day),
    y = mean_log2FC,
    fill = strain
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    alpha = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = mean_log2FC - se_log2FC,
      ymax = mean_log2FC + se_log2FC
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  geom_point(
    data = qpcr_relative_treated,
    aes(
      x = factor(as.numeric(day)),
      y = log2_fold_change_vs_control,
      color = strain
    ),
    inherit.aes = FALSE,
    position = position_jitter(width = 0.08),
    alpha = 0.6,
    size = 1.5
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  facet_grid(
    sediment ~ treatment + sample_type
  ) +
  labs(
    title = "Gene Copy Abundance Relative to Matched Control",
    x = "Time (days)",
    y = expression(log[2] * " fold change vs. matched control"),
    fill = "Strain",
    color = "Strain"
  ) +
  theme_pubr()+
  scale_fill_manual(
    values = strain_colors
  ) +
  scale_color_manual(
    values = strain_colors
  )
## testing boxplots
ggplot(
  qpcr_avg_lod %>%
    filter(
      !is.na(sediment),
      !is.na(treatment),
      !is.na(sample_type),
      !is.na(gene_copies_per_mL)
    ),
  aes(
    x = factor(day),
    y = gene_copies_per_mL,
    fill = strain
  )
) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.65,
    alpha = 0.6,
    outlier.shape = NA
  ) +
  geom_point(
    aes(color = strain),
    position = position_jitterdodge(
      jitter.width = 0.08,
      dodge.width = 0.8
    ),
    size = 1.5,
    alpha = 0.7
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks(
      "log10",
      function(x) 10^x
    ),
    labels = scales::trans_format(
      "log10",
      scales::math_format(10^.x)
    )
  ) +
  facet_grid(
    sediment ~ treatment + sample_type
  ) +
  labs(
    title = "Gene Copies per mL",
    x = "Time (days)",
    y = "Gene Copies per mL (log scale)",
    fill = "Strain",
    color = "Strain"
  ) +
  theme_pubr()+
  scale_fill_manual(
    values = strain_colors
  ) +
  scale_color_manual(
    values = strain_colors
  )
## log fold change boxplots
ggplot(
  qpcr_relative_treated %>%
    filter(
      !is.na(sediment),
      !is.na(treatment),
      !is.na(sample_type),
      !is.na(log2_fold_change_vs_control)
    ),
  aes(
    x = factor(day),
    y = log2_fold_change_vs_control,
    fill = strain
  )
) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.65,
    alpha = 0.6,
    outlier.shape = NA
  ) +
  geom_point(
    aes(color = strain),
    position = position_jitterdodge(
      jitter.width = 0.08,
      dodge.width = 0.8
    ),
    size = 1.5,
    alpha = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  facet_grid(
    sediment ~ treatment + sample_type
  ) +
  labs(
    title = "Gene Copy Abundance Relative to Matched Control",
    subtitle = "Sediment slurry relative to N; dissolved capsule relative to sterile-capsule control S",
    x = "Time (days)",
    y = expression(log[2] * " fold change vs. matched control"),
    fill = "Strain",
    color = "Strain"
  ) +
  theme_pubr()+
  scale_fill_manual(
    values = strain_colors
  ) +
  scale_color_manual(
    values = strain_colors
  )
##just day 42
day42_bar <- qpcr_avg_lod %>%
  filter(
    as.numeric(day) == 42,
    !is.na(sediment),
    !is.na(treatment),
    !is.na(sample_type),
    !is.na(gene_copies_per_mL)
  ) %>%
  group_by(
    strain,
    sediment,
    treatment,
    sample_type
  ) %>%
  summarise(
    mean_gene_copies = mean(
      gene_copies_per_mL,
      na.rm = TRUE
    ),
    
    se_gene_copies =
      sd(
        gene_copies_per_mL,
        na.rm = TRUE
      ) / sqrt(n()),
    
    n = n(),
    
    .groups = "drop"
  )
ggplot(
  day42_bar,
  aes(
    x = strain,
    y = mean_gene_copies,
    color = treatment
  )
) +
  
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_gene_copies - se_gene_copies,
      ymax = mean_gene_copies + se_gene_copies
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  scale_y_log10(
    breaks = scales::trans_breaks(
      "log10",
      function(x) 10^x
    ),
    labels = scales::trans_format(
      "log10",
      scales::math_format(10^.x)
    )
  ) +
  
  facet_grid(
    sediment ~ sample_type,
    scales = "free_x",
    space = "free_x"
  ) +
  
  labs(
    title = "Gene Copies per mL at Day 42",
    subtitle = "Treatment comparisons within strains",
    x = "Strain",
    y = "Gene Copies per mL (log scale)",
    fill = "Treatment"
  ) +
  
  theme_pubr() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )
## plot by strain to better compare treatment
qpcr_avg_lod <- qpcr_avg_lod %>%
  mutate(
    LOD_status = case_when(
      n_LOD_substituted == 0 ~ "No LOD substitution",
      n_LOD_substituted == n ~ "All wells at LOD",
      n_LOD_substituted > 0 ~ "Some wells at LOD",
      TRUE ~ NA_character_
    )) %>% 
  filter(sample_type == "sediment slurry")

pd <- position_dodge(width = 4)
ggplot(
  qpcr_avg_lod %>%
    filter(
      !is.na(sediment),
      !is.na(treatment),
      !is.na(sample_type)
    ),
  aes(
    x = as.numeric(day),
    y = gene_copies_per_mL,
    color = treatment
  )
) +
  
  # Mean trajectory
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = treatment),
    position = pd
  ) +
  
  # Mean points
  stat_summary(
    fun = mean,
    geom = "point",
    position = pd,
    size = 3
  ) +
  
  # SE among biological replicates
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.2,
    position = pd
  ) +
  
  # Individual biological samples, with LOD status flagged
  # geom_point(
  #   aes(
  #     shape = LOD_status
  #   ),
  #   size = 2,
  #   alpha = 0.75,
  #   position = pd
  #   ) +
  
  scale_shape_manual(
    values = c(
      "No LOD substitution" = 16,
      "Some wells at LOD"   = 17,
      "All wells at LOD"    = 4
    )
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
      strain = as_labeller(strain_labels, label_parsed))
  ) +
  
  labs(
    title = "Mean Gene Copies per mL",
    y = "Gene Copies per mL (log scale)",
    x = "Time (days)",
    color = "Treatment",
    shape = "LOD status"
  ) +
  
  theme_pubr()+
  scale_color_manual(values = c(
    "E" = "#2E92A2FF",
    "F" = "#D85A44FF", 
    "N" = "black", 
    "S" = "gray"
  ))+
  theme(
    strip.text = element_text(
      face = "italic",
      size = 12))
      
