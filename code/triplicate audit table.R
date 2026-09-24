# ============================================================
# qPCR technical-triplicate QC audit table
# ============================================================

qpcr_triplicate_audit <- qpcr_full %>%
  
  # Keep experimental samples
  filter(
    sample_type == "sediment slurry",
    !is.na(sample),
    !is.na(strain),
    !is.na(day)
  ) %>%
  
  # One row per technical well
  distinct(
    plate_id,
    day,
    sample,
    sediment,
    treatment,
    replicate,
    strain,
    fluor,
    well,
    cq
  ) %>%
  
  # Group technical replicates
  group_by(
    plate_id,
    day,
    sample,
    sediment,
    treatment,
    replicate,
    strain,
    fluor
  ) %>%
  
  summarise(
    
    # Number of technical wells
    n_wells = n_distinct(well),
    
    # Detection information
    n_cq = sum(!is.na(cq)),
    n_cq_na = sum(is.na(cq)),
    
    # Cq summaries
    cq_mean = if_else(
      n_cq > 0,
      mean(cq, na.rm = TRUE),
      NA_real_
    ),
    
    cq_sd = if_else(
      n_cq > 1,
      sd(cq, na.rm = TRUE),
      NA_real_
    ),
    
    cq_min = if_else(
      n_cq > 0,
      min(cq, na.rm = TRUE),
      NA_real_
    ),
    
    cq_max = if_else(
      n_cq > 0,
      max(cq, na.rm = TRUE),
      NA_real_
    ),
    
    cq_range = if_else(
      n_cq > 1,
      max(cq, na.rm = TRUE) -
        min(cq, na.rm = TRUE),
      NA_real_
    ),
    
    # Keep well IDs and Cqs visible
    wells = paste(
      sort(unique(well)),
      collapse = ", "
    ),
    
    well_cqs = paste(
      paste0(
        well,
        "=",
        if_else(
          is.na(cq),
          "NA",
          sprintf("%.2f", cq)
        )
      ),
      collapse = "; "
    ),
    
    .groups = "drop"
  )
qpcr_triplicate_audit <- qpcr_triplicate_audit %>%
  mutate(
    
    # Not exactly three technical wells
    flag_n_wells = n_wells != 3,
    
    # Some wells have Cq and some do not
    flag_mixed_detection =
      n_cq > 0 &
      n_cq < n_wells,
    
    # Only one technical well detected
    flag_single_detection =
      n_cq == 1,
    
    # Large Cq spread among detected technical wells
    # Adjust this cutoff if desired
    flag_cq_spread =
      !is.na(cq_range) &
      cq_range > 2,
    
    # Overall flag
    flag_questionable =
      flag_n_wells |
      flag_mixed_detection |
      flag_cq_spread
  )
qpcr_triplicate_audit <- qpcr_triplicate_audit %>%
  rowwise() %>%
  mutate(
    
    qc_reason = paste(
      c(
        if (flag_n_wells)
          paste0(
            n_wells,
            " technical wells"
          )
        else NULL,
        
        if (flag_mixed_detection)
          paste0(
            n_cq,
            "/",
            n_wells,
            " wells assigned Cq"
          )
        else NULL,
        
        if (flag_cq_spread)
          paste0(
            "Cq range = ",
            round(cq_range, 2)
          )
        else NULL
      ),
      collapse = "; "
    )
    
  ) %>%
  ungroup()
qpcr_triplicate_audit %>%
  filter(flag_questionable) %>%
  arrange(
    as.numeric(day),
    plate_id,
    sample,
    strain
  ) %>%
  View()
