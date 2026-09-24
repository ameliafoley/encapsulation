## check amplification for threshold/baseline processed data
unknown_amp_plot <- unknown_processed %>%
  transmute(
    plate_id,
    fluor,
    well,
    r,
    curve = map(
      analysis,
      "curve"
    )
  ) %>%
  unnest(curve)
unknown_amp_plot <- unknown_amp_plot %>%
  left_join(
    unknown_cq %>%
      select(
        plate_id,
        fluor,
        well,
        cq,
        cq_status,
        max_signal,
        crosses_threshold
      ),
    by = c(
      "plate_id",
      "fluor",
      "well"
    )
  )
sample_to_check <- "1-S-C"

unknown_amp_plot %>%
  filter(
    sample == sample_to_check
  ) %>%
  ggplot(
    aes(
      x = cycle,
      y = fluorescence_processed
    )
  ) +
  geom_point(
    alpha = 0.35,
    size = 1
  ) +
  # geom_line(
  #   linewidth = 0.8
  # ) +
  
  # Fluorescence threshold
  geom_hline(
    aes(
      yintercept = r
    ),
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  # th.cyc Cq
  geom_vline(
    aes(
      xintercept = cq
    ),
    linetype = "dotted",
    linewidth = 0.7,
    na.rm = TRUE
  ) +
  
  facet_grid(
    fluor ~ well,
    scales = "free_y"
  ) +
  
  theme_bw() +
  
  labs(
    title = paste(
      "Amplification curves:",
      sample_to_check
    ),
    subtitle =
      "Dashed horizontal line = threshold; dotted vertical line = Cq",
    x = "Cycle",
    y = "Baseline-corrected fluorescence"
  )
plot_amplification <- function(
    sample_name,
    fluor_name = NULL
) {
  
  d <- unknown_amp_plot %>%
    filter(
      sample == sample_name
    )
  
  if (!is.null(fluor_name)) {
    d <- d %>%
      filter(
        fluor == fluor_name
      )
  }
  
  ggplot(
    d,
    aes(
      x = cycle,
      y = fluorescence_processed
    )
  ) +
    
    geom_point(
      alpha = 0.35,
      size = 1
    ) +
    
    geom_line(
      linewidth = 0.8
    ) +
    
    # Fluorescence threshold
    geom_hline(
      aes(
        yintercept = r
      ),
      linetype = "dashed",
      linewidth = 0.7
    ) +
    
    # th.cyc Cq
    geom_vline(
      aes(
        xintercept = cq
      ),
      linetype = "dotted",
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    
    facet_grid(
      fluor + day ~ well,
      scales = "free_y"
    ) +
    
    theme_bw() +
    
    labs(
      title = paste(
        "Amplification curves:",
        sample_name
      ),
      subtitle =
        "Dashed horizontal = threshold r; dotted vertical = th.cyc Cq",
      x = "Cycle",
      y = "Baseline-corrected fluorescence"
    )
}
plot_amplification(
  "1-S-A",
  "HEX"
)
## show with standards and controls
control_nested <- amplification %>%
  filter(
    sample == "NTC" |
      str_detect(sample, "^std-")
  ) %>%
  group_by(
    plate_id,
    fluor,
    well
  ) %>%
  nest() %>%
  ungroup() %>%
  
  left_join(
    r_values %>%
      select(
        fluor,
        r
      ),
    by = "fluor"
  )
control_processed <- control_nested %>%
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
control_amp_plot <- control_processed %>%
  transmute(
    plate_id,
    fluor,
    well,
    r,
    
    curve = map(
      analysis,
      "curve"
    ),
    
    result = map(
      analysis,
      "result"
    )
  ) %>%
  unnest(curve) %>%
  unnest(
    result,
    names_sep = "result_"
  )
control_amp_plot <- control_amp_plot %>%
  rename(
    processing_ok = resultresult_processing_ok,
    processing_message = resultresult_processing_message,
    threshold = resultresult_threshold,
    max_signal = resultresult_max_signal,
    crosses_threshold = resultresult_crosses_threshold,
    cq = resultresult_cq
  )
plot_control_amplification <- function(
    plate_name,
    fluor_name
) {
  
  d <- control_amp_plot %>%
    filter(
      plate_id == plate_name,
      fluor == fluor_name
    ) %>%
    mutate(
      sample = factor(
        sample,
        levels = c(
          "NTC",
          paste0("std-", 0:8)
        )
      )
    )
  
  ggplot(
    d,
    aes(
      x = cycle,
      y = fluorescence_processed
    )
  ) +
    
    geom_point(
      alpha = 0.25,
      size = 0.6
    ) +
    
    geom_line(
      linewidth = 0.7
    ) +
    
    # Final threshold used for th.cyc
    geom_hline(
      aes(
        yintercept = threshold
      ),
      linetype = "dashed",
      linewidth = 0.7
    ) +
    
    # Calculated Cq
    geom_vline(
      aes(
        xintercept = cq
      ),
      linetype = "dotted",
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    
    facet_grid(
      sample ~ well,
      scales = "free_y",
      drop = TRUE
    ) +
    
    theme_bw() +
    
    labs(
      title = paste(
        "Standards and NTCs:",
        "Plate", plate_name,
        "|", fluor_name
      ),
      
      subtitle =
        "Dashed horizontal = threshold; dotted vertical = th.cyc Cq",
      
      x = "Cycle",
      y = "Baseline-corrected fluorescence"
    )
}
plot_control_amplification(
  "1",
  "FAM"
)
## plot all wells on plate
names(unknown_amp_plot)
unknown_amp_plate <- unknown_amp_plot %>%
  rename(
    threshold = r
  ) %>%
  select(
    plate_id,
    fluor,
    well,
    cycle,
    fluorescence,
    fluorescence_processed,
    strain,
    sample,
    day,
    replicate,
    sediment,
    treatment,
    sample_type,
    threshold,
    cq,
    max_signal,
    crosses_threshold
  )

control_amp_plate <- control_amp_plot %>%
  select(
    plate_id,
    fluor,
    well,
    cycle,
    fluorescence,
    fluorescence_processed,
    strain,
    sample,
    day,
    replicate,
    sediment,
    treatment,
    sample_type,
    threshold,
    cq,
    max_signal,
    crosses_threshold
  )
plate_amp_plot <- bind_rows(
  unknown_amp_plate,
  control_amp_plate
)
plate_amp_plot <- plate_amp_plot %>%
  mutate(
    plate_row = str_extract(
      well,
      "^[A-H]"
    ),
    
    plate_col = as.integer(
      str_extract(
        well,
        "[0-9]+$"
      )
    ),
    
    plate_row = factor(
      plate_row,
      levels = LETTERS[1:8]
    ),
    
    plate_col = factor(
      plate_col,
      levels = 1:12
    )
  )
plate_amp_plot %>%
  distinct(
    plate_id,
    fluor,
    well
  ) %>%
  count(
    plate_id,
    fluor
  ) %>%
  print(n = Inf)
plot_amplification_plate <- function(
    plate_name,
    fluor_name
) {
  
  d <- plate_amp_plot %>%
    filter(
      plate_id == plate_name,
      fluor == fluor_name
    )
  
  # One row per well for annotations
  labels <- d %>%
    distinct(
      plate_id,
      fluor,
      well,
      plate_row,
      plate_col,
      sample,
      day,
      cq,
      crosses_threshold
    ) %>%
    mutate(
      well_label = case_when(
        
        sample == "NTC" ~
          paste0(
            well,
            "\nNTC",
            "\nCq=",
            if_else(
              is.na(cq),
              "NA",
              sprintf("%.1f", cq)
            )
          ),
        
        str_detect(sample, "^std-") ~
          paste0(
            well,
            "\n",
            sample,
            "\nCq=",
            if_else(
              is.na(cq),
              "NA",
              sprintf("%.1f", cq)
            )
          ),
        
        TRUE ~
          paste0(
            well,
            "\n",
            sample,
            " D",
            day,
            "\nCq=",
            if_else(
              is.na(cq),
              "NA",
              sprintf("%.1f", cq)
            )
          )
      )
    )
  
  ggplot(
    d,
    aes(
      x = cycle,
      y = fluorescence_processed
    )
  ) +
    
    # Processed amplification curve
    geom_line(
      linewidth = 0.35
    ) +
    
    # Threshold used for this fluorophore
    geom_hline(
      aes(
        yintercept = threshold
      ),
      linetype = "dashed",
      linewidth = 0.3
    ) +
    
    # th.cyc Cq
    geom_vline(
      aes(
        xintercept = cq
      ),
      linetype = "dotted",
      linewidth = 0.3,
      na.rm = TRUE
    ) +
    
    # Well/sample/Cq annotation
    geom_text(
      data = labels,
      aes(
        x = -Inf,
        y = Inf,
        label = well_label
      ),
      inherit.aes = FALSE,
      hjust = -0.03,
      vjust = 1.05,
      size = 1.7
    ) +
    
    # Actual 8 × 12 plate arrangement
    facet_grid(
      plate_row ~ plate_col,
      scales = "fixed",
      drop = FALSE
    ) +
    
    theme_bw() +
    
    theme(
      axis.text = element_text(
        size = 4
      ),
      
      axis.title = element_text(
        size = 8
      ),
      
      strip.text = element_text(
        size = 7
      ),
      
      panel.spacing = unit(
        0.05,
        "lines"
      )
    ) +
    
    labs(
      title = paste(
        "Plate", plate_name,
        "|", fluor_name
      ),
      
      subtitle =
        "Dashed horizontal = threshold; dotted vertical = th.cyc Cq",
      
      x = "Cycle",
      
      y = "Baseline-corrected fluorescence"
    )
}
plot_amplification_plate(
  "3",
  "FAM"
)
## export all to PDF
# =============================================================================
# Export all plate x fluorophore amplification plots to one PDF
# =============================================================================

plate_ids <- sort(
  unique(plate_amp_plot$plate_id)
)

fluors <- c(
  "Cy5",
  "FAM",
  "HEX",
  "Texas Red"
)

pdf(
  file = "all_plate_amplification_QC.pdf",
  width = 16,
  height = 11,
  onefile = TRUE
)

for (plate in plate_ids) {
  
  for (fluor_name in fluors) {
    
    p <- plot_amplification_plate(
      plate_name = plate,
      fluor_name = fluor_name
    )
    
    print(p)
  }
}

dev.off()
##log scale amplification plots
plot_amplification_plate_log <- function(
    plate_name,
    fluor_name
) {
  
  d <- plate_amp_plot %>%
    filter(
      plate_id == plate_name,
      fluor == fluor_name
    )
  
  # One row per well for annotations
  labels <- d %>%
    distinct(
      plate_id,
      fluor,
      well,
      plate_row,
      plate_col,
      sample,
      day,
      cq,
      crosses_threshold
    ) %>%
    mutate(
      well_label = case_when(
        
        sample == "NTC" ~
          paste0(
            well,
            "\nNTC",
            "\nCq=",
            if_else(
              is.na(cq),
              "NA",
              sprintf("%.1f", cq)
            )
          ),
        
        str_detect(sample, "^std-") ~
          paste0(
            well,
            "\n",
            sample,
            "\nCq=",
            if_else(
              is.na(cq),
              "NA",
              sprintf("%.1f", cq)
            )
          ),
        
        TRUE ~
          paste0(
            well,
            "\n",
            sample,
            " D",
            day,
            "\nCq=",
            if_else(
              is.na(cq),
              "NA",
              sprintf("%.1f", cq)
            )
          )
      )
    )
  
  ggplot(
    d %>%
      filter(
        fluorescence_processed > 0
      ),
    aes(
      x = cycle,
      y = fluorescence_processed
    )
  ) +
    
    geom_line(
      linewidth = 0.35
    ) +
    
    # Same absolute threshold used for th.cyc
    geom_hline(
      aes(
        yintercept = threshold
      ),
      linetype = "dashed",
      linewidth = 0.35
    ) +
    
    # th.cyc Cq
    geom_vline(
      aes(
        xintercept = cq
      ),
      linetype = "dotted",
      linewidth = 0.35,
      na.rm = TRUE
    ) +
    
    geom_text(
      data = labels,
      aes(
        x = -Inf,
        y = Inf,
        label = well_label
      ),
      inherit.aes = FALSE,
      hjust = -0.03,
      vjust = 1.05,
      size = 1.7
    ) +
    
    # True log10 fluorescence scale
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
      plate_row ~ plate_col,
      scales = "fixed",
      drop = FALSE
    ) +
    
    theme_bw() +
    
    theme(
      axis.text = element_text(size = 4),
      axis.title = element_text(size = 8),
      strip.text = element_text(size = 7),
      
      panel.spacing = unit(
        0.05,
        "lines"
      )
    ) +
    
    labs(
      title = paste(
        "Plate",
        plate_name,
        "|",
        fluor_name
      ),
      
      subtitle =
        "Log10 fluorescence; dashed = threshold; dotted = th.cyc Cq",
      
      x = "Cycle",
      
      y = "Baseline-corrected fluorescence (log10 scale)"
    )
}
plot_amplification_plate_log(
  "4",
  "FAM"
)
