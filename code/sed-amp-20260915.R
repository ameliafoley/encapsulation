amp_files <- list.files(
  here("data", "sediment-qPCR", "regression"),
  pattern = "Quantification Amplification Results\\.xlsx$",
  full.names = TRUE
)

fluor_sheets <- c(
  "FAM",
  "HEX",
  "Texas Red",
  "Cy5"
)

read_amp_file <- function(file) {
  
  plate_id <- stringr::str_extract(
    basename(file),
    "sed\\d+"
  ) %>%
    stringr::str_remove("sed")
  
  sheets <- intersect(
    readxl::excel_sheets(file),
    fluor_sheets
  )
  
  purrr::map_dfr(sheets, function(sheet) {
    
    readxl::read_excel(
      file,
      sheet = sheet
    ) %>%
      rename(cycle = 1) %>%
      pivot_longer(
        cols = -cycle,
        names_to = "well",
        values_to = "fluorescence"
      ) %>%
      mutate(
        plate_id = plate_id,
        fluor = sheet,
        
        # A1 -> A01, A2 -> A02, but A10 stays A10
        well = stringr::str_replace(
          well,
          "^([A-H])(\\d)$",
          "\\10\\2"
        )
      )
  })
}

amplification <- purrr::map_dfr(
  amp_files,
  read_amp_file
)

amp_metadata <- qpcr_full %>%
  distinct(
    plate_id,
    well,
    fluor,
    strain,
    sample,
    day,
    replicate,
    sediment,
    treatment,
    sample_type
  )

amplification <- amplification %>%
  left_join(
    amp_metadata,
    by = c(
      "plate_id",
      "well",
      "fluor"
    )
  )
##check if that worked
# amplification %>%
#   filter(!is.na(sample)) %>%
#   distinct(
#     plate_id,
#     well,
#     fluor,
#     strain,
#     sample,
#     day,
#     treatment
#   ) %>%
#   arrange(
#     as.numeric(plate_id),
#     well,
#     fluor
#   ) %>%
#   # View()
# ##mistmatches
# amplification %>%
#   filter(is.na(sample)) %>%
#   distinct(
#     plate_id,
#     well,
#     fluor
#   ) %>%
#   # View() ##none

##function
plot_amp_qc <- function(
    sediment_check,
    treatment_check,
    strain_check,
    day_check
) {
  
  # Find wells corresponding to samples of interest
  selected_wells <- qpcr_full %>%
    filter(
      sediment == sediment_check,
      treatment == treatment_check,
      strain == strain_check,
      day == day_check
    ) %>%
    distinct(
      plate_id,
      well,
      fluor,
      sample,
      replicate
    )
  
  # Determine exact plate x fluorophore combinations
  plate_fluor <- selected_wells %>%
    distinct(
      plate_id,
      fluor
    )
  
  # Keep amplification curves from only those
  # plate x fluorophore combinations
  plot_data <- amplification %>%
    semi_join(
      plate_fluor,
      by = c("plate_id", "fluor")
    )
  
  # Flag selected wells
  plot_data <- plot_data %>%
    left_join(
      selected_wells %>%
        select(plate_id, fluor, well) %>%
        distinct() %>%
        mutate(selected = TRUE),
      by = c("plate_id", "fluor", "well")
    ) %>%
    mutate(
      selected = replace_na(selected, FALSE)
    )
  
  # Plot
  ggplot() +
    
    # Background curves from same fluorophore
    geom_line(
      data = plot_data %>% filter(!selected),
      aes(
        x = cycle,
        y = fluorescence,
        group = interaction(plate_id, fluor, well)
      ),
      color = "grey80",
      linewidth = 0.35
    ) +
    
    # Selected wells
    geom_line(
      data = plot_data %>% filter(selected),
      aes(
        x = cycle,
        y = fluorescence,
        color = well_label,
        group = interaction(plate_id, fluor, well)
      ),
      linewidth = 1
    ) +
    
    facet_wrap(
      ~plate_id,
      scales = "free_y"
    ) +
    
    # # Pseudo-log fluorescence scale
    # scale_y_continuous(
    #   trans = scales::pseudo_log_trans(base = 10)
    # ) +
    
    labs(
      title = "qPCR Amplification QC",
      subtitle = paste(
        sediment_check,
        treatment_check,
        strain_check,
        paste0("Day ", day_check)
      ),
      x = "Cycle",
      y = "Fluorescence",
      color = "Well"
    ) +
    
    theme_minimal()
}
##input info to check samples with high error for quality issues or faulty Ct determination 
# plot_amp_qc(
#   sediment_check = "REP",
#   treatment_check = "E",
#   strain_check = "p.putida",
#   day_check = 42
# )

##generate a report
library(tidyverse)
library(plotly)
library(htmltools)
library(htmlwidgets)

plot_amp_sample_strain <- function(
    sample_check,
    strain_check,
    day_check,
    amplification,
    qpcr_full,
    show_background = TRUE
) {
  
  # Select ONLY this sample x strain x day
  selected_wells <- qpcr_full %>%
    filter(
      sample == sample_check,
      strain == strain_check,
      day == day_check
    ) %>%
    distinct(
      plate_id,
      well,
      fluor,
      cq,
      sample,
      day,
      sediment,
      treatment,
      replicate,
      sample_type,
      strain
    ) %>%
    mutate(
      well_label = if_else(
        is.na(cq),
        paste0(well, " | Cq: NA"),
        paste0(well, " | Cq: ", round(cq, 2))
      )
    )
  
  if (nrow(selected_wells) == 0) {
    return(NULL)
  }
  
  plate_fluor <- selected_wells %>%
    distinct(
      plate_id,
      fluor
    )
  
  plot_data <- amplification %>%
    semi_join(
      plate_fluor,
      by = c("plate_id", "fluor")
    ) %>%
    left_join(
      selected_wells %>%
        select(
          plate_id,
          fluor,
          well,
          well_label
        ) %>%
        distinct() %>%
        mutate(selected = TRUE),
      by = c(
        "plate_id",
        "fluor",
        "well"
      )
    ) %>%
    mutate(
      selected = replace_na(selected, FALSE)
    )
  
  meta <- selected_wells %>%
    slice(1)
  
  n_wells <- selected_wells %>%
    distinct(
      plate_id,
      fluor,
      well
    ) %>%
    nrow()
  
  p <- ggplot()
  
  # Background curves from SAME plate + fluor
  if (show_background) {
    
    p <- p +
      geom_line(
        data = plot_data %>%
          filter(!selected),
        aes(
          x = cycle,
          y = fluorescence,
          group = interaction(
            plate_id,
            fluor,
            well
          )
        ),
        color = "grey85",
        linewidth = 0.3,
        alpha = 0.7
      )
  }
  
  # Three technical wells
  p <- p +
    geom_line(
      data = plot_data %>%
        filter(selected),
      aes(
        x = cycle,
        y = fluorescence,
        color = well_label,
        group = interaction(
          plate_id,
          fluor,
          well
        )
      ),
      linewidth = 1
    ) +
    
    labs(
      title = paste(
        sample_check,
        "—",
        strain_check
      ),
      
      subtitle = paste0(
        "Day ", day_check,
        " | Plate ", meta$plate_id,
        " | Sediment: ", meta$sediment,
        " | Treatment: ", meta$treatment,
        " | Biological replicate: ", meta$replicate,
        " | Technical wells: ", n_wells
      ),
      
      x = "Cycle",
      y = "Fluorescence",
      color = "Well"
    ) +
    
    theme_minimal(base_size = 11) +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 14
      ),
      plot.subtitle = element_text(
        size = 9
      ),
      legend.position = "bottom"
    )
  
  p
}
# plot_amp_sample_strain(
#   sample_check = "1-F-A",
#   strain_check = "p.resin",
#   amplification = amplification,
#   qpcr_full = qpcr_full
# )
