###############################################################
# Import
###############################################################

df <- read_excel(
  here("data",
       "Aim 2.3 Sediment Sample Code.xlsx")
)

library(dplyr)
library(tidyr)

sample_code <- df %>%
  separate_wider_delim(
    Abbreviation,
    delim = "-",
    names = c("Location_Code", "Treatment_Code", "Replicate", 
              "Timepoint", "Dissolved_Code"),
    too_few = "align_start"
  ) %>%
  mutate(
    # Sediment location
    Sediment_Location = case_when(
      Location_Code == "M" ~ "Mains",
      Location_Code == "S" ~ "Mains Sterile",
      Location_Code == "P" ~ "Pescara",
      Location_Code == "R" ~ "Republic",
      TRUE ~ NA_character_
    ),
    
    # Treatment
    Treatment = case_when(
      Treatment_Code == "F" ~ "Free",
      Treatment_Code == "N" ~ "None",
      Treatment_Code == "S" ~ "Abiotic Capsule",
      Treatment_Code == "E" ~ "Encapsulated",
      TRUE ~ NA_character_
    ),
    
    # Convert timepoint to numeric
    Timepoint = as.numeric(Timepoint),
    
    # Sample type
    Sample_Type = case_when(
      Dissolved_Code == "D" ~ "Dissolved Capsule",
      TRUE ~ "Sediment Slurry"
    )
  ) %>%
  
  # Remove temporary code columns
  select(-Location_Code, -Treatment_Code, -Dissolved_Code)

#import PAH results
pah_results <- read_excel(
  here("data",
       "Sediment Concentration report.xlsx")
)

##combine sample code and PAH results
# Make the join columns the same data type
sample_code <- sample_code %>%
  mutate(`PAH Extract No.` = as.character(`PAH Extract No.`))

pah_results <- pah_results %>%
  mutate(Replicate = as.character(Replicate))

# Add the sample information to the PAH results
pah_joined <- pah_results %>%
  left_join(
    sample_code,
    by = c("Replicate" = "PAH Extract No.")
  )
#rename

pah_joined <- pah_joined %>%
  rename(
    pah_extract_no = Replicate,
    pah_sample_type = `Sample Type`,
    molecule_name = `Molecule Name`,
    concentration = `Calculated Concentration`,
    batch_figures_of_merit = `Batch Figures Of Merit`,
    total_area = `Total Area`,
    molecule_retention_time = `Molecule Retention Time`,
    sample = Sample,
    replicate = Replicate.y,
    timepoint = Timepoint,
    sediment_location = Sediment_Location,
    treatment = Treatment,
    sample_type = Sample_Type
  )

ggplot(
  pah_joined,
  aes(x = treatment, y = concentration)
) +
  geom_boxplot() +
  facet_wrap(~ sediment_location) +
  theme_classic()

pah_joined <- pah_joined %>%
  mutate(
    sediment_location = factor(
      sediment_location,
      levels = c("Mains", "Mains Sterile", "Pescara", "Republic")
    ),
    treatment = factor(
      treatment,
      levels = c("None", "Free", "Abiotic Capsule", "Encapsulated")
    ),
    timepoint = as.numeric(timepoint),
    replicate = factor(replicate),
    sample_type = factor(sample_type)
  )
ggplot(pah_joined, aes(x = concentration)) +
  geom_histogram(bins = 40) +
  theme_classic() +
  labs(
    x = "PAH Concentration",
    y = "Number of observations",
    title = "Distribution of PAH Concentrations"
  )
total_pah <- pah_joined %>%
  group_by(
    pah_extract_no,
    sample,
    sediment_location,
    treatment,
    replicate,
    timepoint,
    sample_type
  ) %>%
  summarise(
    total_pah = sum(concentration, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(
  total_pah,
  aes(
    x = treatment,
    y = total_pah,
    color = treatment
  )
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    width = 0.15,
    size = 2,
    alpha = 0.7
  ) +
  facet_grid(
    sample_type ~ sediment_location,
    scales = "free_y"
  ) +
  theme_classic() +
  labs(
    x = "Treatment",
    y = "Total PAH Concentration",
    color = "Treatment",
    title = "Total PAH Concentration by Sample Type"
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )
ggplot(
  total_pah,
  aes(
    x = timepoint,
    y = total_pah,
    color = treatment,
    group = interaction(treatment, replicate)
  )
) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2.5) +
  facet_grid(
    sample_type ~ sediment_location,
    scales = "free_y"
  ) +
  theme_classic() +
  labs(
    x = "Time (days)",
    y = "Total PAH Concentration",
    color = "Treatment",
    title = "Total PAH Concentration Through Time"
  )
total_summary <- total_pah %>%
  group_by(
    sample_type,
    sediment_location,
    treatment,
    timepoint
  ) %>%
  summarise(
    mean_total_pah = mean(total_pah, na.rm = TRUE),
    sd_total_pah = sd(total_pah, na.rm = TRUE),
    n = sum(!is.na(total_pah)),
    se_total_pah = sd_total_pah / sqrt(n),
    .groups = "drop"
  )
ggplot(
  total_summary,
  aes(
    x = timepoint,
    y = mean_total_pah,
    color = treatment,
    group = treatment
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = mean_total_pah - se_total_pah,
      ymax = mean_total_pah + se_total_pah
    ),
    width = 1
  ) +
  facet_grid(
    sample_type ~ sediment_location,
    scales = "free_y"
  ) +
  theme_classic() +
  labs(
    x = "Time (days)",
    y = "Mean Total PAH Concentration ± SE",
    color = "Treatment",
    title = "Total PAH Concentration Through Time"
  )
ggplot(
  pah_joined,
  aes(
    x = factor(timepoint),
    y = concentration,
    color = treatment
  )
) +
  geom_boxplot(
    outlier.shape = NA
  ) +
  geom_jitter(
    position = position_jitterdodge(
      jitter.width = 0.1,
      dodge.width = 0.75
    ),
    alpha = 0.5
  ) +
  facet_wrap(
    ~ molecule_name + sample_type,
    scales = "free_y"
  ) +
  theme_classic() +
  labs(
    x = "Time (days)",
    y = "PAH Concentration",
    color = "Treatment",
    title = "Individual PAHs by Sample Type"
  )
sediment_pah <- pah_joined %>%
  filter(sample_type == "Sediment Slurry")

dissolved_pah <- pah_joined %>%
  filter(sample_type == "Dissolved Capsule")
sediment_total <- total_pah %>%
  filter(sample_type == "Sediment Slurry")

dissolved_total <- total_pah %>%
  filter(sample_type == "Dissolved Capsule")
ggplot(
  sediment_total,
  aes(
    x = timepoint,
    y = total_pah,
    color = treatment,
    group = interaction(treatment, replicate)
  )
) +
  geom_line(alpha = 0.5) +
  geom_point(size = 3) +
  facet_wrap(~ sediment_location) +
  theme_classic() +
  labs(
    x = "Time (days)",
    y = "Total PAH Concentration",
    color = "Treatment",
    title = "Total PAHs in Sediment Slurry"
  )
ggplot(
  dissolved_total,
  aes(
    x = timepoint,
    y = total_pah,
    color = treatment,
    group = interaction(treatment, replicate)
  )
) +
  geom_line(alpha = 0.5) +
  geom_point(size = 3) +
  facet_wrap(~ sediment_location) +
  theme_classic() +
  labs(
    x = "Time (days)",
    y = "Total PAH Concentration",
    color = "Treatment",
    title = "Total PAHs in Dissolved Capsule Samples"
  )
## multi panel by individual PAH 
pah_joined %>%
  filter(!is.na(sample_type)) %>%
  ggplot(
    aes(
      x = factor(timepoint),
      y = concentration,
      color = treatment
    )
  ) +
  geom_boxplot(
    outlier.shape = NA
  ) +
  geom_jitter(
    position = position_jitterdodge(
      jitter.width = 0.1,
      dodge.width = 0.75
    ),
    alpha = 0.5
  ) +
  facet_wrap(
    ~ molecule_name + sample_type,
    scales = "free_y"
  ) +
  theme_classic() +
  labs(
    x = "Time (days)",
    y = "PAH Concentration",
    color = "Treatment",
    title = "Individual PAHs by Sample Type"
  )
#break down individual PAHs into slurry and dissolved capsule plots
dissolved_pah <- pah_joined %>%
  filter(sample_type == "Dissolved Capsule")

ggplot(
  dissolved_pah,
  aes(
    x = factor(timepoint),
    y = concentration,
    color = treatment
  )
) +
  geom_boxplot(
    outlier.shape = NA
  ) +
  geom_jitter(
    position = position_jitterdodge(
      jitter.width = 0.1,
      dodge.width = 0.75
    ),
    alpha = 0.5
  ) +
  facet_wrap(
    ~ molecule_name,
    scales = "free_y"
  ) +
  theme_classic() +
  labs(
    x = "Time (days)",
    y = "PAH Concentration",
    color = "Treatment",
    title = "Individual PAHs in Dissolved Capsule Samples"
  ) +
  theme(
    strip.text = element_text(size = 8)
  )

sediment_pah <- pah_joined %>%
  filter(sample_type == "Sediment Slurry")

ggplot(
  sediment_pah,
  aes(
    x = factor(timepoint),
    y = concentration,
    color = treatment
  )
) +
  geom_boxplot(
    outlier.shape = NA
  ) +
  geom_jitter(
    position = position_jitterdodge(
      jitter.width = 0.1,
      dodge.width = 0.75
    ),
    alpha = 0.5
  ) +
  facet_wrap(
    ~ molecule_name,
    scales = "free_y"
  ) +
  theme_classic() +
  labs(
    x = "Time (days)",
    y = "PAH Concentration",
    color = "Treatment",
    title = "Individual PAHs in Sediment Slurry Samples"
  ) +
  theme(
    strip.text = element_text(size = 8)
  )
pah_filtered <- pah_joined %>%
  filter(
    !is.na(sample_type),
    !str_detect(molecule_name, regex("d10|d12", ignore_case = TRUE))
  )
dissolved_pah <- pah_filtered %>%
  filter(sample_type == "Dissolved Capsule")

sediment_pah <- pah_filtered %>%
  filter(sample_type == "Sediment Slurry")
sort(unique(pah_filtered$molecule_name))
pah_joined %>%
  filter(str_detect(molecule_name, regex("d10|d12", ignore_case = TRUE))) %>%
  distinct(molecule_name)
ggplot(
  sediment_pah,
  aes(
    x = factor(timepoint),
    y = concentration,
    color = treatment
  )
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    position = position_jitterdodge(
      jitter.width = 0.1,
      dodge.width = 0.75
    ),
    alpha = 0.5
  ) +
  facet_wrap(
    ~ molecule_name,
    scales = "free_y"
  ) +
  theme_classic() +
  labs(
    x = "Time (days)",
    y = "PAH Concentration",
    color = "Treatment",
    title = "Individual PAHs in Sediment Slurry Samples"
  )
ggplot(
  dissolved_pah,
  aes(
    x = factor(timepoint),
    y = concentration,
    color = treatment
  )
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    position = position_jitterdodge(
      jitter.width = 0.1,
      dodge.width = 0.75
    ),
    alpha = 0.5
  ) +
  facet_wrap(
    ~ molecule_name,
    scales = "free_y"
  ) +
  theme_classic() +
  labs(
    x = "Time (days)",
    y = "PAH Concentration",
    color = "Treatment",
    title = "Individual PAHs in Dissolved Capsule Samples"
  )
plot_pah_profile <- function(data, site, type) {
  
  # Filter to the site and sample type
  plot_data <- data %>%
    filter(
      sediment_location == site,
      sample_type == type
    )
  
  # Calculate mean and SE
  summary_data <- plot_data %>%
    group_by(
      molecule_name,
      timepoint,
      treatment
    ) %>%
    summarise(
      mean_concentration = mean(concentration, na.rm = TRUE),
      sd_concentration = sd(concentration, na.rm = TRUE),
      n = sum(!is.na(concentration)),
      se_concentration = sd_concentration / sqrt(n),
      .groups = "drop"
    )
  
  # Plot
  ggplot(
    summary_data,
    aes(
      x = timepoint,
      y = mean_concentration,
      color = treatment,
      group = treatment
    )
  ) +
    geom_line(
      linewidth = 0.8
    ) +
    geom_point(
      size = 2.5
    ) +
    geom_errorbar(
      aes(
        ymin = mean_concentration - se_concentration,
        ymax = mean_concentration + se_concentration
      ),
      width = 0.15
    ) +
    facet_wrap(
      ~ molecule_name,
      scales = "free_y",
      ncol = 4
    ) +
    theme_classic() +
    labs(
      x = "Time (days)",
      y = "Mean PAH Concentration ± SE",
      color = "Treatment",
      title = paste(site, "—", type)
    ) +
    theme(
      strip.text = element_text(size = 8),
      legend.position = "bottom"
    )
}
##MAINS
plot_pah_profile(
  pah_filtered,
  site = "Mains",
  type = "Sediment Slurry"
)
plot_pah_profile(
  pah_filtered,
  site = "Mains",
  type = "Dissolved Capsule"
)
##PESCARA
plot_pah_profile(
  pah_filtered,
  site = "Pescara",
  type = "Sediment Slurry"
)

plot_pah_profile(
  pah_filtered,
  site = "Pescara",
  type = "Dissolved Capsule"
)
## REPUBLIC
plot_pah_profile(
  pah_filtered,
  site = "Republic",
  type = "Sediment Slurry"
)

plot_pah_profile(
  pah_filtered,
  site = "Republic",
  type = "Dissolved Capsule"
)
## MAINS STERILE
plot_pah_profile(
  pah_filtered,
  site = "Mains Sterile",
  type = "Sediment Slurry"
)

plot_pah_profile(
  pah_filtered,
  site = "Mains Sterile",
  type = "Dissolved Capsule"
)
