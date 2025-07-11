library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(ggplot2)
library("writexl", )
library(here)

#load data. 
qpcr_files <- list(
  expK1 <- read_excel(here::here("data", "qPCR", "AMF-16S-k-strat-wk0-1-20250625.xlsx")), 
  expM1 <- read_excel(here::here("data", "qPCR", "AMF-16S-mix-wk0-1-20250626 -  Quantification Cq Results copy.xlsx")), 
  expR1 <- read_excel(here::here("data", "qPCR", "AMF-16S-r-strat-wk0-1-20250625 -  Quantification Cq Results copy.xlsx"))
)
qpcr_files <- imap(qpcr_files, ~ mutate(.x, plate_id = .y))
qpcr_all <- bind_rows(qpcr_files)
#lcean
qpcr_all <- qpcr_all %>% select(-target, -sample, -`biological set name`)

plate_files <- list(
  plate_K1 <- read_excel(here::here("data", "qPCR", "K1-platelayout.xlsx")), 
  plate_M1 <- read_excel(here::here("data", "qPCR", "M1-platelayout.xlsx")), 
  plate_R1 <- read_excel(here::here("data", "qPCR", "R1-platelayout.xlsx"))
)

process_plate <- function(plate) {
  # Rename the first column to 'row'
  names(plate)[1] <- "row"
  
  # Convert all values to character
  plate[] <- lapply(plate, as.character)
  
  # Reshape and create well IDs
  plate <- plate %>%
    pivot_longer(cols = !row, names_to = "column_num", values_to = "sample") %>%
    mutate(
      column_num = stringr::str_pad(column_num, width = 2, pad = "0"),
      well = paste0(row, column_num)
    ) %>%
    select(well, sample)
  
  return(plate)
}

processed_plates <- lapply(plate_files, process_plate)
all_plates <- bind_rows(processed_plates, .id = "plate_id")
# Convert column names to lowercase
names(all_plates) <- tolower(names(all_plates))
names(qpcr_all)   <- tolower(names(qpcr_all))
#coerce to character
qpcr_all$plate_id   <- as.character(qpcr_all$plate_id)
all_plates$plate_id <- as.character(all_plates$plate_id)

merged_qpcr <- left_join(qpcr_all, all_plates, by = c("plate_id", "well"))

sample_codes <- read_excel(here::here("data", "qPCR", "qPCR-sample-code.xlsx"))

qpcr_full <- left_join(merged_qpcr, sample_codes, by = "sample")
#clean
qpcr_full <- qpcr_full %>% select(-`set point`, -`well note`) %>% mutate(sample.type = fct_relevel(sample.type, "planktonic", "capsule", "supernatant"))

#add strain data
qpcr_full <- qpcr_full %>%
  mutate(strain = case_when(
    fluor == "Cy5"       & consortia == "r" ~ "p.resin",
    fluor == "FAM"       & consortia == "r" ~ "a.venet",
    fluor == "HEX"       & consortia == "r" ~ "p.putida",
    
    fluor == "Cy5"       & consortia == "m" ~ "p.resin",
    fluor == "FAM"       & consortia == "m" ~ "n.penta",
    fluor == "Texas Red" & consortia == "m" ~ "sphingo.sp",
    fluor == "HEX"       & consortia == "m" ~ "p.putida",
    
    fluor == "Cy5"       & consortia == "k" ~ "a.faecalis",
    fluor == "FAM"       & consortia == "k" ~ "n.penta",
    fluor == "Texas Red" & consortia == "k" ~ "sphingo.sp",
    
    TRUE ~ NA_character_
  ))

#check for mismatches
mismatch<- anti_join(merged_qpcr, sample_codes, by = "sample") #just contains standards and NTC, so that makes sense.

#summarize technical replicates
qpcr_avg <- qpcr_full %>%
  group_by(sample, rep, day, consortia, treatment, sample.type, sample.vol.ml, elution.vol.ul, dna.ng.ul, strain) %>%   # add other grouping vars as needed
  summarise(
    mean_sq = mean(`starting quantity (sq)`, na.rm = TRUE),
    sd_sq = sd(`starting quantity (sq)`, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

qpcr_avg <- qpcr_avg %>%
  mutate(
    gene_copies_per_mL = (mean_sq*elution.vol.ul/sample.vol.ml)
  )

#summarize biological triplicates
qpcr_summary <- qpcr_avg %>%
  group_by(day, consortia, treatment, sample.type, sample.vol.ml, elution.vol.ul, strain) %>%   # add other grouping vars if needed
  summarise(
    mean_gene_copies = mean(gene_copies_per_mL, na.rm = TRUE),
    se_gene_copies = sd(gene_copies_per_mL, na.rm = TRUE) / sqrt(n()),
    n = n()
  ) %>%
  ungroup() %>%
  filter(!is.na(consortia))


#visualize with plot
library(ggplot2)

ggplot(qpcr_summary, aes(x = day, y = mean_gene_copies, color = sample.type)) +
  geom_line(aes(group = sample.type), size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_gene_copies - se_gene_copies, ymax = mean_gene_copies + se_gene_copies), width = 0.2) +
  scale_y_log10(labels = scales::comma_format()) +
  labs(
    title = "Mean Gene Copies per Sample Volume Over Time (with SE)",
    x = "Time (days)",
    y = "Gene Copies per Sample Volume (log10 scale)",
    color = "Treatment"
  ) +
  theme_minimal()+
  facet_wrap(~consortia)

##INDIVIDUAL STRAINS
#summarize technical replicates
qpcr_avg_ind <- qpcr_full %>%
  group_by(sample, fluor, rep, day, consortia, treatment, sample.type, sample.vol.ml, elution.vol.ul, dna.ng.ul, strain) %>%   # add other grouping vars as needed
  summarise(
    mean_sq = mean(`starting quantity (sq)`, na.rm = TRUE),
    sd_sq = sd(`starting quantity (sq)`, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

qpcr_avg_ind <- qpcr_avg_ind %>%
  mutate(
    gene_copies_per_mL = (mean_sq*elution.vol.ul/sample.vol.ml)
  ) %>%
  filter(!is.na(consortia))

#summarize biological triplicates
qpcr_summary_ind <- qpcr_avg_ind %>%
  group_by(day, fluor, consortia, treatment, sample.type, sample.vol.ml, elution.vol.ul, strain) %>%   # add other grouping vars if needed
  summarise(
    mean_gene_copies = mean(gene_copies_per_mL, na.rm = TRUE),
    se_gene_copies = sd(gene_copies_per_mL, na.rm = TRUE) / sqrt(n()),
    n = n()
  ) %>%
  ungroup() %>%
  filter(!is.na(consortia))


#visualize with plot

ggplot(qpcr_summary_ind, aes(x = day, y = mean_gene_copies, color = strain)) +
  geom_line(aes(group = strain), size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_gene_copies - se_gene_copies, ymax = mean_gene_copies + se_gene_copies), width = 0.2) +
  scale_y_log10(labels = scales::comma_format()) +
  labs(
    title = "Mean Gene Copies per mL",
    x = "Time (days)",
    y = "Gene Copies per mL",
    color = "Treatment"
  ) +
  theme_minimal()+
  facet_wrap(~consortia + sample.type, ncol = 3)

ggplot(qpcr_summary_ind, aes(x = day, y = mean_gene_copies, color = strain)) +
  geom_line(aes(group = strain), size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_gene_copies - se_gene_copies, ymax = mean_gene_copies + se_gene_copies), width = 0.2) +
  scale_y_continuous(transform = "log10", limits = c(NA, 1e10), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  labs(
    title = "Mean Gene Copies per mL",
    x = "Time (days)",
    y = "Gene Copies per mL",
    color = "Treatment"
  ) +
  theme_minimal()+
  facet_wrap(~consortia + sample.type, ncol = 3)

#stat_summary
pd <- position_dodge(width = 0.4)  # define dodge position once

ggplot(qpcr_avg_ind, aes(x = day, y = gene_copies_per_mL, color = strain)) +
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
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.2,
    position = pd
  ) +
  scale_y_log10(labels = scales::scientific) +  
  facet_grid(consortia ~ sample.type) +
  labs(
    title = "Mean Gene Copies per mL",
    y = "Gene Copies per mL (log scale)",
    x = "Time (days)",
    color = "Treatment"
  ) +
  theme_minimal()


