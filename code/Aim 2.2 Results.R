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
  expR1 <- read_excel(here::here("data", "qPCR", "AMF-16S-r-strat-wk0-1-20250625 -  Quantification Cq Results copy.xlsx")), 
  expK2 <- read_excel(here::here("data", "qPCR", "AMF-16S-k-strat-wk2-3-20250806-QuantificationCqResultscopy.xlsx")),
  expM2 <- read_excel(here::here("data", "qPCR", "AMF-16S-mix-strat-wk2-3-redo-20250813 -  Quantification Cq Results copy.xlsx")),
  expR2 <- read_excel(here::here("data", "qPCR", "AMF-16S-r-strat-wk2-3-20250801 -  Quantification Cq Results copy.xlsx")),
  expK3 <- read_excel(here::here("data", "qPCR", "AMF-16S-k-strat-wk6-20250812 -  Quantification Cq Results copy.xlsx")),
  expM3 <- read_excel(here::here("data", "qPCR", "AMF-16S-mix-duplex-wk1-2-20250814 -  Quantification Cq Results copy.xlsx")),
  expM4 <- read_excel(here::here("data", "qPCR", "AMF-16S-mix-duplex-2-wk3-6-20250814 -  Quantification Cq Results copy.xlsx")),
  expR3 <- read_excel(here::here("data", "qPCR", "AMF-16S-r-strat-wk6-20250811 -  Quantification Cq Results copy.xlsx")), 
  expNP1 <- read_excel(here::here("data", "qPCR", "AMF-16S-k-strat-npenta-20250822 -  Quantification Cq Results copy.xlsx")), 
  expM5 <- read_excel(here::here("data", "qPCR", "AMF-16S-mix-strat-wk6-20250822 -  Quantification Cq Results copy.xlsx"))
)
qpcr_files <- imap(qpcr_files, ~ mutate(.x, plate_id = .y))
qpcr_all <- bind_rows(qpcr_files)
#clean
qpcr_all <- qpcr_all %>% dplyr::select(-Target, -Sample, -`Biological Set Name`)

plate_files <- list(
  plate_K1 <- read_excel(here::here("data", "qPCR", "K1-platelayout.xlsx")), #1
  plate_M1 <- read_excel(here::here("data", "qPCR", "M1-platelayout.xlsx")), #2
  plate_R1 <- read_excel(here::here("data", "qPCR", "R1-platelayout.xlsx")), #3
  
  plate_K2 <- read_excel(here::here("data", "qPCR", "K2-platelayout.xlsx")), #4
  plate_M2 <- read_excel(here::here("data", "qPCR", "M2-platelayout.xlsx")), #5
  plate_R2 <- read_excel(here::here("data", "qPCR", "R2-platelayout.xlsx")), #6
  
  plate_K3 <- read_excel(here::here("data", "qPCR", "K3-platelayout.xlsx")), #7
  plate_M3 <- read_excel(here::here("data", "qPCR", "M3-platelayout.xlsx")), #8
  plate_M4 <- read_excel(here::here("data", "qPCR", "M4-platelayout.xlsx")), #9
  plate_R3 <- read_excel(here::here("data", "qPCR", "R3-platelayout.xlsx")),  #10
  plate_NP1 <- read_excel(here::here("data", "qPCR", "NP1-platelayout.xlsx")), #11
  plate_M5 <- read_excel(here::here("data", "qPCR", "M5-platelayout.xlsx")) #12
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
    dplyr::select(well, sample)
  
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

sample_codes <- read_excel(here::here("data", "qPCR", "qPCR-sample-code.xlsx"))

merged_qpcr <- left_join(qpcr_all, all_plates, by = c("plate_id", "well"))
#check for mismatches
mismatch<- anti_join(merged_qpcr, sample_codes, by = "sample") #just contains standards and NTC, so that makes sense; and the throwaway diluted wells from plates 4 and 5
#remove samples with poor quality
# Preview rows that would be removed
to_remove <- merged_qpcr %>%
  filter(
    # remove the 6 samples from plate 2
    !(plate_id == 2 & sample %in% c("M-CA-7", "M-CB-7", "M-CC-7", 
                                    "M-FA-7", "M-FB-7", "M-FC-7")),
    
    # remove FAM + Texas Red from plate 8
    !(plate_id == 5 & fluor %in% c("FAM", "Texas Red")),
    
    # remove wells D12, E12, F12 that are Texas Red from plate 2
    !(plate_id == 2 & fluor == "Texas Red" & well %in% c("D12", "E12", "F12"))
  )

# Show them
to_remove

#remove and clean
clean_qpcr <- merged_qpcr %>%
  filter(
    # remove the 6 samples from plate 2
    !(plate_id == 2 & sample %in% c("M-CA-7", "M-CB-7", "M-CC-7", 
                                    "M-FA-7", "M-FB-7", "M-FC-7")),
    
    # remove FAM + Texas Red from plate 5
    !(plate_id == 5 & fluor %in% c("FAM", "Texas Red")),
    
    # remove wells D12, E12, F12 that are Texas Red from plate 2
    !(plate_id == 2 & fluor == "Texas Red" & well %in% c("D12", "E12", "F12")), 
    
    # remove well D04 that is Texas Red from plate 8
    !(plate_id == 8 & fluor == "Texas Red" & well == "D04"), 
    
    # remove wells A05, A09, D06 that are Texas Red from plate 9
    !(plate_id == 9 & fluor == "Texas Red" & well %in% c("A05", "A09", "D06")), 
    
    # NEW: remove FAM + Texas Red from plate 12
    !(plate_id == 12 & fluor %in% c("FAM", "Texas Red")),
    
    # NEW: remove FAM for K-CA-42, K-CB-42, K-CC-42 on plate 7
    !(plate_id == 7 & fluor == "FAM" & sample %in% c("K-CA-42", "K-CB-42", "K-CC-42")),
    
    # NEW: remove FAM for M-CA-42, M-CB-42, M-CC-42 on plate 9
    !(plate_id == 9 & fluor == "FAM" & sample %in% c("M-CA-42", "M-CB-42", "M-CC-42"))
  )


qpcr_full <- left_join(clean_qpcr, sample_codes, by = "sample")
#clean
qpcr_full <- qpcr_full %>% dplyr::select(-`set point`, -`well note`) %>% mutate(sample.type = fct_relevel(sample.type, "planktonic", "capsule", "supernatant"))


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

##jittered
pd  <- position_dodge(width = 1)  # for lines & error bars
pjd <- position_jitterdodge(jitter.width = .3, jitter.height = 0, dodge.width = 1)  # for points
my_labels <- list(
  treatment = c(
    "free" = "Free",
    "encapsulated" = "Encapsulated"
  ),
  consortia = c(
    "k" = "K-Strat",
    "m" = "Mixed",
    "r" = "R-Strat"
  ), 
  sample.type = c(
    "planktonic" = "Planktonic", 
    "capsule" = "Capsule", 
    "supernatant" = "Supernatant"
  )
)
strain_labels <- c(
  "a.venet"   = "italic('A. venetianus')",
  "a.faecalis"= "italic('A. faecalis')",
  "p.resin"   = "italic('P. resinovorans')",
  "sphingo.sp"= "italic('Sphingomonas sp.')",
  "n.penta"   = "italic('N. pentaromativorans')",
  "p.putida"  = "italic('P. putida')"
)
#colors for strains
# R-strategists (warm)
r_colors <- c(
  "a.venet"  = "#E64B35",  # vermilion
  "p.resin"  = "#F58634",  # coral
  "p.putida" = "#F2CD37"   # gold
)

# K-strategists (cool)
k_colors <- c(
  "a.faecalis" = "#4DBBD5FF", # cyan/blue
  "sphingo.sp" = "#00A087FF", # teal
  "n.penta"    = "#3C5488FF"  # deep blue
)

# combine palette
strain_colors <- c(k_colors, r_colors)


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
    position = pjd,           # jittered points
    size = 2
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.2,
    position = pd
  ) +
  scale_y_log10(labels = scales::scientific) +  
  facet_grid(consortia ~ sample.type, labeller = labeller(
    sample.type = as_labeller(my_labels$sample.type), 
    consortia = as_labeller(my_labels$consortia)
  )) +
  labs(
    title = "Mean Gene Copies per mL",
    y = "Gene Copies per mL (log scale)",
    x = "Time (days)",
    color = "Treatment"
  ) +
  theme_classic()+
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA)
  )+ scale_x_continuous(breaks = c(0, 7, 14, 21, 42))+
  scale_color_manual(values = strain_colors, 
                       labels = function(x) parse(text = strain_labels[x]))
  

##MASS BALANCE FOR QPCR DATA#

library(dplyr)
library(tidyr)

df <- qpcr_avg_ind

# standardize sample.type values so 'planktonic' maps to 'supernatant'
df <- df %>%
  rename(sample_type = sample.type,
         gc_per_mL = gene_copies_per_mL) %>%
  mutate(sample_type = tolower(sample_type),
         sample_type = case_when(
           sample_type %in% c("planktonic", "plank", "liquid") ~ "supernatant",
           sample_type %in% c("supernatant", "super") ~ "supernatant",
           sample_type %in% c("capsule", "caps") ~ "capsule",
           TRUE ~ sample_type
         ))

# pivot: include strain so mass-balance is per-strain
wide <- df %>%
  pivot_wider(
    id_cols = c(consortia, day, rep, treatment, strain),
    names_from = sample_type,
    values_from = c(gc_per_mL, sd_sq, n),
    values_fn = mean,
    values_fill = NA
  )

# normalize column names
wide <- wide %>%
  rename(gc_super = gc_per_mL_supernatant,
         gc_capsule = gc_per_mL_capsule,
         sd2_super = sd_sq_supernatant,
         sd2_capsule = sd_sq_capsule,
         n_super = n_supernatant,
         n_capsule = n_capsule)

# assign volumes: any reactor labelled encapsulated -> V_caps = 2, else 0
wide <- wide %>%
  mutate(
    is_encapsulated = tolower(treatment) == "encapsulated",
    V_super = 10,
    V_caps  = if_else(is_encapsulated, 2, 0),
    V_total = V_super + V_caps
  )

# compute totals per strain
wide <- wide %>%
  mutate(
    gc_super = as.numeric(gc_super),
    gc_capsule = as.numeric(gc_capsule),
    
    total_copies = case_when(
      !is.na(gc_super) & !is.na(gc_capsule) ~ gc_super * V_super + gc_capsule * V_caps,
      !is.na(gc_super) &  is.na(gc_capsule) ~ gc_super * V_super,
      is.na(gc_super)  & !is.na(gc_capsule) ~ gc_capsule * V_caps,
      TRUE ~ NA_real_
    ),
    
    gc_total_per_mL = if_else(!is.na(total_copies), total_copies / V_total, NA_real_),
    
    pct_in_capsule = case_when(
      !is.na(total_copies) & !is.na(gc_capsule) ~ (gc_capsule * V_caps) / total_copies * 100,
      TRUE ~ NA_real_
    )
  )

# Inspect a few planktonic rows to confirm gc_total_per_mL is present
wide %>% filter(tolower(treatment) == "free" | tolower(treatment) == "planktonic") %>%
  select(consortia, day, rep, treatment, strain, gc_super, gc_capsule, V_total, total_copies, gc_total_per_mL) %>%
  head(20) %>% print(n = 20)

# Save
#write.csv(wide, "qpcr_massbalance_including_planktonic.csv", row.names = FALSE)
pd  <- position_dodge(width = 1)  # for lines & error bars
pjd <- position_jitterdodge(jitter.width = .3, jitter.height = 0, dodge.width = 1)  # for points
my_labels <- list(
  treatment = c(
    "free" = "Free",
    "encapsulated" = "Encapsulated"
  ),
  consortia = c(
    "k" = "K-Strat",
    "m" = "Mixed",
    "r" = "R-Strat"
  )
)
ggplot(wide, aes(x = day, y = gc_total_per_mL, color = strain)) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = strain),
    position = pd
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    position = pjd,           # jittered points
    size = 2
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.2,
    position = pd
  ) +
  scale_y_log10(labels = scales::scientific) +  
  facet_grid(consortia ~ treatment, labeller = labeller(
    treatment = as_labeller(my_labels$treatment), 
    consortia = as_labeller(my_labels$consortia)
  )) +
  labs(
    title = "Mass Balance - Gene Copies per mL",
    y = "Gene Copies per mL (log scale)",
    x = "Time (days)",
    color = "Treatment"
  ) +
  theme_classic()+
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA)
  )+ scale_x_continuous(breaks = c(0, 7, 14, 21, 42))+
  scale_color_manual(values = strain_colors, 
                     labels = function(x) parse(text = strain_labels[x]))

