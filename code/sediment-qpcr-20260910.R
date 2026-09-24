library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(ggplot2)
library("writexl", )
library(here)

#load data. 
qpcr_files <- list(
  pcr1 <- read_excel(here::here("data", "sediment-qPCR", "regression", "AMF-16S-mix-sed1-MC-MCS-20251204 -  Quantification Cq Results.xlsx")), 
  pcr2 <- read_excel(here::here("data", "sediment-qPCR", "regression", "AMF-16S-mix-sed2-REP-PC-20251205 -  Quantification Cq Results.xlsx")), 
  pcr3 <- read_excel(here::here("data", "sediment-qPCR", "regression", "AMF-16S-mix-sed3-dissolved-20251208 -  Quantification Cq Results.xlsx")), 
  pcr4 <- read_excel(here::here("data", "sediment-qPCR", "regression", "AMF-16S-mix-sed4-MC-MCS-20251208 -  Quantification Cq Results.xlsx")),
  pcr5 <- read_excel(here::here("data", "sediment-qPCR", "regression", "AMF-16S-mix-sed5-REDO-REP-PC-20251218 -  Quantification Cq Results.xlsx")),
  pcr6 <- read_excel(here::here("data", "sediment-qPCR", "regression", "AMF-16S-mix-sed6-dissolved-20251209 -  Quantification Cq Results.xlsx")),
  pcr7 <- read_excel(here::here("data", "sediment-qPCR", "regression", "AMF-16S-mix-sed7-MC-MCS-20251210 -  Quantification Cq Results.xlsx")),
  pcr8 <- read_excel(here::here("data", "sediment-qPCR", "regression", "AMF-16S-mix-sed8-REP-PC-20251215 -  Quantification Cq Results.xlsx")),
  pcr9 <- read_excel(here::here("data", "sediment-qPCR", "regression", "AMF-16S-mix-sed9-dissolved-20251217 -  Quantification Cq Results.xlsx"))
  
)
qpcr_files <- imap(qpcr_files, ~ mutate(.x, plate_id = .y))
qpcr_all <- bind_rows(qpcr_files)
#clean
qpcr_all <- qpcr_all %>% dplyr::select(-Target, -Sample, -`Biological Set Name`)

plate_files <- list(
  plate_1 <- read_excel(here::here("data", "sediment-qPCR", "sed1-platelayout-std8.xlsx")) %>% mutate(day = 0), 
  plate_2 <- read_excel(here::here("data", "sediment-qPCR", "sed2-platelayout-std8.xlsx")) %>% mutate(day = 0), 
  plate_3 <- read_excel(here::here("data", "sediment-qPCR", "sed3-platelayout.xlsx")) %>% mutate(day = 0), 
  
  plate_4 <- read_excel(here::here("data", "sediment-qPCR", "sed1-platelayout.xlsx")) %>% mutate(day = 14), 
  plate_5 <- read_excel(here::here("data", "sediment-qPCR", "sed2-platelayout.xlsx")) %>% mutate(day = 14), 
  plate_6 <- read_excel(here::here("data", "sediment-qPCR", "sed3-platelayout.xlsx")) %>% mutate(day = 14), 
  
  plate_7 <- read_excel(here::here("data", "sediment-qPCR", "sed1-platelayout.xlsx")) %>% mutate(day = 42), 
  plate_8 <- read_excel(here::here("data", "sediment-qPCR", "sed2-platelayout.xlsx")) %>% mutate(day = 42), 
  plate_9 <- read_excel(here::here("data", "sediment-qPCR", "sed3-platelayout.xlsx")) %>% mutate(day = 42) 
  
)

## 50 uL elution volume according to lab notebook 

process_plate <- function(plate) {
  # Rename the first column to 'row'
  names(plate)[1] <- "row"
  
  # Convert all values to character
  plate[] <- lapply(plate, as.character)
  
  # Reshape and create well IDs
  plate <- plate %>%
    pivot_longer(cols = -c(row,day), names_to = "column_num", values_to = "sample") %>%
    mutate(
      column_num = stringr::str_pad(column_num, width = 2, pad = "0"),
      well = paste0(row, column_num)
    ) %>%
    dplyr::select(well, sample, day)
  
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

#sample_codes <- read_excel(here::here("data", "qPCR", "qPCR-sample-code.xlsx"))

merged_qpcr <- left_join(qpcr_all, all_plates, by = c("plate_id", "well"))
#check for mismatches
#mismatch<- anti_join(merged_qpcr, sample_codes, by = "sample") #just contains standards and NTC, so that makes sense; and the throwaway diluted wells from plates 4 and 5
#remove samples with poor quality
# Preview rows that would be removed
# to_remove <- merged_qpcr %>%
#   filter(
#     # remove the 6 samples from plate 2
#     !(plate_id == 2 & sample %in% c("M-CA-7", "M-CB-7", "M-CC-7", 
#                                     "M-FA-7", "M-FB-7", "M-FC-7")),
#     
#     # remove FAM + Texas Red from plate 8
#     !(plate_id == 5 & fluor %in% c("FAM", "Texas Red")),
#     
#     # remove wells D12, E12, F12 that are Texas Red from plate 2
#     !(plate_id == 2 & fluor == "Texas Red" & well %in% c("D12", "E12", "F12"))
#   )

# Show them
#to_remove

#remove and clean
clean_qpcr <- merged_qpcr
# clean_qpcr <- merged_qpcr %>%
#   filter(
#     # remove the 6 samples from plate 2 8/7/26 updated this to add FAM and Texas Red qualifier bc pputida and presin data was missing
#     !(plate_id == 2 & fluor %in% c("FAM", "Texas Red") & sample %in% c("M-CA-7", "M-CB-7", "M-CC-7", 
#                                                                        "M-FA-7", "M-FB-7", "M-FC-7")),
#     
#     # remove FAM + Texas Red from plate 5
#     !(plate_id == 5 & fluor %in% c("FAM", "Texas Red")),
#     
#     # remove wells D12, E12, F12 that are Texas Red from plate 2
#     !(plate_id == 2 & fluor == "Texas Red" & well %in% c("D12", "E12", "F12")), 
#     
#     # remove well D04 that is Texas Red from plate 8
#     !(plate_id == 8 & fluor == "Texas Red" & well == "D04"), 
#     
#     # remove wells A05, A09, D06 that are Texas Red from plate 9
#     !(plate_id == 9 & fluor == "Texas Red" & well %in% c("A05", "A09", "D06")), 
#     
#     # NEW: remove FAM + Texas Red from plate 12
#     !(plate_id == 12 & fluor %in% c("FAM", "Texas Red")),
#     
#     # NEW: remove FAM for K-CA-42, K-CB-42, K-CC-42 on plate 7
#     !(plate_id == 7 & fluor == "FAM" & sample %in% c("K-CA-42", "K-CB-42", "K-CC-42")),
#     
#     # NEW: remove FAM for M-CA-42, M-CB-42, M-CC-42 on plate 9
#     !(plate_id == 9 & fluor == "FAM" & sample %in% c("M-CA-42", "M-CB-42", "M-CC-42"))
#   )


#qpcr_full <- left_join(clean_qpcr, sample_codes, by = "sample")
#clean
qpcr_full <- clean_qpcr %>% dplyr::select(-`set point`, -`well note`) 
unique(qpcr_full$sample)
#use names to add treatment metadata
qpcr_full <- qpcr_full %>%
  mutate(
    sediment_code = stringr::str_extract(sample, "^[1-4](?=-)"),
    
    sediment = case_when(
      sediment_code == "1" ~ "MC",
      sediment_code == "2" ~ "MCS",
      sediment_code == "3" ~ "REP",
      sediment_code == "4" ~ "PC",
      TRUE ~ NA_character_
    ),
    
    treatment = stringr::str_match(
      sample, "^[1-4]-([A-Z])-"
    )[, 2],
    
    replicate = stringr::str_match(
      sample, "^[1-4]-[A-Z]-([A-C])"
    )[, 2],
    
    sample_type = case_when(
      stringr::str_detect(sample, "^[1-4].*-D$") ~ "dissolved capsule",
      stringr::str_detect(sample, "^[1-4]-") ~ "sediment slurry",
      TRUE ~ NA_character_
    )
  )


#add strain data
qpcr_full <- qpcr_full %>%
  mutate(strain = case_when(
    fluor == "Cy5" ~ "p.resin",
    fluor == "FAM" ~ "n.penta",
    fluor == "Texas Red" ~ "sphingo.sp",
    fluor == "HEX" ~ "p.putida",
    
    TRUE ~ NA_character_
  ))

saveRDS(
  qpcr_full,
  here("data", "qpcr_full.rds")
)


#summarize technical replicates
qpcr_avg <- qpcr_full %>%
  group_by(sample, day, replicate, sediment, treatment, strain, sample_type) %>%   # add other grouping vars as needed
  summarise(
    mean_sq = mean(`starting quantity (sq)`, na.rm = TRUE),
    sd_sq = sd(`starting quantity (sq)`, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

qpcr_avg <- qpcr_avg %>%
  mutate(
    sample_volume_mL = case_when(
      sample_type == "sediment slurry" ~ 1.45,
      sample_type == "dissolved capsule" ~ 0.2,
      TRUE ~ NA_real_
    ),
    
    gene_copies_per_mL = mean_sq * 50 / sample_volume_mL
  )

qpcr_lod <- qpcr_avg %>%
  mutate(
    LOD = case_when(
      strain %in% c("n.penta", "p.resin") ~ 500,
      strain %in% c("p.putida", "sphingo.sp") ~ 50,
      TRUE ~ NA_real_
    ),
    
    below_detection = is.na(mean_sq),
    
    mean_sq_lod = if_else(
      below_detection,
      LOD,
      mean_sq
    ),
    
    gene_copies_per_mL_lod = mean_sq_lod * 50 / sample_volume_mL,
    
    LOD_per_mL = LOD * 50 / sample_volume_mL
  )

# 1. Assign LOD and replace nondetects BEFORE averaging technical replicates
qpcr_lod_full <- qpcr_full %>%
  mutate(
    LOD = case_when(
      strain %in% c("n.penta", "p.resin") ~ 500,
      strain %in% c("p.putida", "sphingo.sp") ~ 50,
      TRUE ~ NA_real_
    ),
    
    # Flag original nondetects
    below_detection = is.na(`starting quantity (sq)`),
    
    # Replace each nondetect with strain-specific LOD
    sq_lod = if_else(
      below_detection,
      LOD,
      `starting quantity (sq)`
    )
  )
# 2. Consolidate technical replicates AFTER LOD substitution
qpcr_avg_lod <- qpcr_lod_full %>%
  group_by(
    sample,
    day,
    replicate,
    sediment,
    treatment,
    strain,
    sample_type
  ) %>%
  summarise(
    mean_sq = mean(sq_lod, na.rm = TRUE),
    sd_sq = sd(sq_lod, na.rm = TRUE),
    n = n(),
    
    # Useful QC information
    n_detected = sum(!below_detection),
    n_nondetect = sum(below_detection),
    
    .groups = "drop"
  )
# 3. Convert technical-replicate mean to copies/mL
qpcr_avg_lod <- qpcr_avg_lod %>%
  mutate(
    LOD = case_when(
      strain %in% c("n.penta", "p.resin") ~ 500,
      strain %in% c("p.putida", "sphingo.sp") ~ 50,
      TRUE ~ NA_real_
    ),
    
    sample_volume_mL = case_when(
      sample_type == "sediment slurry" ~ 1.45,
      sample_type == "dissolved capsule" ~ 0.2,
      TRUE ~ NA_real_
    ),
    
    gene_copies_per_mL = mean_sq * 50 / sample_volume_mL,
    
    LOD_per_mL = LOD * 50 / sample_volume_mL
  )
#fix false positives
false_positives <- tribble(
  ~sediment, ~strain, ~treatment, ~day,
  "MC",      "n.penta",   "E",       42,
  "MC",      "n.penta",   "F",       42,
  "MC",      "n.penta",   "N",       14,
  "MC",      "n.penta",   "S",       14,
  "MC",      "p.putida",  "N",       14,
  "MC",      "p.putida",  "N",       42,
  "MC",      "sphingo.sp","F",        0,
  "MC",      "sphingo.sp","F",       14,
  "MC",      "sphingo.sp","S",       14,
  "REP",     "n.penta",   "E",       14,
  "REP",     "p.resin",   "N",       42,
  "REP",     "n.penta",   "N",       42
) 
false_positives$day <- as.character(false_positives$day)
qpcr_lod_full <- qpcr_full %>%
  
  # Flag combinations manually classified as false positives
  left_join(
    false_positives %>%
      mutate(false_positive = TRUE),
    by = c("sediment", "strain", "treatment", "day")
  ) %>%
  
  mutate(
    false_positive = replace_na(false_positive, FALSE),
    
    # Strain-specific LOD
    LOD = case_when(
      strain %in% c("n.penta", "p.resin") ~ 500,
      strain %in% c("p.putida", "sphingo.sp") ~ 50,
      TRUE ~ NA_real_
    ),
    
    # Preserve whether the ORIGINAL result was a nondetect
    below_detection = is.na(`starting quantity (sq)`),
    
    # Replace BOTH nondetects and false positives with LOD
    sq_lod = case_when(
      false_positive ~ LOD,
      below_detection ~ LOD,
      TRUE ~ `starting quantity (sq)`
    )
  )
#summarize techncial repliactes
qpcr_avg_lod <- qpcr_lod_full %>%
  group_by(
    sample,
    day,
    replicate,
    sediment,
    treatment,
    strain,
    sample_type
  ) %>%
  summarise(
    mean_sq = mean(sq_lod, na.rm = TRUE),
    sd_sq = sd(sq_lod, na.rm = TRUE),
    n = n(),
    
    # QC information
    n_nondetect = sum(below_detection),
    n_false_positive = sum(false_positive),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    sample_volume_mL = case_when(
      sample_type == "sediment slurry" ~ 1.45,
      sample_type == "dissolved capsule" ~ 0.2,
      TRUE ~ NA_real_
    ),
    
    gene_copies_per_mL = mean_sq * 50 / sample_volume_mL
  )

#summarize biological triplicates
qpcr_summary <- qpcr_avg %>%
  group_by(day, sediment, treatment, sample_type, strain) %>%   # add other grouping vars if needed
  summarise(
    mean_gene_copies = mean(gene_copies_per_mL, na.rm = TRUE),
    se_gene_copies = sd(gene_copies_per_mL, na.rm = TRUE) / sqrt(n()),
    n = n()
  ) %>%
  ungroup() 


#visualize with plot
library(ggplot2)

ggplot(qpcr_summary, aes(x = day, y = mean_gene_copies, color = sample_type)) +
  geom_line(aes(group = sample_type), size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_gene_copies - se_gene_copies, ymax = mean_gene_copies + se_gene_copies), width = 0.2) +
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x), 
                labels = trans_format( "log10", math_format(10^.x) )) +
  labs(
    title = "Mean Gene Copies per Sample Volume Over Time (with SE)",
    x = "Time (days)",
    y = "Gene Copies per Sample Volume (log10 scale)",
    color = "Treatment"
  ) +
  theme_minimal()+
  facet_wrap(~sediment + treatment)

##INDIVIDUAL STRAINS
#summarize technical replicates
# qpcr_avg_ind <- qpcr_full %>%
#   group_by(sample, fluor, rep, day, sediment, treatment, sample_type, strain) %>%   # add other grouping vars as needed
#   summarise(
#     mean_sq = mean(`starting quantity (sq)`, na.rm = TRUE),
#     sd_sq = sd(`starting quantity (sq)`, na.rm = TRUE),
#     n = n()
#   ) %>%
#   ungroup()
# 
# qpcr_avg_ind <- qpcr_avg_ind %>%
#   mutate(
#     gene_copies_per_mL = (mean_sq*elution.vol.ul/sample.vol.ml)
#   ) %>%
#   filter(!is.na(consortia))
# 
# #summarize biological triplicates
# qpcr_summary_ind <- qpcr_avg_ind %>%
#   group_by(day, fluor, consortia, treatment, sample.type, sample.vol.ml, elution.vol.ul, strain) %>%   # add other grouping vars if needed
#   summarise(
#     mean_gene_copies = mean(gene_copies_per_mL, na.rm = TRUE),
#     se_gene_copies = sd(gene_copies_per_mL, na.rm = TRUE) / sqrt(n()),
#     n = n()
#   ) %>%
#   ungroup() %>%
#   filter(!is.na(consortia))


#visualize with plot

ggplot(qpcr_summary %>% 
         filter(
           !is.na(sediment),
           !is.na(treatment),
           !is.na(sample_type)
         )  , aes(x = day, y = mean_gene_copies, color = strain)) +
  geom_line(aes(group = strain), size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_gene_copies - se_gene_copies, ymax = mean_gene_copies + se_gene_copies), width = 0.2) +
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x), 
                labels = trans_format( "log10", math_format(10^.x) ))+
  labs(
    title = "Mean Gene Copies per mL",
    x = "Time (days)",
    y = "Gene Copies per mL",
    color = "Treatment"
  ) +
  theme_minimal()+
  facet_grid(sediment ~ treatment + sample_type)

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
pd <- position_dodge(width = 0.3)  # define dodge position once

ggplot(qpcr_avg %>%
         filter(
           !is.na(sediment),
           !is.na(treatment),
           !is.na(sample_type)
         ) , aes(x = day, y = gene_copies_per_mL, color = strain)) +
  geom_point( ##raw data points
    alpha = 0.4,
    size = 1.5,
    position = position_jitter(width = 0.1, height = 0) 
  ) +
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
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x), 
                labels = trans_format( "log10", math_format(10^.x) ))+ 
  facet_grid(sediment ~ treatment + sample_type) +
  labs(
    title = "Mean Gene Copies per mL",
    y = "Gene Copies per mL (log scale)",
    x = "Time (days)",
    color = "Treatment"
  ) +
  theme_minimal()

##LOD replacement
ggplot(qpcr_avg_lod %>%
         filter(
           !is.na(sediment),
           !is.na(treatment),
           !is.na(sample_type)
         ) , aes(x = day, y = gene_copies_per_mL, color = strain)) +
  geom_point( ##raw data points
    alpha = 0.4,
    size = 1.5,
    position = position_jitter(width = 0.1, height = 0) 
  ) +
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
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x), 
                labels = trans_format( "log10", math_format(10^.x) ))+ 
  facet_grid(sediment ~ treatment + sample_type) +
  labs(
    title = "Mean Gene Copies per mL",
    y = "Gene Copies per mL (log scale)",
    x = "Time (days)",
    color = "Treatment"
  ) +
  theme_pubr()

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
  "sphingo.sp"= "italic('Sphingobium sp.')",
  "n.penta"   = "italic('N. pentaromativorans')",
  "p.putida"  = "italic('P. putida')"
)

#original colors for strains
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

#new colors for strains
c("#61BEA4FF", "#B6E7E0FF", "#AA3F5DFF", "#DAA5ACFF", "#98A54FFF", "#2E92A2FF", "#FFB651FF", "#D85A44FF") #Moma Colors "Lupi"
r_colors <- c(
  "a.venet"  = "#D85A44FF",  # vermilion
  "p.resin"  = "#DAA5ACFF",  # coral
  "p.putida" = "#FFB651FF"   # gold
)

# K-strategists (cool)
k_colors <- c(
  "a.faecalis" = "#B6E7E0FF", # cyan/blue
  "sphingo.sp" = "#98A54FFF", # teal
  "n.penta"    = "#2E92A2FF"  # deep blue
)

# combine palette
# strain_colors <- c(k_colors, r_colors)
# 
# lod_50 <- qpcr_avg_ind %>% filter(mean_sq > 50) #filter out values outside of standard curve/limit of detection
# 
# ggplot(lod_50, aes(x = day, y = gene_copies_per_mL, color = strain, shape = strain)) +
#   stat_summary(
#     fun = mean,
#     geom = "line",
#     aes(group = strain),
#     position = pd
#   ) +
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     position = pjd,           # jittered points
#     size = 2
#   ) +
#   stat_summary(
#     fun.data = mean_se,
#     geom = "errorbar",
#     width = 0.2,
#     position = pd
#   ) +
#   scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x), 
#                 labels = trans_format( "log10", math_format(10^.x) ))+  
#   facet_grid(consortia ~ sample.type, labeller = labeller(
#     sample.type = as_labeller(my_labels$sample.type), 
#     consortia = as_labeller(my_labels$consortia)
#   )) +
#   labs(
#     title = "Mean Gene Copies per mL",
#     y = "Gene Copies per mL (log scale)",
#     x = "Time (days)"
#   ) +
#   theme_pubr()+
#   theme(
#     strip.text = element_text(size = 12, face = "italic")
#     #strip.background = element_rect(fill = "grey90", color = NA)
#   )+ scale_x_continuous(breaks = c(0, 7, 14, 21, 42))+
#   scale_color_manual(values = strain_colors, 
#                      name = "Strain",
#                      labels = function(x) parse(text = strain_labels[x]))+
#   scale_shape_manual(
#     name = "Strain",
#     values = c(
#       "a.faecalis" = 16,
#       "a.venet"    = 17,
#       "n.penta"    = 15,
#       "p.putida"   = 3,
#       "p.resin"    = 4,
#       "sphingo.sp" = 8
#     ),
#     labels = function(x) parse(text = strain_labels[x])
#   )+
#   
#   guides(
#     colour = guide_legend(
#       override.aes = list(
#         shape = c(16, 17, 15, 3, 4, 8),
#         linewidth = .5
#       )
#     ),
#     shape = "none"
#   )
# 
# 
