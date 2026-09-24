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
    # remove the 6 samples from plate 2 8/7/26 updated this to add FAM and Texas Red qualifier bc pputida and presin data was missing
    !(plate_id == 2 & fluor %in% c("FAM", "Texas Red") & sample %in% c("M-CA-7", "M-CB-7", "M-CC-7", 
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
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x), 
                labels = trans_format( "log10", math_format(10^.x) )) +
  labs(
    title = "Mean Gene Copies per Sample Volume Over Time (with SE)",
    x = "Day",
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
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x), 
                labels = trans_format( "log10", math_format(10^.x) ))+
  labs(
    title = "Mean Gene Copies per mL",
    x = "Day",
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
    x = "Day",
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
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x), 
                labels = trans_format( "log10", math_format(10^.x) ))+ 
  facet_grid(consortia ~ sample.type) +
  labs(
    title = "Mean Gene Copies per mL",
    y = "Gene Copies per mL (log scale)",
    x = "Day",
    color = "Treatment"
  ) +
  theme_minimal()

##jittered
pd  <- position_dodge(width = 1)  # for lines & error bars
pjd <- position_jitterdodge(jitter.width = .3, jitter.height = 0, dodge.width = 1)  # for points
my_labels <- list(
  treatment = c(
    "free" = "Free",
    "encapsulated" = "Encapsulated", 
    "supernatant" = "Supernatant"
  ),
  consortia = c(
    "k" = "K-Strat",
    "m" = "Mixed",
    "r" = "R-Strat"
  ), 
  sample.type = c(
    "planktonic" = "Free", 
    "supernatant" = "Extracapsular",
    "capsule" = "Capsule"
    
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
strain_colors <- c(k_colors, r_colors)

#lod_50 <- qpcr_avg_ind %>% filter(mean_sq > 50) #filter out values outside of standard curve/limit of detection

LOD <- 50
LOD_line <- 50*150/1

qpcr_plot <- qpcr_avg_ind %>%
  mutate(
    below_lod = mean_sq < LOD | is.na(mean_sq),
    
    gene_copies_plot =
      if_else(
        below_lod,
        (50*150/1)/2, #because for all samples where this is a concern, they are supernatant, and this was the calculation
        gene_copies_per_mL
      )
  )

qpcr_plot$sample.type <- factor(
  qpcr_plot$sample.type,
  levels = c("planktonic", "supernatant", "capsule")
)

qpcr_plot$strain <- factor(
  qpcr_plot$strain,
  levels = c("a.faecalis", "n.penta", "sphingo.sp", "p.putida", "p.resin", "a.venet")
)

qpcr_summary_plot <- qpcr_plot %>%
  group_by(
    day,
    fluor,
    consortia,
    treatment,
    sample.type,
    sample.vol.ml,
    elution.vol.ul,
    strain
  ) %>%
  summarise(
    mean_gene_copies = mean(gene_copies_plot),
    se_gene_copies = sd(gene_copies_plot) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )



ggplot(qpcr_plot, aes(x = day, y = gene_copies_plot, color = strain, shape = strain)) + #CHANGED TO GENE_COPIES_PLOT
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
    size = 1.5
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 3,
    position = pd
  ) +
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x), 
                labels = trans_format( "log10", math_format(10^.x) ))+  
  facet_grid(consortia ~ sample.type, labeller = labeller(
    sample.type = as_labeller(my_labels$sample.type), 
    consortia = as_labeller(my_labels$consortia)
  )) +
  labs(
    #title = "Mean Gene Copies per mL",
    y = "Gene Copies per mL (log scale)",
    x = "Day"
  ) +
  theme_pubr()+
  theme(
    strip.text = element_text(size = 12, face = "italic"), 
    axis.text.x = element_text(size = 9), 
    axis.text.y = element_text(size = 9), 
    panel.spacing.x = unit(0.08, "cm")
    #strip.background = element_rect(fill = "grey90", color = NA)
  )+ scale_x_continuous(breaks = c(0, 7, 14, 21, 42))+
  scale_color_manual(values = strain_colors, 
                     name = "Strain",
                     labels = function(x) parse(text = strain_labels[x]))+
  scale_shape_manual(
    name = "Strain",
    values = c(
      "a.faecalis" = 16,
      "a.venet"    = 17,
      "n.penta"    = 15,
      "p.putida"   = 3,
      "p.resin"    = 4,
      "sphingo.sp" = 8
    ),
    labels = function(x) parse(text = strain_labels[x])
  )+
  
  guides(
    colour = guide_legend(
      override.aes = list(
        shape = c(16, 17, 15, 3, 4, 8),
        linewidth = .5
      )
    ),
    shape = "none"
  )+
  geom_hline(
    yintercept = LOD_line,
    linetype = "dashed",
    colour = "grey40"
  )
qpcr_all_fig <- last_plot()

##MASS BALANCE FOR QPCR DATA#

library(dplyr)
library(tidyr)

df <- qpcr_plot #was qpcr_avg_ind before updating based on LOD of 50 cp.uL

# standardize sample.type values so 'planktonic' maps to 'supernatant'
df <- df %>%
  rename(sample_type = sample.type,
         gc_per_mL = gene_copies_plot) %>%
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
pd  <- position_dodge(width = 4)  # for lines & error bars
pjd <- position_jitterdodge(jitter.width = .3, jitter.height = 0, dodge.width = 4)  # for points
my_labels <- list(
  treatment = c(
    "free" = "Free",
    "encapsulated" = "Encapsulated", 
    "supernatant" = "Supernatant"
  ),
  consortia = c(
    "k" = "K-Strat",
    "m" = "Mixed",
    "r" = "R-Strat"
  )
)
ggplot(wide, aes(x = day, y = gc_total_per_mL, color = strain, shape = strain)) +
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
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x), 
                labels = trans_format( "log10", math_format(10^.x) )) +  
  facet_grid(consortia ~ treatment, labeller = labeller(
    treatment = as_labeller(my_labels$treatment), 
    consortia = as_labeller(my_labels$consortia)
  )) +
  labs(
   # title = "Mass Balance - Gene Copies per mL",
    y = "Gene Copies per mL (log scale)",
    x = "Day",
    color = "Strain"
  ) +
  theme_pubr()+
  theme(
    strip.text = element_text(size = 12, face = "italic")
    #strip.background = element_rect(fill = "grey90", color = NA)
  )+ scale_x_continuous(breaks = c(0, 7, 14, 21, 42))+
  scale_color_manual(values = strain_colors, 
                     name = "Strain",
                     labels = function(x) parse(text = strain_labels[x]))+
  scale_shape_manual(
    name = "Strain",
    values = c(
      "a.faecalis" = 16,
      "a.venet"    = 17,
      "n.penta"    = 15,
      "p.putida"   = 3,
      "p.resin"    = 4,
      "sphingo.sp" = 8
    ),
    labels = function(x) parse(text = strain_labels[x])
  )+
  
  guides(
    colour = guide_legend(
      override.aes = list(
        shape = c(16, 17, 15, 3, 4, 8),
        linewidth = .5
      )
    ),
    shape = "none"
  )

##trying faceting by STRAIN instead for easier comparisons
treatment_colors <- c(
  # Free / planktonic
  "free"        = "#D85A44FF",
  "planktonic"  = "#D85A44FF",
  "f"  = "#D85A44FF",
  
  # Aqueous / supernatant
  "aqueous"     = "#2E92A2FF",
  "super"       = "#2E92A2FF",
  "supernatant" = "#2E92A2FF",
  "c" = "#2E92A2FF",
  
  # Capsule
  "capsule"     = "#98A54FFF",
  "cap"         = "#98A54FFF",
  "cc"         = "#98A54FFF",
  
  # Encapsulated combined
  #"capsule"      = "#61BEA4FF", #for this script in particular, I used capsule to represent the combined encapsulated reactor
  "encapsulated"= "#61BEA4FF"   # if this level exists anywhere
  
)
treatment_linetypes <- c(
  "free"        = "solid",
  "planktonic"  = "solid",
  "f"  = "solid",
  
  "aqueous"     = "dashed",
  "super"       = "dashed",
  "supernatant" = "dashed",
  "c" = "dashed",
  
  "capsule"     = "dotted",
  "cap"         = "dotted",
  "cc"         = "dotted",
  
  #"capsule"      = "dotdash",
  "encapsulated"= "dotdash"
)
##FACET BY STRAIN
ggplot(qpcr_plot, aes(x = day, y = gene_copies_plot, color = sample.type, linetype = sample.type)) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = sample.type),
    position = pd
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    position = pjd,           # jittered points
    size = 1.5
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 4,
    position = pd, 
    linetype = "solid"
  ) +
  scale_y_log10(breaks = trans_breaks("log10",function(x) 10^x), 
                labels = trans_format( "log10", math_format(10^.x) )) +  
  facet_grid(consortia ~ strain, labeller = labeller(
    strain = as_labeller(strain_labels, label_parsed),
    consortia = as_labeller(my_labels$consortia)
  )) +
  labs(
    y = "Gene Copies per mL (log scale)",
    x = "Day"
  ) +
  theme_pubr()+
  theme(
    strip.text = element_text(size = 12, face = "italic")
    #strip.background = element_rect(fill = "grey90", color = NA)
  )+ scale_x_continuous(breaks = c(0, 7, 14, 21, 42))+
  scale_color_manual(values = treatment_colors, 
                     name = "Sample", 
                     labels = c(planktonic = "Free", 
                                supernatant = "Extracapsular", 
                                capsule = "Capsule"), 
                     limits = c("planktonic", "supernatant", "capsule"))+
  scale_linetype_manual(values = treatment_linetypes, 
                        name = "Sample", 
                        labels = c(planktonic = "Free", 
                                   supernatant = "Extracapsular", 
                                   capsule = "Capsule"), 
                        limits = c("planktonic", "supernatant", "capsule"))+
  guides(
    colour = guide_legend(
      override.aes = list(
        linewidth = 1,
        linetype = c("solid", "dashed", "dotted"),
        shape = 16
      )
    ),
    linetype = "none"
  )+
  theme(
    legend.position = "top",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.spacing.x = unit(0.4, "cm"), 
    axis.text.x = element_text(size = 9), 
    axis.text.y = element_text(size = 9), 
    panel.spacing.x = unit(0.08, "cm")
  )+
  geom_hline(
    yintercept = LOD_line,
    linetype = "dashed",
    colour = "grey40"
  )

 
##FACET BY STRAIN ONLY FOR SHARED STRAINS                       
library(dplyr)

shared_strains <- qpcr_plot %>%
  distinct(strain, consortia) %>%
  count(strain) %>%
  filter(n > 1) %>%
  pull(strain)

shared_strains

qpcr_shared <- qpcr_plot %>%
  filter(strain %in% shared_strains) %>%
  mutate(
    community_context = if_else(
      consortia == "m",
      "Mixed",
      "Original",
      missing = "Original"
    ),
    strain = factor(
      strain,
      levels = c(
        "n.penta",
        "sphingo.sp",
        "p.putida",
        "p.resin"
      )
    ),
    community_context = factor(
      community_context,
      levels = c("Original", "Mixed")
    )
  )

table(qpcr_shared$strain, qpcr_shared$consortia)
ggplot(
  qpcr_shared,
  aes(
    x = day,
    y = gene_copies_plot,
    color = sample.type,
    linetype = sample.type
  )
) +
  stat_summary(
    fun = mean,
    geom = "line",
    aes(group = sample.type),
    position = pd
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    position = pjd,
    size = 1.5
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 4,
    position = pd,
    linetype = "solid"
  ) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  facet_grid(
    community_context ~ strain,
    labeller = labeller(
      strain = as_labeller(strain_labels, label_parsed)
    )
  ) +
  geom_hline(
    yintercept = LOD_line,
    linetype = "dashed",
    colour = "grey40"
  ) +
  scale_x_continuous(
    breaks = c(0, 7, 14, 21, 42)
  ) +
  labs(
    y = "Gene Copies per mL (log scale)",
    x = "Day"
  ) +
  scale_color_manual(
    values = treatment_colors,
    name = "Sample",
    labels = c(
      planktonic = "Free",
      supernatant = "Extracapsular",
      capsule = "Capsule"
    ),
    limits = c("planktonic", "supernatant", "capsule")
  ) +
  scale_linetype_manual(
    values = treatment_linetypes,
    name = "Sample",
    labels = c(
      planktonic = "Free",
      supernatant = "Extracapsular",
      capsule = "Capsule"
    ),
    limits = c("planktonic", "supernatant", "capsule")
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(
        linewidth = 1,
        linetype = c("solid", "dashed", "dotted"),
        shape = 16
      )
    ),
    linetype = "none"
  ) +
  theme_pubr() +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.spacing.x = unit(0.25, "cm"),
    strip.text = element_text(size = 9.5), 
    axis.text.x = element_text(size = 9), 
    axis.text.y = element_text(size = 9), 
    panel.spacing.x = unit(0.08, "cm")
  )
qpcr_shared <- last_plot()

  # scale_shape_manual(
  #   name = "Strain",
  #   values = c(
  #     "a.faecalis" = 16,
  #     "a.venet"    = 17,
  #     "n.penta"    = 15,
  #     "p.putida"   = 3,
  #     "p.resin"    = 4,
  #     "sphingo.sp" = 8
  #   ),
  #   labels = function(x) parse(text = strain_labels[x])
  # )

##STATS
###############################################################
# Split by consortium
###############################################################

k_data <- qpcr_stats %>%
  filter(consortia == "k")

m_data <- qpcr_stats %>%
  filter(consortia == "m")

r_data <- qpcr_stats %>%
  filter(consortia == "r")
###############################################################
# Analyze one consortium
###############################################################

analyze_consortium <- function(dat){
  
  fit <-
    
    aov(
      
      log_gc ~
        
        strain *
        treatment *
        day,
      
      data = dat
      
    )
  
  print(summary(fit))
  
  ###########################################################
  # Encapsulated vs free
  ###########################################################
  
  emm_treatment <-
    
    emmeans(
      
      fit,
      
      ~ treatment |
        
        strain * day
      
    )
  
  treatment_tests <-
    
    pairs(
      
      emm_treatment,
      
      adjust = "tukey"
      
    )
  
  ###########################################################
  # Day comparisons
  ###########################################################
  
  emm_day <-
    
    emmeans(
      
      fit,
      
      ~ day |
        
        strain * treatment
      
    )
  
  day_tests <-
    
    pairs(
      
      emm_day,
      
      adjust = "tukey"
      
    )
  
  list(
    
    fit = fit,
    
    treatment_emm = emm_treatment,
    
    treatment_tests = treatment_tests,
    
    day_emm = emm_day,
    
    day_tests = day_tests
    
  )
  
}
stats_k <- analyze_consortium(k_data)

stats_m <- analyze_consortium(m_data)

stats_r <- analyze_consortium(r_data)
summary(stats_k$fit)
stats_k$treatment_tests #npenta day 42 not significant; sphingo days 0, 7, 14, 21 are signficant

summary(stats_m$fit)
stats_m$treatment_tests 
#n penta day 7 *, npenta day 14 *, n penta day 42 p = 0.0503

###############################################################
# Community-level qPCR ANOVA
###############################################################

qpcr_comm <- qpcr_avg %>%
  filter(mean_sq > 50) %>%
  mutate(
    log_gc = log10(gene_copies_per_mL),
    day = factor(day, levels = c(0, 7, 14, 21, 42)),
    treatment = factor(treatment, levels = c("free", "encapsulated")),
    consortia = factor(consortia, levels = c("k", "m", "r"))
  )

community_fit <- aov(
  log_gc ~ consortia * treatment * day,
  data = qpcr_comm
)

summary(community_fit) #** in consortia*treatment*day -> subdividing by consortia is suitable(what we did above)

#invidiaul sstrain effect of treatment based on consortia
putida <- qpcr_stats %>%
  filter(strain == "p.putida")

fit_putida <- aov(
  log_gc ~ consortia * treatment * day,
  data = putida
)
summary(fit_putida)

###############################################################
# Analyze one strain across consortia
###############################################################

analyze_strain <- function(strain_name){
  
  dat <- qpcr_stats %>%
    filter(strain == strain_name)
  
  fit <- aov(
    log_gc ~
      consortia *
      treatment *
      day,
    data = dat
  )
  
  print(summary(fit))
  
  ###########################################################
  # Treatment comparisons
  ###########################################################
  
  emm_treatment <- emmeans(
    fit,
    ~ treatment | consortia * day
  )
  
  treatment_tests <- pairs(
    emm_treatment,
    adjust = "tukey"
  )
  
  ###########################################################
  # Consortium comparisons
  ###########################################################
  
  emm_consortia <- emmeans(
    fit,
    ~ consortia | treatment * day
  )
  
  consortia_tests <- pairs(
    emm_consortia,
    adjust = "tukey"
  )
  
  ###########################################################
  # Day comparisons
  ###########################################################
  
  emm_day <- emmeans(
    fit,
    ~ day | consortia * treatment
  )
  
  day_tests <- pairs(
    emm_day,
    adjust = "tukey"
  )
  
  list(
    fit = fit,
    treatment_emm = emm_treatment,
    treatment_tests = treatment_tests,
    consortia_emm = emm_consortia,
    consortia_tests = consortia_tests,
    day_emm = emm_day,
    day_tests = day_tests
  )
  
}
###############################################################
# Shared strains
###############################################################

stats_putida <- analyze_strain("p.putida")

stats_npenta <- analyze_strain("n.penta") #yes * in consortia*treatment*day

stats_sphingo <- analyze_strain("sphingo.sp")

stats_presin <- analyze_strain("p.resin")
