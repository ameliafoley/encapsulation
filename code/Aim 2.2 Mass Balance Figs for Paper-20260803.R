#load data. 
consortia <- read_excel(here::here("data", "Exp 2.2 PAH Results updated.xlsx"))
consortia <- consortia[ , -c(6,7)]
consortia.code <- read_excel(here::here("data", "Exp 2.2 Sample Code updated.xlsx"))
merged.consortia <- left_join(consortia, consortia.code, by = "sample.no")
glimpse(merged.consortia)
merged.consortia <- merged.consortia %>%
  mutate(
    naphthalene = as.numeric(naphthalene),
    phenanthrene = as.numeric(phenanthrene),
    fluoranthene = as.numeric(fluoranthene), 
    sample.no = as.numeric(sample.no)
  )
#adjust capsule concentrations based on dilution
merged.consortia.adj <- merged.consortia %>%
  mutate(
    across(c(naphthalene, phenanthrene, fluoranthene),
           ~ ifelse(treatment == "cc", .x * 5, .x))
  )
write.csv(merged.consortia.adj, "merged.consortia.adj.csv")
#adjust for values below the MDL
merged.consortia.adj <- merged.consortia.adj %>%
  mutate(
    # Replace NAs with half the MDL
    naphthalene  = ifelse(is.na(naphthalene), 0.01 / 2, naphthalene),
    phenanthrene = ifelse(is.na(phenanthrene), 6.29 / 2, phenanthrene),
    fluoranthene = ifelse(is.na(fluoranthene), 0.33 / 2, fluoranthene),
    
    # Flag if below MDL (either originally missing OR < MDL after replacement)
    naphthalene_bdl  = naphthalene  < 0.01,
    phenanthrene_bdl = phenanthrene < 6.29,
    fluoranthene_bdl = fluoranthene < 0.33
  )

saveRDS(merged.consortia.adj, "merged.consortia.adj2.2")

##full workflow
#keep raw values in long format with flags
long_with_flags <- merged.consortia.adj %>%
  pivot_longer(
    cols = c(naphthalene, phenanthrene, fluoranthene),
    names_to = "compound",
    values_to = "value"
  ) %>%
  mutate(
    bdl_flag = case_when(
      compound == "naphthalene"  ~ naphthalene_bdl,
      compound == "phenanthrene" ~ phenanthrene_bdl,
      compound == "fluoranthene" ~ fluoranthene_bdl
    ),
    type = "raw"   # marker so we know these are raw values
  )
#summarize to means + SE
sum_long <- long_with_flags %>%
  group_by(consortia, day, treatment, compound) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    se   = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
    .groups = "drop"
  ) %>%
  mutate(type = "summary")
#merge raw and summary into one data frame
plot_data <- bind_rows(long_with_flags, sum_long)
# constants
V_AQ <- 10   # mL aqueous per reactor
M_CAP <- 2   # g capsule per reactor

# prepare data: raw rows only, ensure types
df <- plot_data %>%
  filter(type == "raw") %>%
  mutate(
    day = as.character(day),
    value = as.numeric(value),
    treatment = as.character(treatment),
    consortia = as.character(consortia),
    sample = as.character(sample)
  )

library(dplyr)
library(tidyr)
library(stringr)

treatment_colors <- c(
  # Free / planktonic
  "free"        = "#D85A44FF",
  "planktonic"  = "#D85A44FF",
  "f"  = "#D85A44FF",
  
  # Aqueous / supernatant
  "aqueous"     = "#98A54FFF",
  "super"       = "#98A54FFF",
  "supernatant" = "#98A54FFF",
  "c" = "#98A54FFF",
  
  # Capsule
  #"capsule"     = "#2E92A2FF",
  "cap"         = "#2E92A2FF",
  "cc"         = "#2E92A2FF",
  
  # Encapsulated combined
  "capsule"      = "#61BEA4FF", #for this script in particular, I used capsule to represent the combined encapsulated reactor
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
  
  #"capsule"     = "dotted",
  "cap"         = "dotted",
  "cc"         = "dotted",
  
  "capsule"      = "dotdash", #for this script in particular, I used "capsule" to represent the combined encapsulated reactor
  "encapsulated"= "dotdash"
)

# df is your raw data: sample, compound, day, consortia, treatment, value (numeric)
# V_AQ and M_CAP already defined

# 1) split sample into parts and create sample_base (reactor id)
df2 <- df %>%
  mutate(sample = as.character(sample)) %>%
  separate(sample, into = c("p1", "p2", "p3"), sep = "-", fill = "right", remove = FALSE) %>%
  mutate(
    sample_base = case_when(
      !is.na(p3) & p3 == "c" ~ paste(p1, p2, sep = "-"),  # c-a-c -> c-a
      !is.na(p2)              ~ paste(p1, p2, sep = "-"),  # f-a -> f-a ; f-c -> f-c
      TRUE                    ~ p1
    ),
    sample_orig = .data$sample
  ) %>%
  select(-p1, -p2, -p3)

# 2) create one sample_orig_list per reactor (sample_base × compound × day × consortia)
orig_lists <- df2 %>%
  group_by(sample_base, compound, day, consortia) %>%
  summarise(sample_orig_list = list(unique(sample_orig)), .groups = "drop")

# 3) pivot values wider by sample_base (DO NOT include sample_orig_list in id_cols)
wide_vals <- df2 %>%
  select(sample_base, compound, day, consortia, treatment, value) %>%
  pivot_wider(
    id_cols = c(sample_base, compound, day, consortia),
    names_from = treatment,
    values_from = value,
    values_fn = list(value = mean),
    values_fill = NA_real_
  )

# 4) attach the precomputed sample_orig_list (one per reactor)
wide_by_reactor <- wide_vals %>%
  left_join(orig_lists, by = c("sample_base", "compound", "day", "consortia")) %>%
  rename(sample = sample_base)

# 5) diagnostic: show rows where cc exists (should have corresponding c or NA if missing)
check_cc <- wide_by_reactor %>%
  filter(!is.na(cc)) %>%
  select(sample, sample_orig_list, compound, day, consortia, c, cc, f)

if (nrow(check_cc) == 0) {
  message("No cc rows found (no capsule-material measurements).")
} else {
  message("Rows where cc is present (c should be on same row if supernatant exists):")
  print(check_cc %>% arrange(sample, compound, day) %>% slice_head(n = 80))
}

# 6) compute aqueous/capsule conc, masses and total mass
wide_by_reactor <- wide_by_reactor %>%
  mutate(
    aqueous_conc = coalesce(f, c),   # ng/mL: prefer f (free) else c (supernatant)
    capsule_conc = cc,               # ng/g
    mass_aqueous = if_else(!is.na(aqueous_conc), aqueous_conc * V_AQ, 0),
    mass_capsule = if_else(!is.na(capsule_conc), capsule_conc * M_CAP, 0),
    total_mass = mass_aqueous + mass_capsule,
    treatment_combined = case_when(
      !is.na(capsule_conc) | !is.na(c) | !is.na(cc) ~ "capsule",
      !is.na(f)                                        ~ "f",
      TRUE                                             ~ "other"
    )
  )

# 7) quick sample of the wide_by_reactor for manual inspection
print(wide_by_reactor %>% select(sample, sample_orig_list, compound, day, consortia, f, c, cc, aqueous_conc, capsule_conc, total_mass) %>% slice_head(n = 60))


# compute Day 0 baseline and percent degraded per reactor-group
mass_balance <- wide_by_reactor %>%
  group_by(compound, consortia, treatment_combined) %>%
  mutate(
    total_mass_day0 = mean(total_mass[day == "0"], na.rm = TRUE),
    frac_remaining = total_mass / total_mass_day0,
    perc_remaining = frac_remaining * 100,
    perc_degraded = (1 - frac_remaining) * 100
  ) %>%
  ungroup()

# summarize for plotting/statistics
mass_summary <- mass_balance %>%
  group_by(compound, consortia, treatment_combined, day) %>%
  summarise(
    mean_perc_remaining = mean(perc_remaining, na.rm = TRUE),
    se_perc_remaining   = ifelse(n() > 1, sd(perc_remaining, na.rm = TRUE) / sqrt(n()), NA_real_),
    mean_perc_degraded  = mean(perc_degraded, na.rm = TRUE),
    se_perc_degraded    = ifelse(n() > 1, sd(perc_degraded, na.rm = TRUE) / sqrt(n()), NA_real_),
    n = n(),
    .groups = "drop"
  )
## statistics functions
analyze_mass <- function(compound_name){
  
  dat <-
    
    mass_balance %>%
    
    filter(
      compound == compound_name
    ) %>%
    
    mutate(
      treatment_combined = factor(
        treatment_combined,
        levels = c("f","capsule")
      ),
      consortia = factor(
        consortia,
        levels = c("a","k","m","r")
      ),
      day = factor(
        day,
        levels = c(0,7,14,21,42)
      )
    )
  
  fit <-
    
    aov(
      perc_remaining ~
        consortia *
        treatment_combined *
        day,
      data = dat
    )
  
  print(summary(fit))
  
  ###########################################################
  # Tukey
  ###########################################################
  
  emm_tukey <-
    
    emmeans(
      fit,
      ~ treatment_combined * day |
        consortia
    )
  
  letters <-
    
    multcomp::cld(
      emm_tukey,
      adjust = "tukey",
      Letters = letters
    )
  
  ###########################################################
  # Dunnett
  ###########################################################
  
  emm_control <-
    
    emmeans(
      fit,
      ~ consortia |
        treatment_combined * day
    )
  
  dunnett <-
    
    contrast(
      emm_control,
      method = "trt.vs.ctrl",
      ref = "a",
      adjust = "dunnett"
    )
  
  list(
    
    fit = fit,
    
    tukey = letters,
    
    dunnett = summary(dunnett)
    
  )
  
}
stats_mass_fla <- analyze_mass("fluoranthene")

stats_mass_nap <- analyze_mass("naphthalene")

stats_mass_phe <- analyze_mass("phenanthrene")

##build plotting data
letters_mass_fla <- stats_mass_fla$tukey %>%
  as.data.frame() %>%
  mutate(
    .group = trimws(.group),
    letters = trimws(.group)
  )

letters_mass_nap <- stats_mass_nap$tukey %>%
  as.data.frame() %>%
  mutate(
    .group = trimws(.group),
    letters = trimws(.group)
  )

letters_mass_phe <- stats_mass_phe$tukey %>%
  as.data.frame() %>%
  mutate(
    .group = trimws(.group),
    letters = trimws(.group)
  )
make_plot_data_mass <- function(summary_data,
                                letters,
                                dunnett,
                                compound_name){
  
  plot_data <-
    
    summary_data %>%
    
    filter(
      compound == compound_name
    ) %>%
    
    left_join(
      
      letters,
      
      by = c(
        "consortia",
        "treatment_combined",
        "day"
      )
      
    ) %>%
    
    left_join(
      
      dunnett,
      
      by = c(
        "consortia",
        "treatment_combined",
        "day"
      )
      
    ) %>%
    
    mutate(
      
      .group = trimws(.group),
      
      letters = trimws(.group),
      
      stars = replace_na(stars,"")
      
    )
  
  plot_data
  
}

##dunnet
make_dunnett_mass <- function(x){
  
  x$dunnett %>%
    
    mutate(
      
      consortia = sub(" - a","",contrast),
      
      stars = case_when(
        
        p.value < .001 ~ "***",
        
        p.value < .01 ~ "**",
        
        p.value < .05 ~ "*",
        
        TRUE ~ ""
        
      )
      
    ) %>%
    
    select(
      
      consortia,
      
      treatment_combined,
      
      day,
      
      stars
      
    )
  
}
##now generate plotting data
plot_mass_fla <- make_plot_data_mass(
  mass_summary,
  letters_mass_fla,
  make_dunnett_mass(stats_mass_fla),
  "fluoranthene"
)

plot_mass_nap <- make_plot_data_mass(
  mass_summary,
  letters_mass_nap,
  make_dunnett_mass(stats_mass_nap),
  "naphthalene"
)

plot_mass_phe <- make_plot_data_mass(
  mass_summary,
  letters_mass_phe,
  make_dunnett_mass(stats_mass_phe),
  "phenanthrene"
)

#fix levels
plot_mass_fla <- plot_mass_fla %>%
  mutate(
    day = factor(day,
                 levels = c("0","7","14","21","42"))
  )

plot_mass_nap <- plot_mass_nap %>%
  mutate(
    day = factor(day,
                 levels = c("0","7","14","21","42"))
  )

plot_mass_phe <- plot_mass_phe %>%
  mutate(
    day = factor(day,
                 levels = c("0","7","14","21","42"))
  )

mass_balance <- mass_balance %>%
  mutate(
    day = factor(day,
                 levels = c("0","7","14","21","42"))
  )

plot_mass <- function(plot_data,
                      raw_data,
                      ylab = "Total PAH Remaining (%)") {
  
  pd <- position_dodge(width = 0.4)
  
  ###########################################################
  # Automatic annotation spacing
  ###########################################################
  
  y_max <- max(
    plot_data$mean_perc_remaining +
      plot_data$se_perc_remaining,
    na.rm = TRUE
  )
  
  letter_offset <- 0.04 * y_max
  star_offset   <- 0.08 * y_max
  
  ggplot() +
    
    ###########################################################
  # Raw reactor values
  ###########################################################
  
  geom_point(
    
    data = raw_data,
    
    aes(
      x = day,
      y = perc_remaining,
      colour = treatment_combined
    ),
    
    position = pd,
    
    alpha = 0.35,
    
    size = 2
    
  ) +
    
    ###########################################################
  # Mean lines
  ###########################################################
  
  geom_line(
    
    data = plot_data,
    
    aes(
      x = day,
      y = mean_perc_remaining,
      colour = treatment_combined,
      linetype = treatment_combined,
      group = treatment_combined
    ),
    
    linewidth = 0.9,
    
    position = pd
    
  ) +
    
    ###########################################################
  # Mean points
  ###########################################################
  
  geom_point(
    
    data = plot_data,
    
    aes(
      x = day,
      y = mean_perc_remaining,
      colour = treatment_combined,
      group = treatment_combined
    ),
    
    position = pd,
    
    size = 2.5
    
  ) +
    
    ###########################################################
  # Error bars
  ###########################################################
  
  geom_errorbar(
    
    data = plot_data,
    
    aes(
      
      x = day,
      
      ymin = mean_perc_remaining - se_perc_remaining,
      
      ymax = mean_perc_remaining + se_perc_remaining,
      
      colour = treatment_combined,
      
      group = treatment_combined
      
    ),
    
    width = 0.15,
    
    position = pd
    
  ) +
    
    ###########################################################
  # Tukey letters
  ###########################################################
  
  geom_text(
    
    data = plot_data,
    
    aes(
      
      x = day,
      
      y = mean_perc_remaining +
        se_perc_remaining +
        letter_offset,
      
      label = letters,
      
      group = treatment_combined
      
    ),
    
    position = pd,
    
    size = 3.5
    
  ) +
    
    ###########################################################
  # Dunnett stars
  ###########################################################
  
  geom_text(
    
    data = plot_data,
    
    aes(
      
      x = day,
      
      y = mean_perc_remaining +
        se_perc_remaining +
        star_offset,
      
      label = ifelse(day == "0", "", stars),
      
      group = treatment_combined
      
    ),
    
    position = pd,
    
    size = 4,
    
    fontface = "bold"
    
  ) +
    
    ###########################################################
  # Facets
  ###########################################################
  
  facet_wrap(
    
    ~ consortia,
    
    ncol = 4,
    
    labeller = as_labeller(
      
      c(
        
        a = "Abiotic",
        
        k = "K-strat",
        
        m = "Mixed",
        
        r = "R-strat"
        
      )
      
    )
    
  ) +
    
    ###########################################################
  # Theme
  ###########################################################
  
  theme_pubr() +
    theme(
      strip.text = element_text(
        face = "italic",
        size = 12
      )
    ) +
    scale_colour_manual(
      values = treatment_colors
    ) +
    scale_linetype_manual(
      values = treatment_linetypes
    ) +
    labs(
      x = "Day",
      y = ylab,
      colour = NULL,
      linetype = NULL
    ) +
    
    ###########################################################
  # Colors / linetypes
  ###########################################################
  
  scale_colour_manual(values = treatment_colors,
    
    labels = c(
      
      f = "Free",
      
      capsule = "Capsule"
      
    ),
    
    name = "Treatment"
    
  ) +
    
    scale_linetype_manual(
      
      values = c(
        
        f = 1,
        
        capsule = 2
        
      ),
      
      labels = c(
        
        f = "Free",
        
        capsule = "Capsule"
        
      ),
      
      name = "Treatment"
      
    ) +
    
    labs(
      
      x = "Day",
      
      y = ylab
      
    )
}

plot_mass(
  plot_mass_fla,
  filter(mass_balance, compound == "fluoranthene"),
  "Fluoranthene Remaining (%)"
)

plot_mass(
  plot_mass_nap,
  filter(mass_balance, compound == "naphthalene"),
  "Naphthalene Remaining (%)"
)

plot_mass(
  plot_mass_phe,
  filter(mass_balance, compound == "phenanthrene"),
  "Phenanthrene Remaining (%)"
)
