
# ============================================================================
# Ames Assay Dose-Response Analysis (72-hour Total Only)
# Version 2.0
# Designed for repeated Ames assays with experiment-specific controls
#
# Input requirements:
# sample_no, samples, dose, mix, day, exp, strain, s9, 48_hr, 72_hr, total
# ============================================================================

packages <- c(
  "readxl","dplyr","tidyr","purrr","broom","ggplot2",
  "janitor","stringr","readr","tibble"
)
missing <- packages[!packages %in% installed.packages()[,1]]
if(length(missing)>0) install.packages(missing)
invisible(lapply(packages, library, character.only=TRUE))

# ---------------- USER SETTINGS ----------------

input_file <- "Ames Assay Results 20260824 copy.xlsx"
output_dir <- here::here(
  "ames_outputs_20260824"
)


dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(
  file.path(output_dir, "plots"),
  recursive = TRUE,
  showWarnings = FALSE
)
message("Output directory: ", output_dir)
message("Directory exists: ", dir.exists(output_dir))

# Check where it was created
message("Output directory: ", output_dir)
message("Directory exists: ", dir.exists(output_dir))
dose_key <- tribble(
  ~dose, ~dose_value,
  "a",50,
  "b",25,
  "c",13,
  "d",5
)


dir.create(output_dir, showWarnings = FALSE)
dir.create(file.path(output_dir,"plots"), showWarnings = FALSE)

# ---------------- READ DATA ----------------

#raw <- read_excel(input_file) |> clean_names()
raw <- read_excel(here::here("data", "Ames Assay Results 20260824 copy.xlsx"))|> janitor::clean_names()

names(raw) <- str_replace_all(names(raw), "^x48_hr$", "hr48")
names(raw) <- str_replace_all(names(raw), "^x72_hr$", "hr72")

required <- c("samples","dose","mix","day","exp",
              "strain","s9","hr48","hr72","total")

stopifnot(all(required %in% names(raw)))

assay <- raw |>
  mutate(
    samples = str_to_lower(as.character(samples)),
    dose    = str_to_lower(as.character(dose)),
    strain  = as.character(strain),
    s9      = str_to_lower(as.character(s9)),
    total   = if_else(is.na(total), hr48 + hr72, total)
  )

##ignore original experiments that have repeats
assay <- assay %>%
  filter(!exp %in% c("5", "6", "9"))

# ---------------- QUALITY CHECKS ----------------

qc <- assay |>
  group_by(exp,strain,s9) |>
  summarise(
    dmso_n = sum(samples=="dmso"),
    treatments = sum(dose %in% dose_key$dose),
    missing_total = sum(is.na(total)),
    .groups="drop"
  )

write_csv(qc,file.path(output_dir,"quality_check_summary.csv"))

# ---------------- TREATMENTS ----------------

treatment <- assay |>
  filter(dose %in% dose_key$dose) |>
  left_join(dose_key,by="dose")

summary_by_dose <- treatment |>
  group_by(exp,samples,mix,day,strain,s9,dose,dose_value) |>
  summarise(
    n=n(),
    mean_rev=mean(total,na.rm=TRUE),
    sd_rev=sd(total,na.rm=TRUE),
    se_rev=sd_rev/sqrt(n),
    .groups="drop"
  )

write_csv(summary_by_dose,
          file.path(output_dir,"summary_by_dose.csv"))

# ---------------- EXPERIMENT-SPECIFIC DMSO ----------------

dmso_ref <- assay |>
  filter(samples=="dmso") |>
  group_by(exp,strain,s9) |>
  summarise(
    dmso_mean=mean(total,na.rm=TRUE),
    dmso_sd=sd(total,na.rm=TRUE),
    dmso_2x=2*dmso_mean,
    .groups="drop"
  )

analysis <- treatment |>
  left_join(dmso_ref,
            by=c("exp","strain","s9"))

# ---------------- LINEAR REGRESSION ----------------

fit_one <- function(df){
  
  df <- df |>
    filter(!is.na(dose_value),
           !is.na(total))
  
  # Need at least two different dose levels
  if(nrow(df) < 3 ||
     dplyr::n_distinct(df$dose_value) < 2){
    
    return(tibble(
      intercept = NA_real_,
      slope = NA_real_,
      slope_se = NA_real_,
      p_value = NA_real_,
      r_squared = NA_real_,
      n = nrow(df),
      max_fold_dmso = NA_real_,
      positive = FALSE
    ))
  }
  
  fit <- lm(total ~ dose_value, data = df)
  
  td <- broom::tidy(fit)
  gl <- broom::glance(fit)
  
  slope_row <- td |>
    filter(term == "dose_value")
  
  if(nrow(slope_row) == 0){
    
    return(tibble(
      intercept = coef(fit)[1],
      slope = NA_real_,
      slope_se = NA_real_,
      p_value = NA_real_,
      r_squared = gl$r.squared,
      n = nrow(df),
      max_fold_dmso = NA_real_,
      positive = FALSE
    ))
    
  }
  
  dmso_mean <- unique(df$dmso_mean)[1]
  
  max_fold <- max(df$total, na.rm = TRUE) / dmso_mean
  
  tibble(
    intercept = coef(fit)[1],
    slope = slope_row$estimate,
    slope_se = slope_row$std.error,
    p_value = slope_row$p.value,
    r_squared = gl$r.squared,
    n = nrow(df),
    max_fold_dmso = max_fold,
    positive = !is.na(slope_row$p.value) &&
      slope_row$p.value <= 0.05 &&
      max_fold >= 2
  )
  
}
potency <- analysis |>
  group_by(exp,samples,mix,day,strain,s9) |>
  nest() |>
  mutate(model=map(data,fit_one)) |>
  unnest(model) |>
  select(-data)

write_csv(potency,
          file.path(output_dir,"linear_potency.csv"))

# ---------------- INITIAL LINEAR REGION ----------------

fit_initial <- function(df){
  
  doses <- sort(unique(df$dose_value))
  
  if(length(doses)<3) return(NULL)
  
  map_dfr(3:length(doses), function(k){
    
    keep <- doses[1:k]
    
    d <- filter(df,dose_value %in% keep)
    
    fit <- lm(total~dose_value,data=d)
    
    s <- tidy(fit) |> filter(term=="dose_value")
    g <- glance(fit)
    
    tibble(
      doses_used=paste(keep,collapse=";"),
      slope=s$estimate,
      slope_p=s$p.value,
      r2=g$r.squared
    )
    
  }) |>
    arrange(desc(r2)) |>
    slice(1)
  
}

initial_linear <- analysis |>
  group_by(exp,samples,mix,day,strain,s9) |>
  nest() |>
  mutate(best=map(data,fit_initial)) |>
  unnest(best) |>
  select(-data)

write_csv(initial_linear,
          file.path(output_dir,"initial_linear_region.csv"))

# ---------------- POSITIVE CONTROLS ----------------

controls <- assay |>
  filter(samples %in% c("dmso","nf","sa","2aa"))

control_summary <- controls |>
  group_by(exp,samples,strain,s9) |>
  summarise(
    n=n(),
    mean_rev=mean(total,na.rm=TRUE),
    se=sd(total,na.rm=TRUE)/sqrt(n),
    .groups="drop"
  )

write_csv(control_summary,
          file.path(output_dir,"control_summary.csv"))

# ---------------- QC TABLE ----------------

qc_table <- control_summary |>
  pivot_wider(
    names_from=samples,
    values_from=mean_rev
  )

write_csv(qc_table,
          file.path(output_dir,"experiment_qc_table.csv"))

# ---------------- PLOTS ----------------

plot_groups <- summary_by_dose |>
  distinct(exp,strain,s9,mix)

walk(seq_len(nrow(plot_groups)), function(i){
  
  g <- plot_groups[i,]
  
  d <- summary_by_dose |>
    filter(exp==g$exp,
           strain==g$strain,
           s9==g$s9,
           mix==g$mix) |>
    left_join(dmso_ref,
              by=c("exp","strain","s9"))
  
  p <- ggplot(d,
              aes(dose_value,
                  mean_rev,
                  color=samples))+
    geom_hline(aes(yintercept=dmso_mean),
               linetype="dashed")+
    geom_hline(aes(yintercept=dmso_2x),
               linetype="dotted",
               color="red")+
    geom_point(size=3)+
    geom_errorbar(aes(
      ymin=mean_rev-se_rev,
      ymax=mean_rev+se_rev),
      width=.25)+
    geom_smooth(method="lm",se=TRUE)+
    facet_wrap(~day)+
    theme_bw()+
    labs(
      title=paste("Experiment",g$exp,
                  g$strain,
                  ifelse(g$s9=="yes","+S9","-S9"),
                  "Mix",g$mix),
      x="Dose",
      y="Mean revertants ± SE"
    )
  
  ggsave(
    filename=file.path(
      output_dir,
      "plots",
      paste0(
        "Exp",g$exp,"_",
        g$strain,"_",
        g$s9,"_Mix",g$mix,".png"
      )
    ),
    plot=p,
    width=8,
    height=5,
    dpi=300
  )
  
})

message("Analysis complete.")

## multi panel figures
plot_dat <- summary_by_dose %>%
  left_join(dmso_ref, by = c("exp", "strain", "s9")) %>%
  mutate(
    strain = factor(strain, levels = c("TA98", "TA100")),
    s9 = factor(s9, levels = c("no", "yes"),
                labels = c("-S9", "+S9"))
  )
ggplot(
  plot_dat,
  aes(x = dose_value,
      y = mean_rev,
      color = samples,
      group = samples)
) +
  geom_hline(
    aes(yintercept = dmso_mean),
    linetype = "dashed",
    color = "blue"
  ) +
  geom_hline(
    aes(yintercept = dmso_2x),
    linetype = "dotted",
    color = "red"
  ) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = mean_rev - se_rev,
      ymax = mean_rev + se_rev
    ),
    width = 0.25
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  facet_grid(
    rows = vars(strain, s9),
    cols = vars(mix, day),
    scales = "free_y"
  ) +
  labs(
    title = "72-hour Ames Dose Response",
    subtitle = "Rows: Strain/S9   Columns: Experiment and Mix",
    x = "Dose",
    y = "Mean revertants ± SE"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
