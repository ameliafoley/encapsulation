# full_pipeline_two_phase_complete.R
# Run AFTER you've loaded your raw plate data frames (f199, both, g7, g72, f1992)
# and after you've installed required packages.

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(gcplyr)    # for calc_deriv, find_local_extrema, lag_time, which_max_gc, min_gc, auc
library(flextable)
library(cowplot)
library(htmltools)

# -------------------------
# OPTIONAL: your phase1_time_map (edit to match your desired groups)
# -------------------------
phase1_time_map <- tibble::tribble(
  ~strain, ~media, ~treatment, ~min_time, ~max_time, ~min_phase2_time,
  "n.aroma", "sRB15", "cap", 0, 20, 40,
  "n.aroma", "sRB15", "free", 10, 65, 40,
  NA, NA, "chit", 0, 20, NA, 
  "n.aroma", "LB", "free", 0, 30, NA
)

# -------------------------
# 1) Smoothing
# -------------------------
smooth_plate <- function(df) {
  df %>%
    group_by(well, media, strain, treatment, plate.id) %>%
    mutate(
      smoothed_no = corrected,
      sm_med3 = smooth_data(x = time_h, y = corrected,
                            sm_method = "moving-median", window_width_n = 7),
      sm_mean = smooth_data(x = time_h, y = corrected,
                            sm_method = "moving-average", window_width_n = 7)
    ) %>%
    ungroup()
}

# Apply smoothing to your loaded data frames
f199.sm  <- smooth_plate(f199)
both.sm  <- smooth_plate(both)
g7.sm    <- smooth_plate(g7)
g72.sm   <- smooth_plate(g72)
f1992.sm <- smooth_plate(f1992)

# -------------------------
# 2) Per-capita derivative
# -------------------------
calc_plate_deriv <- function(df, window = 5, od_threshold = 0.01) {
  df %>%
    mutate(sm_mean_thres = ifelse(sm_mean < od_threshold, NA, sm_mean)) %>%
    group_by(well, plate.id) %>%
    mutate(
      percap_deriv = calc_deriv(
        y = sm_mean_thres,
        x = time_h,
        percapita = TRUE,
        blank = 0,
        window_width_n = window,
        trans_y = "log"
      )
    ) %>%
    ungroup()
}

# Apply derivative calc
f199_test  <- calc_plate_deriv(f199.sm)
both_test  <- calc_plate_deriv(both.sm)
g7_test    <- calc_plate_deriv(g7.sm)
g72_test   <- calc_plate_deriv(g72.sm)
f1992_test <- calc_plate_deriv(f1992.sm)

# -------------------------
# 3) Build per-group phase1 windows (group_windows)
#    MUST run BEFORE joining & summarizing
# -------------------------
# Normalize map: trim strings and coerce numeric times
phase1_time_map2 <- phase1_time_map %>%
  mutate(across(c(strain, media, treatment),
                ~ ifelse(is.na(.), NA_character_, str_trim(as.character(.)))),
         min_time = if ("min_time" %in% names(.) ) as.numeric(min_time) else NA_real_,
         max_time = if ("max_time" %in% names(.) ) as.numeric(max_time) else NA_real_,
         min_phase2_time = if ("min_phase2_time" %in% names(.)) as.numeric(min_phase2_time) else NA_real_)

# defaults when no row in map matches
global_min <- 0
global_max <- 15

# gather unique groups present in your data
groups <- bind_rows(
  f199_test  %>% distinct(strain, media, treatment),
  both_test  %>% distinct(strain, media, treatment),
  g7_test    %>% distinct(strain, media, treatment),
  g72_test   %>% distinct(strain, media, treatment),
  f1992_test %>% distinct(strain, media, treatment)
) %>%
  distinct() %>%
  mutate(across(c(strain, media, treatment),
                ~ ifelse(is.na(.), NA_character_, str_trim(as.character(.)))))

# helper: choose best map row (wildcard NA allowed); most-specific wins
get_best_window_for_group <- function(s_val, m_val, t_val, map_df, default_min, default_max) {
  s_val <- ifelse(is.na(s_val), NA_character_, str_trim(as.character(s_val)))
  m_val <- ifelse(is.na(m_val), NA_character_, str_trim(as.character(m_val)))
  t_val <- ifelse(is.na(t_val), NA_character_, str_trim(as.character(t_val)))
  
  if (is.null(map_df) || nrow(map_df) == 0) {
    return(tibble(eff_min = default_min, eff_max = default_max, eff_min_phase2 = NA_real_, matched_row = NA_integer_))
  }
  
  matched <- map_df %>%
    mutate(.row = row_number()) %>%
    filter(
      (is.na(strain) | strain == s_val) &
        (is.na(media)  | media == m_val) &
        (is.na(treatment) | treatment == t_val)
    )
  
  if (nrow(matched) == 0) {
    return(tibble(eff_min = default_min, eff_max = default_max, eff_min_phase2 = NA_real_, matched_row = NA_integer_))
  }
  
  matched <- matched %>%
    mutate(spec = (!is.na(strain)) + (!is.na(media)) + (!is.na(treatment))) %>%
    arrange(desc(spec), .row)
  
  best <- matched %>% slice(1)
  eff_min <- if (!is.na(best$min_time)) best$min_time else default_min
  eff_max <- if (!is.na(best$max_time)) best$max_time else default_max
  eff_min_phase2 <- if (!is.na(best$min_phase2_time)) best$min_phase2_time else NA_real_
  
  tibble(eff_min = eff_min, eff_max = eff_max, eff_min_phase2 = eff_min_phase2, matched_row = best$.row)
}

# compute group_windows dataframe
group_windows <- groups %>%
  mutate(tmp = pmap(list(strain, media, treatment),
                    function(s, m, t) {
                      get_best_window_for_group(s, m, t, phase1_time_map2, global_min, global_max)
                    })) %>%
  unnest_wider(tmp) %>%
  rename(eff_min_time_for_phase1 = eff_min,
         eff_max_time_for_phase1 = eff_max,
         eff_min_phase2_time = eff_min_phase2)

# Diagnostic prints to confirm mapping
message("Groups falling back to defaults (matched_row is NA):")
print(group_windows %>% filter(is.na(matched_row)))
message("Groups with mapped windows:")
print(group_windows %>% filter(!is.na(matched_row)) %>% arrange(strain, media, treatment))

# -------------------------
# 4) Join windows onto measurement tables (produce *_test2)
# -------------------------
join_windows <- function(df, group_windows) {
  df %>%
    mutate(across(c(strain, media, treatment),
                  ~ ifelse(is.na(.), NA_character_, str_trim(as.character(.))))) %>%
    left_join(group_windows %>% select(strain, media, treatment,
                                       eff_min_time_for_phase1, eff_max_time_for_phase1, eff_min_phase2_time),
              by = c("strain", "media", "treatment"))
}

f199_test2  <- join_windows(f199_test,  group_windows)
both_test2  <- join_windows(both_test,  group_windows)
g7_test2    <- join_windows(g7_test,    group_windows)
g72_test2   <- join_windows(g72_test,   group_windows)
f1992_test2 <- join_windows(f1992_test, group_windows)

# quick sanity (optional)
sanity_missing <- tibble(
  dataset = c("f199_test2","both_test2","g7_test2","g72_test2","f1992_test2"),
  n_missing_eff = c(
    sum(is.na(f199_test2$eff_max_time_for_phase1)),
    sum(is.na(both_test2$eff_max_time_for_phase1)),
    sum(is.na(g7_test2$eff_max_time_for_phase1)),
    sum(is.na(g72_test2$eff_max_time_for_phase1)),
    sum(is.na(f1992_test2$eff_max_time_for_phase1))
  )
)
print(sanity_missing)

# -------------------------
# 5) Summarizer (prejoined) -- robust version with multiple phase1 methods
# -------------------------
summarize_plate_two_phase_prejoined <- function(df,
                                                max_shift_time = 50,
                                                two_phase_treatments = c("cap", "chit"),
                                                # phase1 thresholds
                                                min_od_for_phase1 = 0,
                                                min_percap_rate = 0,
                                                fallback_to_global = FALSE,
                                                # method options: "extrema", "max_in_window", "sustained"
                                                phase1_method = c("extrema", "max_in_window", "sustained"),
                                                min_deriv_for_sustained = 0.005,
                                                sustained_consec = 3,
                                                # optional per-group column has been pre-joined as eff_min_phase2_time
                                                min_phase2_time = NULL,
                                                window_width_n = 15) {
  
  phase1_method <- match.arg(phase1_method)
  
  df %>%
    group_by(strain, media, treatment, well, plate.id) %>%
    summarize(
      # bring through the effective window (first() so scalar)
      eff_min_time_for_phase1 = first(eff_min_time_for_phase1),
      eff_max_time_for_phase1 = first(eff_max_time_for_phase1),
      eff_min_phase2_time     = first(eff_min_phase2_time),
      
      # --- Phase 1 detection (multiple selectable methods) ---
      {
        phase1_idx <- NA_integer_
        lag_time1 <- NA_real_
        max_percap1 <- NA_real_
        max_percap_time1 <- NA_real_
        max_percap_dens1 <- NA_real_
        
        inds_in_window <- which(time_h >= eff_min_time_for_phase1 & time_h <= eff_max_time_for_phase1)
        
        if (length(inds_in_window) > 0) {
          if (phase1_method == "extrema") {
            max_candidates <- find_local_extrema(x = time_h, y = percap_deriv,
                                                 return = "index", return_maxima = TRUE, return_minima = FALSE,
                                                 window_width_n = window_width_n)
            max_candidates <- max_candidates[max_candidates %in% inds_in_window]
            if (length(max_candidates) > 0) {
              ok_cands <- max_candidates[
                !is.na(sm_mean[max_candidates]) & sm_mean[max_candidates] >= min_od_for_phase1 &
                  !is.na(percap_deriv[max_candidates]) & percap_deriv[max_candidates] >= min_percap_rate
              ]
              if (length(ok_cands) > 0) {
                chosen <- ok_cands[which.max(percap_deriv[ok_cands])]
                phase1_idx <- chosen
              }
            }
            
          } else if (phase1_method == "max_in_window") {
            cand_inds <- inds_in_window[!is.na(percap_deriv[inds_in_window])]
            if (length(cand_inds) > 0) {
              ok_inds <- cand_inds[
                (!is.na(sm_mean[cand_inds]) & sm_mean[cand_inds] >= min_od_for_phase1) &
                  (percap_deriv[cand_inds] >= min_percap_rate)
              ]
              if (length(ok_inds) > 0) {
                phase1_idx <- ok_inds[which.max(percap_deriv[ok_inds])]
              }
            }
            
          } else if (phase1_method == "sustained") {
            in_w <- inds_in_window
            deriv_vals <- percap_deriv[in_w]
            cond <- !is.na(deriv_vals) & (deriv_vals >= min_deriv_for_sustained)
            if (any(cond)) {
              r <- rle(cond)
              lengths <- r$lengths; vals <- r$values
              ends <- cumsum(lengths)
              starts <- ends - lengths + 1
              candidate_runs <- which(vals & (lengths >= sustained_consec))
              if (length(candidate_runs) > 0) {
                run_idx <- candidate_runs[1]
                start_pos <- starts[run_idx]
                run_inds <- in_w[start_pos:(start_pos + lengths[run_idx] - 1)]
                phase1_idx <- run_inds[which.max(percap_deriv[run_inds])]
              }
            }
          }
          
          if (!is.na(phase1_idx)) {
            max_percap1 <- percap_deriv[phase1_idx]
            max_percap_time1 <- time_h[phase1_idx]
            max_percap_dens1 <- sm_mean[phase1_idx]
            lag_time1 <- tryCatch(
              lag_time(x = time_h[1:phase1_idx], y = sm_mean[1:phase1_idx], deriv = percap_deriv[1:phase1_idx], blank = 0),
              error = function(e) NA_real_,
              warning = function(w) NA_real_
            )
          }
        }
        
        # fallback to global max if requested
        if (is.na(phase1_idx) && fallback_to_global) {
          gm <- which_max_gc(percap_deriv)
          if (length(gm) > 0) {
            idx <- gm[1]
            phase1_idx <- idx
            max_percap1 <- percap_deriv[idx]
            max_percap_time1 <- time_h[idx]
            max_percap_dens1 <- sm_mean[idx]
            lag_time1 <- tryCatch(
              lag_time(x = time_h[1:idx], y = sm_mean[1:idx], deriv = percap_deriv[1:idx], blank = 0),
              error = function(e) NA_real_,
              warning = function(w) NA_real_
            )
          }
        }
        
        tibble(phase1_idx = phase1_idx,
               lag_time1 = lag_time1,
               max_percap1 = max_percap1,
               max_percap_time1 = max_percap_time1,
               max_percap_dens1 = max_percap_dens1)
      },
      
      # --- Diauxic shift detection (first minimum AFTER phase1, before max_shift_time) ---
      {
        diauxie_idx  <- NA_integer_
        diauxie_time <- NA_real_
        diauxie_dens <- NA_real_
        
        if (!is.na(first(treatment)) && (first(treatment) %in% two_phase_treatments) && !is.na(phase1_idx)) {
          min_candidates <- find_local_extrema(x = time_h, y = percap_deriv,
                                               return = "index", return_maxima = FALSE, return_minima = TRUE,
                                               window_width_n = window_width_n)
          min_candidates <- min_candidates[min_candidates > phase1_idx & time_h[min_candidates] < max_shift_time]
          if (length(min_candidates) > 0) {
            diauxie_idx  <- min_candidates[1]
            diauxie_time <- time_h[diauxie_idx]
            diauxie_dens <- sm_mean[diauxie_idx]
          }
        }
        
        tibble(diauxie_idx = diauxie_idx, diauxie_time = diauxie_time, diauxie_dens = diauxie_dens)
      },
      
      # --- Phase 2 detection (first local max after diauxie) ---
      {
        max_percap2 <- NA_real_; max_percap_time2 <- NA_real_; max_percap_dens2 <- NA_real_; lag_time2 <- NA_real_
        
        if (!is.na(diauxie_idx) && !is.na(first(treatment)) && (first(treatment) %in% two_phase_treatments)) {
          max2_candidates <- find_local_extrema(x = time_h, y = percap_deriv,
                                                return = "index", return_maxima = TRUE, return_minima = FALSE,
                                                window_width_n = 25) # wider window for phase 2
          max2_candidates <- max2_candidates[max2_candidates > diauxie_idx]
          
          # apply per-group min phase2 restriction if present (joined as eff_min_phase2_time)
          if (!is.na(eff_min_phase2_time)) {
            max2_candidates <- max2_candidates[time_h[max2_candidates] >= eff_min_phase2_time]
          } else if (!is.null(min_phase2_time)) {
            # fallback to scalar argument if provided
            max2_candidates <- max2_candidates[time_h[max2_candidates] >= min_phase2_time]
          }
          
          if (length(max2_candidates) > 0) {
            max2_idx <- max2_candidates[1]
            max_percap2 <- percap_deriv[max2_idx]
            max_percap_time2 <- time_h[max2_idx]
            max_percap_dens2 <- sm_mean[max2_idx]
            lag_time2 <- tryCatch(
              lag_time(x = time_h[max2_idx:length(time_h)], y = sm_mean[max2_idx:length(time_h)], deriv = percap_deriv[max2_idx:length(time_h)], blank = 0),
              error = function(e) NA_real_,
              warning = function(w) NA_real_
            )
          }
        }
        
        tibble(max_percap2 = max_percap2, max_percap_time2 = max_percap_time2, max_percap_dens2 = max_percap_dens2, lag_time2 = lag_time2)
      },
      
      # --- Other metrics ---
      max_dens = max(sm_mean, na.rm = TRUE),
      min_dens = min_gc(sm_mean),
      auc = auc(y = sm_mean, x = as.numeric(time_h)),
      .groups = "drop"
    ) %>%
    relocate(eff_min_time_for_phase1, eff_max_time_for_phase1, eff_min_phase2_time, .before = phase1_idx)
}

# -------------------------
# 6) Apply summarizer on the *_test2 (prejoined) data
#    Try phase1_method = "max_in_window" first; switch to "sustained" if noisy spikes wrongly chosen
# -------------------------
data_sum_f199  <- summarize_plate_two_phase_prejoined(f199_test2,  phase1_method = "max_in_window", fallback_to_global = FALSE)
data_sum_both  <- summarize_plate_two_phase_prejoined(both_test2,  phase1_method = "max_in_window", fallback_to_global = FALSE)
data_sum_g7    <- summarize_plate_two_phase_prejoined(g7_test2,    phase1_method = "max_in_window", fallback_to_global = FALSE)
data_sum_g72   <- summarize_plate_two_phase_prejoined(g72_test2,   phase1_method = "max_in_window", fallback_to_global = FALSE)
data_sum_f1992 <- summarize_plate_two_phase_prejoined(f1992_test2, phase1_method = "max_in_window", fallback_to_global = FALSE)

# re-create gc_test / gc_test2 for downstream code
gc_test  <- bind_rows(f199_test2, both_test2, g7_test2, g72_test2, f1992_test2)
gc_test2 <- bind_rows(data_sum_f199, data_sum_both, data_sum_g7, data_sum_g72, data_sum_f1992)

# -------------------------
# 7) Summary table (means ± SE) and flextable
# -------------------------
sum_gc <- gc_test2 %>%
  group_by(strain, media, treatment) %>%
  summarise(
    growthrate1_mean = mean(max_percap1, na.rm = TRUE),
    growthrate1_se   = sd(max_percap1, na.rm = TRUE) / sqrt(sum(!is.na(max_percap1))),
    growthrate2_mean = mean(max_percap2, na.rm = TRUE),
    growthrate2_se   = sd(max_percap2, na.rm = TRUE) / sqrt(sum(!is.na(max_percap2))),
    dens_mean       = mean(max_dens, na.rm = TRUE),
    dens_se         = sd(max_dens, na.rm = TRUE) / sqrt(sum(!is.na(max_dens))),
    auc_mean        = mean(auc, na.rm = TRUE),
    auc_se          = sd(auc, na.rm = TRUE) / sqrt(sum(!is.na(auc))),
    lag1_mean       = mean(lag_time1, na.rm = TRUE),
    lag1_se         = sd(lag_time1, na.rm = TRUE) / sqrt(sum(!is.na(lag_time1))),
    .groups = "drop"
  ) %>%
  mutate(
    strain = recode(strain, n.aroma = "F199", p.putida = "G7"),
    treatment = recode(treatment, cap = "Capsule", chit = "Chitosan", free = "Planktonic")
  )

gc_flex <- flextable(sum_gc) %>%
  colformat_double(digits = 2) %>%
  separate_header() %>%
  align(align = "center", part = "all") %>%
  autofit() %>%
  theme_vanilla()

gc_flex <- labelizor(
  x = gc_flex, part = "header",
  labels = c("growthrate1" = "Growth Rate (Phase 1)",
             "growthrate2" = "Growth Rate (Phase 2)",
             "dens" = "Density",
             "auc" = "AUC",
             "lag1" = "Lag Time (Phase 1)",
             "mean" = "Mean",
             "se" = "SE",
             "strain" = "Strain",
             "media" = "Media",
             "treatment" = "Treatment")
)

# Print table to console as a quick check
print(sum_gc)
gc_flex

# -------------------------
# 8) Plotting per well (uses gc_test and gc_test2) and export HTML
# -------------------------
plots <- list()
group_vars <- c("strain", "media", "treatment", "well", "plate.id")

gc_test %>%
  group_by(across(all_of(group_vars))) %>%
  group_walk(~{
    data_group <- .x
    keys <- .y
    group_name <- paste(paste(names(keys), keys, sep = "=", collapse = ", "))
    
    summ <- gc_test2 %>%
      filter(
        strain    == keys$strain,
        media     == keys$media,
        treatment == keys$treatment,
        well      == keys$well,
        plate.id  == keys$plate.id
      )
    
    p0 <- ggplot(data_group, aes(x = time_h, y = sm_mean)) +
      geom_point() +
      labs(title = paste("Corrected OD:", group_name), y = "Corrected OD", x = "Time")
    
    p1 <- ggplot(data_group, aes(x = time_h, y = log(sm_mean))) +
      geom_point() +
      # tangents for phase1/phase2 if present
      geom_abline(data = summ,
                  aes(slope = max_percap1,
                      intercept = log(max_percap_dens1) - max_percap1 * max_percap_time1),
                  color = "red", inherit.aes = FALSE) +
      geom_abline(data = summ %>% filter(!is.na(max_percap2)),
                  aes(slope = max_percap2,
                      intercept = log(max_percap_dens2) - max_percap2 * max_percap_time2),
                  color = "orange", inherit.aes = FALSE) +
      # show applied windows for QC (phase1 window and phase2 min if present)
      geom_vline(data = summ %>% filter(!is.na(eff_min_time_for_phase1)),
                 aes(xintercept = eff_min_time_for_phase1),
                 linetype = "dashed", color = "purple", inherit.aes = FALSE) +
      geom_vline(data = summ %>% filter(!is.na(eff_max_time_for_phase1)),
                 aes(xintercept = eff_max_time_for_phase1),
                 linetype = "dashed", color = "purple", inherit.aes = FALSE) +
      geom_vline(data = summ %>% filter(!is.na(eff_min_phase2_time)),
                 aes(xintercept = eff_min_phase2_time),
                 linetype = "dotted", color = "darkgreen", inherit.aes = FALSE) +
      # mark phase1 point on log plot
      geom_point(data = summ %>% filter(!is.na(max_percap1)),
                 aes(x = max_percap_time1, y = log(max_percap_dens1)),
                 color = "red", size = 3, inherit.aes = FALSE) +
      labs(title = "Log(corrected OD)", y = "log(corrected OD)", x = "Time")
    
    p2 <- ggplot(data_group, aes(x = time_h, y = percap_deriv)) +
      geom_line() +
      geom_point(data = summ, aes(x = max_percap_time1, y = max_percap1), color = "red", size = 2, inherit.aes = FALSE) +
      geom_point(data = summ %>% filter(!is.na(max_percap2)), aes(x = max_percap_time2, y = max_percap2), color = "orange", size = 2, inherit.aes = FALSE) +
      geom_vline(data = summ %>% filter(!is.na(diauxie_time)), aes(xintercept = diauxie_time), linetype = "dashed", color = "blue", inherit.aes = FALSE) +
      geom_vline(data = summ %>% filter(!is.na(eff_min_phase2_time)), aes(xintercept = eff_min_phase2_time), linetype = "dotted", color = "darkgreen", inherit.aes = FALSE) +
      labs(title = "Per-capita derivative", y = "Derivative", x = "Time") +
      coord_cartesian(ylim = c(-1, NA))
    
    combined <- cowplot::plot_grid(p0, p1, p2, ncol = 1, align = "v")
    plots[[group_name]] <<- combined
  })

# Export HTML with embedded PNGs
outdir <- tempdir()
htmltools::save_html(
  tagList(lapply(names(plots), function(nm) {
    f <- file.path(outdir, paste0(nm, ".png"))
    ggsave(f, plots[[nm]], width = 8, height = 8, dpi = 120)
    tags$div(style = "page-break-after: always;", tags$h2(nm), tags$img(src = f, style = "width:100%;"))
  })),
  file = "growthcurve_comparison_two_phase_smoothed12.html"
)

message("Done; HTML written to working directory: growthcurve_comparison_two_phase_smoothed12.html")
