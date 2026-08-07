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

data_location2 <- here::here("data", "f199chit_20241011.xlsx")
data_location3 <- here::here("data", "gc_minusa_norm_20240930.xlsx")
data_location4 <- here::here("data", "g7chit_20241011.xlsx")
data_location5 <- here::here("data", "g7chit_clean_202410126.xlsx")
data_location6 <- here::here("data", "f199chit_clean_202410131.xlsx")


#load data
f199 <- read_excel(data_location2) %>%
  mutate(plate.id = "f199") %>% filter(time_h > 0.1)

both <- read_excel(data_location3) %>%
  mutate(plate.id = "both") %>% filter(time_h > 0.1)

g7 <- read_excel(data_location4) %>%
  mutate(plate.id = "g7") %>% filter(time_h > 0.1)

g72 <- read_excel(data_location5) %>%
  mutate(plate.id = "g72") %>% filter(time_h > 0.1)

f1992 <- read_excel(data_location6) %>%
  mutate(plate.id = "f1992") %>% filter(time_h > 0.1)

# -------------------------
# OPTIONAL: your phase1_time_map (edit to match your desired groups)
# Add optional columns: min_phase2_time and max_phase2_time
# -------------------------
phase1_time_map <- tibble::tribble(
  ~strain, ~media, ~treatment, ~min_time, ~max_time, ~min_phase2_time, ~max_phase2_time,
  "n.aroma", "sRB15", "cap", 0, 20, 20, NA,
  "n.aroma", "sRB15", "free", 10, 65, NA, NA, 
  "n.aroma", "LB", "chit", 0, 20, 20, 40, 
  "n.aroma", "sRB15", "chit", 0, 20, 20, NA,
  "n.aroma", "LB", "free", 0, 30, NA, NA, 
  "p.putida", "LB", "cap", 0, 4, 4, 10, 
  "p.putida", "LB", "chit", 0, 4, 4, 10, 
  "p.putida", "sRB15", "cap", 0, 25, 25, NA,
  "p.putida", "sRB15", "chit", 0, 10, 10, NA
  
)

# Apply a small pseudocount (1e-5) to the corrected OD (prevents downstream issues with zero values)
f199  <- f199  %>% mutate(corrected = corrected + .001)
both  <- both  %>% mutate(corrected = corrected + .001)
g7    <- g7    %>% mutate(corrected = corrected + .001)
g72   <- g72   %>% mutate(corrected = corrected + .001)
f1992 <- f1992 %>% mutate(corrected = corrected + .001)

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
calc_plate_deriv <- function(df, window = 3, od_threshold = 0.0009) {
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
# Normalize map: trim strings and coerce numeric times (includes max_phase2_time)
phase1_time_map2 <- phase1_time_map %>%
  mutate(across(c(strain, media, treatment),
                ~ ifelse(is.na(.), NA_character_, str_trim(as.character(.)))),
         min_time = if ("min_time" %in% names(.) ) as.numeric(min_time) else NA_real_,
         max_time = if ("max_time" %in% names(.) ) as.numeric(max_time) else NA_real_,
         min_phase2_time = if ("min_phase2_time" %in% names(.)) as.numeric(min_phase2_time) else NA_real_,
         max_phase2_time = if ("max_phase2_time" %in% names(.)) as.numeric(max_phase2_time) else NA_real_)

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
    return(tibble(eff_min = default_min, eff_max = default_max, eff_min_phase2 = NA_real_, eff_max_phase2 = NA_real_, matched_row = NA_integer_))
  }
  
  matched <- map_df %>%
    mutate(.row = row_number()) %>%
    filter(
      (is.na(strain) | strain == s_val) &
        (is.na(media)  | media == m_val) &
        (is.na(treatment) | treatment == t_val)
    )
  
  if (nrow(matched) == 0) {
    return(tibble(eff_min = default_min, eff_max = default_max, eff_min_phase2 = NA_real_, eff_max_phase2 = NA_real_, matched_row = NA_integer_))
  }
  
  matched <- matched %>%
    mutate(spec = (!is.na(strain)) + (!is.na(media)) + (!is.na(treatment))) %>%
    arrange(desc(spec), .row)
  
  best <- matched %>% slice(1)
  eff_min <- if (!is.na(best$min_time)) best$min_time else default_min
  eff_max <- if (!is.na(best$max_time)) best$max_time else default_max
  eff_min_phase2 <- if (!is.na(best$min_phase2_time)) best$min_phase2_time else NA_real_
  eff_max_phase2 <- if (!is.na(best$max_phase2_time)) best$max_phase2_time else NA_real_
  
  tibble(eff_min = eff_min, eff_max = eff_max, eff_min_phase2 = eff_min_phase2, eff_max_phase2 = eff_max_phase2, matched_row = best$.row)
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
         eff_min_phase2_time = eff_min_phase2,
         eff_max_phase2_time = eff_max_phase2)

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
                                       eff_min_time_for_phase1, eff_max_time_for_phase1,
                                       eff_min_phase2_time, eff_max_phase2_time),
              by = c("strain", "media", "treatment"))
}

f199_test2  <- join_windows(f199_test,  group_windows)
both_test2  <- join_windows(both_test,  group_windows)
g7_test2    <- join_windows(g7_test,    group_windows)
g72_test2   <- join_windows(g72_test,   group_windows)
f1992_test2 <- join_windows(f1992_test, group_windows)


##Exclude treatments not exhibiting two-phase from calculating diauxic shift
# Returns TRUE if the given (strain, media, treatment) matches any row of exclude_df.
# exclude_df may contain NA in columns to act as wildcards.
is_excluded_two_phase <- function(s_val, m_val, t_val, exclude_df) {
  if (is.null(exclude_df)) return(FALSE)
  # ensure columns exist
  cols <- c("strain","media","treatment")
  if (!all(cols %in% names(exclude_df))) stop("exclude_two_phase must have columns: strain, media, treatment")
  # compare with wildcard NA allowed in exclude_df
  any(apply(exclude_df, 1, function(row) {
    (is.na(row["strain"]) || as.character(row["strain"]) == as.character(s_val)) &&
      (is.na(row["media"])  || as.character(row["media"])  == as.character(m_val)) &&
      (is.na(row["treatment"]) || as.character(row["treatment"]) == as.character(t_val))
  }))
}

exclude_tbl <- tibble::tribble(
  ~strain,    ~media, ~treatment,
  #"p.putida", "LB",   "cap",
  #"p.putida", "LB",   "chit"
)


# -------------------------
# 5) Summarizer (prejoined) -- robust version with multiple phase1 methods
# -------------------------
summarize_plate_two_phase_prejoined <- function(df,
                                                max_shift_time = 50,
                                                two_phase_treatments = c("cap", "chit"),
                                                exclude_two_phase = NULL,
                                                # phase1 thresholds
                                                min_od_for_phase1 = 0,
                                                min_percap_rate = 0,
                                                fallback_to_global = FALSE,
                                                # method options: "extrema", "max_in_window", "sustained"
                                                phase1_method = c("extrema", "max_in_window", "sustained"),
                                                min_deriv_for_sustained = 0.005,
                                                sustained_consec = 3,
                                                # optional per-group column has been pre-joined as eff_min_phase2_time / eff_max_phase2_time
                                                min_phase2_time = NULL,
                                                max_phase2_time = NULL,
                                                window_width_n = 15) {
  
  phase1_method <- match.arg(phase1_method)
  
  df %>%
    group_by(strain, media, treatment, well, plate.id) %>%
    summarize(
      # bring through the effective window (first() so scalar)
      eff_min_time_for_phase1 = first(eff_min_time_for_phase1),
      eff_max_time_for_phase1 = first(eff_max_time_for_phase1),
      eff_min_phase2_time     = first(eff_min_phase2_time),
      eff_max_phase2_time     = first(eff_max_phase2_time),
      
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
            min_dens = first_minima(sm_mean, return = "y")
            lag_time1 <- tryCatch(
              lag_time(x = time_h[1:phase1_idx], y = sm_mean[1:phase1_idx],
                       deriv = percap_deriv[1:phase1_idx], blank = 0, y0 = min_dens),
              error = function(e) NA_real_,
              warning = function(w) NA_real_
            )
            
            # --- Manual fallback if lag_time() returns NA ---
            if (is.na(lag_time1) && !is.na(max_percap1) && !is.na(max_percap_time1) && 
                !is.na(max_percap_dens1) && !is.na(min_dens)) {
              
              compute_lag_manual <- function(slope, t_phase, y_phase, y0, first_time) {
                if (is.na(slope) || slope <= 0 || any(is.na(c(t_phase, y_phase, y0)))) return(NA_real_)
                intercept <- log(y_phase) - slope * t_phase
                t_lag <- (log(y0) - intercept) / slope
                if (is.finite(t_lag) && t_lag >= first_time) return(t_lag) else return(NA_real_)
              }
              
              lag_time_manual <- compute_lag_manual(
                slope = max_percap1,
                t_phase = max_percap_time1,
                y_phase = max_percap_dens1,
                y0 = min_dens,
                first_time = time_h[1]
              )
              
              # Use manual lag time if valid
              if (!is.na(lag_time_manual)) lag_time1 <- lag_time_manual
            }
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
        
        # get current group values (first() returns scalar inside summarize)
        cur_treatment <- first(treatment)
        cur_media     <- first(media)
        cur_strain    <- first(strain)
        
        # skip detection if treatment not allowed OR if explicitly excluded
        skip_two_phase <- TRUE
        if (!is.na(cur_treatment) && (cur_treatment %in% two_phase_treatments)) {
          # allowed by treatment list; next check exclude table
          if (!is_excluded_two_phase(cur_strain, cur_media, cur_treatment, exclude_two_phase)) {
            skip_two_phase <- FALSE
          }
        }
        
        if (!skip_two_phase && !is.na(phase1_idx)) {
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
      
      
      # --- Phase 2 detection (robust bump detection using trough-to-peak depth) ---
      {
        max_percap2 <- NA_real_; max_percap_time2 <- NA_real_; max_percap_dens2 <- NA_real_; lag_time2 <- NA_real_
        
        # PARAMETERS YOU CAN TUNE
        extrema_window_phase2 <- 7    # window for find_local_extrema for phase-2 (small enough to catch short bumps)
        min_depth_abs <- 0.03         # absolute derivative depth between trough and candidate to accept
        min_depth_rel <- 0.35         # relative depth compared to phase1 peak to accept (fraction of max_percap1)
        require_sustained <- TRUE     # require at least `sustained_n` points near candidate to reduce false positives
        sustained_n <- 2              # how many consecutive points around the candidate should be close to the peak
        
        # do detection if we have a phase1 index (or if we want to allow direct search even without diauxie)
        # Note: we allow detection even if diauxie_idx is NA but phase1_idx exists.
        if (!is.na(phase1_idx) && !is.na(first(treatment)) && (first(treatment) %in% two_phase_treatments)) {
          
          # find local maxima after phase1
          cand_maxima <- find_local_extrema(x = time_h, y = percap_deriv,
                                            return = "index", return_maxima = TRUE, return_minima = FALSE,
                                            window_width_n = extrema_window_phase2)
          cand_maxima <- cand_maxima[cand_maxima > phase1_idx]
          
          # apply per-group / passed-in time bounds (same as before)
          if (!is.na(eff_min_phase2_time)) {
            cand_maxima <- cand_maxima[time_h[cand_maxima] >= eff_min_phase2_time]
          } else if (!is.null(min_phase2_time)) {
            cand_maxima <- cand_maxima[time_h[cand_maxima] >= min_phase2_time]
          }
          if (!is.na(eff_max_phase2_time)) {
            cand_maxima <- cand_maxima[time_h[cand_maxima] <= eff_max_phase2_time]
          } else if (!is.null(max_phase2_time)) {
            cand_maxima <- cand_maxima[time_h[cand_maxima] <= max_phase2_time]
          }
          
          # Evaluate each candidate by trough-to-peak depth
          accepted_idx <- integer(0)
          if (length(cand_maxima) > 0) {
            for (ci in cand_maxima) {
              rng <- (phase1_idx+1):ci
              rng <- rng[rng <= length(percap_deriv)]
              # skip if no valid range
              if (length(rng) < 1) next
              
              # compute trough between phase1 and candidate (ignore NA)
              trough_val <- suppressWarnings(min(percap_deriv[rng], na.rm = TRUE))
              if (!is.finite(trough_val)) next
              
              peak_val <- percap_deriv[ci]
              # depth measures
              depth_abs <- peak_val - trough_val
              depth_rel <- if (!is.na(max_percap1) && max_percap1 > 0) depth_abs / max_percap1 else NA_real_
              
              # Optionally require some sustained support around the peak
              sustained_ok <- TRUE
              if (require_sustained) {
                # define small neighborhood around candidate
                neigh <- (ci - (sustained_n-1)):(ci + (sustained_n-1))
                neigh <- neigh[neigh > phase1_idx & neigh <= length(percap_deriv)]
                # count how many points are within 80% of peak_val
                if (length(neigh) < 1) {
                  sustained_ok <- FALSE
                } else {
                  close_count <- sum(percap_deriv[neigh] >= (0.80 * peak_val), na.rm = TRUE)
                  sustained_ok <- (close_count >= sustained_n)
                }
              }
              
              # Accept candidate if depth passes either absolute OR relative threshold AND sustained_ok
              if (sustained_ok && ( (depth_abs >= min_depth_abs) || (!is.na(depth_rel) && depth_rel >= min_depth_rel) )) {
                accepted_idx <- c(accepted_idx, ci)
              }
            } # end for each candidate
          } # end if length(cand_maxima) > 0
          
          # pick earliest accepted candidate (you can choose highest instead if preferred)
          if (length(accepted_idx) > 0) {
            max2_idx <- accepted_idx[1]
            max_percap2 <- percap_deriv[max2_idx]
            max_percap_time2 <- time_h[max2_idx]
            max_percap_dens2 <- sm_mean[max2_idx]
            lag_time2 <- tryCatch(
              lag_time(x = time_h[max2_idx:length(time_h)], y = sm_mean[max2_idx:length(time_h)], deriv = percap_deriv[max2_idx:length(time_h)], blank = 0),
              error = function(e) NA_real_,
              warning = function(w) NA_real_
            )
          }
        } # end if phase1 exists & treatment allowed
        
        tibble(max_percap2 = max_percap2, max_percap_time2 = max_percap_time2, max_percap_dens2 = max_percap_dens2, lag_time2 = lag_time2)
      },
      
      
      # --- Other metrics ---
      max_dens = max(sm_mean, na.rm = TRUE),
      min_dens = min_gc(sm_mean),
      auc = {
        # choose end time based on strain for this group
        s <- first(strain)
        end_time <- if (!is.na(s) && s == "n.aroma") {
          113
        } else if (!is.na(s) && s == "p.putida") {
          71
        } else {
          72   # fallback default
        }
        
        t_sub <- time_h[time_h >= 0 & time_h <= end_time]
        y_sub <- sm_mean[time_h >= 0 & time_h <= end_time]
        if (length(t_sub) > 1) {
          auc(y = y_sub, x = t_sub)
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    ) %>%
    relocate(eff_min_time_for_phase1, eff_max_time_for_phase1, eff_min_phase2_time, eff_max_phase2_time, .before = phase1_idx)
}

# -------------------------
# 6) Apply summarizer on the *_test2 (prejoined) data
#    Try phase1_method = "max_in_window" first; switch to "sustained" if noisy spikes wrongly chosen
# -------------------------
data_sum_f199  <- summarize_plate_two_phase_prejoined(f199_test2,  phase1_method = "max_in_window", fallback_to_global = FALSE)
data_sum_both  <- summarize_plate_two_phase_prejoined(both_test2,  phase1_method = "max_in_window", fallback_to_global = FALSE, exclude_two_phase = exclude_tbl)
data_sum_g7    <- summarize_plate_two_phase_prejoined(g7_test2,    phase1_method = "max_in_window", fallback_to_global = FALSE, exclude_two_phase = exclude_tbl)
data_sum_g72   <- summarize_plate_two_phase_prejoined(g72_test2,   phase1_method = "max_in_window", fallback_to_global = FALSE, exclude_two_phase = exclude_tbl)
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
    nreps          = round(n()),
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
    #lag2_mean       = mean(lag_time2, na.rm = TRUE),
    #lag2_se         = sd(lag_time2, na.rm = TRUE) / sqrt(sum(!is.na(lag_time2))),
    shifttime_mean = mean(diauxie_time, na.rm = TRUE),
    shifttime_se   = sd(diauxie_time, na.rm = TRUE) / sqrt(sum(!is.na(diauxie_time))),
    #nphase2        = sum(!is.na(diauxie_time)),
    #frac_phase2     = n_phase2 / n_reps,
    maxpercap_time1_mean = mean(max_percap_time1, na.rom = TRUE), 
    maxpercap_time1_se = sd(max_percap_time1, na.rm = TRUE) / sqrt(sum(!is.na(max_percap_time1))),
    .groups = "drop"
  ) %>%
  mutate(
    strain = recode(strain, n.aroma = "N. aroma", p.putida = "P. putida"),
    treatment = recode(treatment, cap = "Capsule", chit = "Chitosan", free = "Planktonic")
  ) %>% #converts NaN to NA
  mutate(across(everything(), ~ { 
    if (is.numeric(.)) {
      .[is.nan(.)] <- NA_real_
      .
    } else {
      # also convert empty-string placeholders to NA for character columns
      .[. == ""] <- NA
      .
    }
  }))

gc_flex <- flextable(sum_gc) %>%
  colformat_double(digits = 2, na_str = "—") %>%
  colformat_double(j = "nreps", digits = 0) %>%
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
             "lag2" = "Lag Time (Phase 2)",
             "shifttime" = "Phase-Shift Time (h)",
             "mean" = "Mean",
             "se" = "SE",
             "strain" = "Strain",
             "media" = "Media",
             "treatment" = "Treatment", 
             "nreps" = "n")) %>%
  italic(i = ~ strain %in% c("N. aroma", "P. putida"), j = "strain")

# Print table to console as a quick check
print(sum_gc)
glimpse(sum_gc)
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
      # show applied windows for QC (phase1 window and phase2 min/max if present)
      geom_vline(data = summ %>% filter(!is.na(eff_min_time_for_phase1)),
                 aes(xintercept = eff_min_time_for_phase1),
                 linetype = "dashed", color = "purple", inherit.aes = FALSE) +
      geom_vline(data = summ %>% filter(!is.na(eff_max_time_for_phase1)),
                 aes(xintercept = eff_max_time_for_phase1),
                 linetype = "dashed", color = "purple", inherit.aes = FALSE) +
      geom_vline(data = summ %>% filter(!is.na(eff_min_phase2_time)),
                 aes(xintercept = eff_min_phase2_time),
                 linetype = "dotted", color = "darkgreen", inherit.aes = FALSE) +
      geom_vline(data = summ %>% filter(!is.na(eff_max_phase2_time)),
                 aes(xintercept = eff_max_phase2_time),
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
      #geom_vline(data = summ %>% filter(!is.na(eff_min_phase2_time)), aes(xintercept = eff_min_phase2_time), linetype = "dotted", color = "darkgreen", inherit.aes = FALSE) +
      #geom_vline(data = summ %>% filter(!is.na(eff_max_phase2_time)), aes(xintercept = eff_max_phase2_time), linetype = "dotted", color = "darkgreen", inherit.aes = FALSE) +
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
  file = "growthcurve_comparison_two_phase_smoothed18.html"
)

message("Done; HTML written to working directory: growthcurve_comparison_two_phase_smoothed12.html")

##STATS##

library(dplyr)
library(purrr)
library(broom)
library(rstatix)    # tukey_hsd()
library(agricolae)  # HSD.test()

# Safe runner: ANOVA of response ~ treatment within a given strain & media
run_anova_within_media <- function(df, strain_name, media_name, response_var) {
  dat <- df %>%
    filter(strain == strain_name, media == media_name) %>%
    filter(!is.na(.data[[response_var]]))
  
  n_obs <- nrow(dat)
  n_treatment <- n_distinct(dat$treatment)
  treatment_levels <- sort(unique(dat$treatment))
  
  # Return informative record if insufficient data
  if (n_obs == 0) {
    return(tibble(
      status = "no_data",
      strain = strain_name, media = media_name, variable = response_var,
      n_obs = n_obs, n_treatment = n_treatment,
      treatment_levels = paste(treatment_levels, collapse = ";"),
      anova = list(NULL), tukey = list(NULL), letters = list(NULL)
    ))
  }
  if (n_treatment < 2) {
    return(tibble(
      status = "insufficient_treatment_levels",
      strain = strain_name, media = media_name, variable = response_var,
      n_obs = n_obs, n_treatment = n_treatment,
      treatment_levels = paste(treatment_levels, collapse = ";"),
      anova = list(NULL), tukey = list(NULL), letters = list(NULL)
    ))
  }
  # require at least 2 non-NA obs per treatment ideally (optional check)
  per_tr_counts <- dat %>% group_by(treatment) %>% summarise(n = n(), .groups = "drop")
  if (any(per_tr_counts$n < 2)) {
    # still attempt model but warn in returned status
    warning_note <- paste0("Some treatment groups have <2 observations: ",
                           paste(per_tr_counts$treatment[per_tr_counts$n < 2], collapse = ";"))
  } else {
    warning_note <- NA_character_
  }
  
  # fit model safely
  res <- tryCatch({
    form <- as.formula(paste(response_var, "~ treatment"))
    model <- aov(form, data = dat)
    anova_summary <- broom::tidy(model)
    tukey <- tryCatch(tukey_hsd(model), error = function(e) tibble())
    # HSD.test expects the fitted model; produce groups table if possible
    hsd_letters <- tryCatch({
      tmp <- HSD.test(model, "treatment", group = TRUE)
      # tmp$groups is a data.frame with rownames as treatment levels
      groups_df <- as.data.frame(tmp$groups)
      # convert to tibble with a treatment column
      groups_tbl <- tibble::rownames_to_column(groups_df, var = "treatment")
      groups_tbl
    }, error = function(e) tibble())
    
    tibble(
      status = "ok",
      strain = strain_name, media = media_name, variable = response_var,
      n_obs = n_obs, n_treatment = n_treatment,
      treatment_levels = paste(treatment_levels, collapse = ";"),
      anova = list(anova_summary),
      tukey = list(tukey),
      letters = list(hsd_letters),
      note = warning_note
    )
  }, error = function(e) {
    tibble(
      status = "error",
      strain = strain_name, media = media_name, variable = response_var,
      n_obs = n_obs, n_treatment = n_treatment,
      treatment_levels = paste(treatment_levels, collapse = ";"),
      anova = list(NULL), tukey = list(NULL), letters = list(NULL),
      note = as.character(e$message)
    )
  })
  
  res
}

# Example variables you want to test (update as needed)
vars_to_test <- c("max_percap1", "max_dens", "auc", "lag_time1", "max_percap2")

# Build combos of strain × media × variable present in gc_test2
combos <- expand.grid(
  strain_name = unique(gc_test2$strain),
  media_name = unique(gc_test2$media),
  response_var = vars_to_test,
  stringsAsFactors = FALSE
) %>% as_tibble()

# Run all analyses safely and collect results as a tibble
all_results_media <- combos %>%
  mutate(result = pmap(list(strain_name, media_name, response_var),
                       ~ run_anova_within_media(gc_test2, ..1, ..2, ..3))) %>%
  unnest(result)


##global anova#
# --- Helper: run global factorial ANOVA for one response ----
run_global_anova <- function(df, response_var, contrasts_sum = TRUE, print_results = TRUE) {
  # ensure factor types (important)
  df2 <- df %>%
    mutate(
      strain = as.factor(strain),
      media = as.factor(media),
      treatment = as.factor(treatment)
    ) %>%
    filter(!is.na(.data[[response_var]]))
  
  # Optionally set sum-to-zero contrasts for Type III interpretation
  if (contrasts_sum) {
    # store old contrasts and restore on exit
    old_contrasts <- options("contrasts")
    options(contrasts = c("contr.sum", "contr.poly"))
    on.exit(options(old_contrasts), add = TRUE)
  }
  
  # Fit model using lm (works well with car::Anova Type III)
  form <- as.formula(paste(response_var, "~ strain * media * treatment"))
  fit_lm <- lm(form, data = df2)
  
  # Classical aov summary (sequential / Type I)
  aov_fit <- aov(fit_lm)
  aov_tab <- summary(aov_fit)
  
  # Type III (marginal) table via car::Anova (recommended if interactions present)
  # car::Anova expects contrasts set to contr.sum (we did that)
  type3_tab <- car::Anova(fit_lm, type = "III")
  
  
  # Optional: pairwise post-hoc with emmeans if main effect or interaction of interest is significant.
  # We'll compute emmeans for treatment within strain*media interaction (so you can extract contrasts within each cell)
  emms <- tryCatch({
    emmeans::emmeans(fit_lm, ~ treatment | strain * media)
  }, error = function(e) NULL)
  
  # If print_results = TRUE, display concise outputs
  if (print_results) {
    cat("\n==== Global ANOVA for response:", response_var, "====\n")
    cat("\n-- aov (Type I) summary --\n"); print(aov_tab)
    cat("\n-- Type III ANOVA (car::Anova) --\n"); print(type3_tab)
    cat("\n-- Residual checks --\n")
    cat(sprintf("Shapiro-Wilk p = %.4g (normality of residuals)\n", shapiro_p))
    cat(sprintf("Levene p = %.4g (homogeneity of variances across strain:media:treatment)\n", levene_p))
    if (!is.null(emms)) {
      cat("\n-- emmeans prepared for 'treatment | strain * media' (use emms to run pairwise) --\n")
    } else {
      cat("\n-- emmeans not available (model may have singularities) --\n")
    }
  }
  
  list(
    response = response_var,
    fit_lm = fit_lm,
    aov = aov_fit,
    type3 = type3_tab,
    residuals = resid_df,
    shapiro_p = shapiro_p,
    levene_p = levene_p,
    emmeans = emms
  )
}

# --- Example: run for a single response variable ---
res_global_maxpercap <- run_global_anova(gc_test2, "max_percap2")

# --- Run for multiple response variables in one go (vectorized) ---
vars_to_run <- c("max_percap2", "max_dens", "auc", "lag_time1", "max_percap2")  # adjust as needed

global_results <- map(vars_to_run, ~ run_global_anova(gc_test2, .x, contrasts_sum = TRUE, print_results = TRUE))
names(global_results) <- vars_to_run

##automate stats output
# Required packages
library(dplyr)
library(purrr)
library(broom)
library(emmeans)
library(glue)
library(tidyr)
library(stringr)

# helpers (same as before)
pformat <- function(p){
  if (is.na(p)) return("NA")
  star <- if      (p < 1e-4) "****"
  else if (p < 1e-3) "***"
  else if (p < 1e-2) "**"
  else if (p < 0.05) "*"
  else if (p < 0.1)  "†"
  else "NS"
  paste0(formatC(p, format="g", digits=3), " (", star, ")")
}

safe_aov <- function(df, response){
  if (n_distinct(df$treatment) < 2) return(list(ok = FALSE, reason = "only_one_treatment"))
  if (sum(!is.na(df[[response]])) < 2)  return(list(ok = FALSE, reason = "insufficient_obs"))
  fmla <- as.formula(paste(response, "~ treatment"))
  fit <- tryCatch(aov(fmla, data = df), error = function(e) e)
  if (inherits(fit, "error")) return(list(ok = FALSE, reason = paste0("fit_error: ", fit$message)))
  if (df.residual(fit) <= 0) return(list(ok = FALSE, reason = "no_residual_df"))
  list(ok = TRUE, fit = fit)
}

safe_tukey <- function(fit) {
  res <- tryCatch({
    em <- emmeans(fit, ~ treatment)
    pw <- pairs(em, adjust = "tukey")
    as.data.frame(pw)
  }, error = function(e) NULL)
  res
}

fmt_pairwise <- function(pw_df) {
  if (is.null(pw_df) || nrow(pw_df) == 0) return("pairwise not computable")
  pw_df %>%
    mutate(
      contrast = as.character(contrast),
      p = p.value,
      star = case_when(p < 1e-4 ~ "****",
                       p < 1e-3 ~ "***",
                       p < 1e-2 ~ "**",
                       p < 0.05  ~ "*",
                       p < 0.1   ~ "†",
                       TRUE      ~ "NS"),
      txt = glue("{contrast} — p={formatC(p, format='g', digits=3)} {star}")
    ) %>%
    pull(txt) %>%
    paste(collapse = "; ")
}

# corrected main function
gc<- gc_test2
gc$treatment <- factor(gc$treatment)
glimpse(gc)
make_grouped_stats_report <- function(gc,
                                      group_vars = c("strain","media"),
                                      responses = c("max_percap1","max_percap2","max_dens","auc","lag_time1","diauxie_time")) {
  # ensure grouping cols exist
  if (!all(group_vars %in% names(gc))) stop("group_vars not all present in gc")
  groups <- gc %>%
    distinct(across(all_of(group_vars))) %>%
    arrange(across(all_of(group_vars)))
  
  report_list <- list()
  for (i in seq_len(nrow(groups))) {
    # get group key-values as named list
    grow <- groups[i, , drop = TRUE]
    # build logical index for subsetting by equality on every group var
    cond <- rep(TRUE, nrow(gc))
    for (gv in group_vars) {
      cond <- cond & (gc[[gv]] == grow[[gv]])
    }
    subset_df <- gc[cond, , drop = FALSE]
    
    header <- paste(as.character(grow[[group_vars[1]]]), "-", as.character(grow[[group_vars[2]]]))
    entry <- list(header = header, results = list())
    
    for (resp in responses) {
      if (!resp %in% names(subset_df)) {
        entry$results[[resp]] <- list(status = "missing_column")
        next
      }
      safe <- safe_aov(subset_df, resp)
      if (!safe$ok) {
        entry$results[[resp]] <- list(status = "no_test", reason = safe$reason)
        next
      }
      fit <- safe$fit
      an_tbl <- tryCatch(broom::tidy(anova(fit)), error = function(e) NULL)
      treat_p <- NA_real_
      if (!is.null(an_tbl)) {
        r <- an_tbl %>% filter(str_detect(term, regex("^treatment", ignore_case = TRUE)))
        if (nrow(r) == 1) treat_p <- r$p.value else {
          r2 <- an_tbl %>% filter(!str_to_lower(term) %in% c("residuals","residual"))
          if (nrow(r2) >= 1) treat_p <- r2$p.value[1]
        }
      }
      tuk <- safe_tukey(fit)
      pair_txt <- if (is.null(tuk)) "pairwise not computable" else fmt_pairwise(tuk)
      entry$results[[resp]] <- list(
        status = "ok",
        anova_p = treat_p,
        anova_p_formatted = ifelse(is.na(treat_p), NA_character_, pformat(treat_p)),
        pairwise_text = pair_txt,
        pairwise_df = tuk
      )
    }
    report_list[[header]] <- entry
  }
  report_list
}

# printer
print_stats_report <- function(report_list, responses_labels = NULL){
  for (hdr in names(report_list)) {
    cat("\n", hdr, "\n")
    res <- report_list[[hdr]]$results
    for (resp in names(res)) {
      out <- res[[resp]]
      label <- if (!is.null(responses_labels) && resp %in% names(responses_labels)) responses_labels[[resp]] else resp
      cat(" ", label, "\n")
      if (out$status != "ok") {
        cat("   - NO TEST (", out$reason, ")\n")
      } else {
        cat("   - Global ANOVA p: ", out$anova_p_formatted, "\n")
        cat("   - Pairwise: ", out$pairwise_text, "\n")
      }
    }
    cat("\n--------------------------\n")
  }
}

# Usage example (adapt the responses/labels to your names)
responses <- c("max_percap1","max_percap2","max_dens","auc","lag_time1","diauxie_time")
labels <- c(max_percap1 = "Growth rate (Phase 1)",
            max_percap2 = "Growth rate (Phase 2)",
            max_dens = "Max density",
            auc = "AUC",
            lag_time1 = "Lag time (Phase 1)",
            diauxie_time = "Phase-shift time")

# Run on your data frame (gc_test2)
report <- make_grouped_stats_report(gc_test2, group_vars = c("strain","media"), responses = responses)
print_stats_report(report, responses_labels = labels)

