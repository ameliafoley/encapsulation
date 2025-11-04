library(gcplyr)
library(growthrates)
library(patchwork)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(flextable)

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

f199_test<- mutate(
  group_by(f199, well, plate.id), 
  percap_deriv = calc_deriv(y = corrected, x = time_h, 
                            percapita = TRUE, blank = 0, window_width_n = 5, trans_y = "log")
)
data_sum_f199 <- summarize(
  group_by(f199_test, strain, media, treatment, well, plate.id),
  lag_time = lag_time(x = time_h, y = corrected, deriv = percap_deriv, blank = 0),
  max_percap = max(percap_deriv, na.rm = TRUE),
  max_percap_time = time_h[which_max_gc(percap_deriv)],
  max_dens = max(corrected),
  max_percap_dens = corrected[which_max_gc(percap_deriv)],
  min_dens = min_gc(corrected),
  auc = auc(y = corrected, x = as.numeric(time_h)))

g7_test<- mutate(
  group_by(g7, well, plate.id), 
  percap_deriv = calc_deriv(y = corrected, x = time_h, 
                            percapita = TRUE, blank = 0, window_width_n = 5, trans_y = "log"), 
  doub_time = doubling_time(y = percap_deriv))

data_sum_g7 <- summarize(
  group_by(g7_test, strain, media, treatment, well, plate.id),
  lag_time = lag_time(x = time_h, y = corrected, deriv = percap_deriv, blank = 0),
  max_percap = max(percap_deriv, na.rm = TRUE),
  max_percap_time = time_h[which_max_gc(percap_deriv)],
  max_dens = max(corrected),
  max_percap_dens = corrected[which_max_gc(percap_deriv)],
  min_dens = min_gc(corrected),
  auc = auc(y = corrected, x = as.numeric(time_h)))

both_test<- mutate(
  group_by(both, well, plate.id), 
  percap_deriv = calc_deriv(y = corrected, x = time_h, 
                            percapita = TRUE, blank = 0, window_width_n = 5, trans_y = "log")
)
data_sum_both <- summarize(
  group_by(both_test, strain, media, treatment, well, plate.id),
  lag_time = lag_time(x = time_h, y = corrected, deriv = percap_deriv, blank = 0),
  max_percap = max(percap_deriv, na.rm = TRUE),
  max_percap_time = time_h[which_max_gc(percap_deriv)],
  max_dens = max(corrected),
  max_percap_dens = corrected[which_max_gc(percap_deriv)],
  min_dens = min_gc(corrected),
  auc = auc(y = corrected, x = as.numeric(time_h)))

g72_test<- mutate(
  group_by(g72, well, plate.id), 
  percap_deriv = calc_deriv(y = corrected, x = time_h, 
                            percapita = TRUE, blank = 0, window_width_n = 5, trans_y = "log")
)
data_sum_g72 <- summarize(
  group_by(g72_test, strain, media, treatment, well, plate.id),
  lag_time = lag_time(x = time_h, y = corrected, deriv = percap_deriv, blank = 0),
  max_percap = max(percap_deriv, na.rm = TRUE),
  max_percap_time = time_h[which_max_gc(percap_deriv)],
  max_dens = max(corrected),
  max_percap_dens = corrected[which_max_gc(percap_deriv)],
  min_dens = min_gc(corrected),
  auc = auc(y = corrected, x = as.numeric(time_h)))

f1992_test<- mutate(
  group_by(f1992, well, plate.id), 
  percap_deriv = calc_deriv(y = corrected, x = time_h, 
                            percapita = TRUE, blank = 0, window_width_n = 5, trans_y = "log")
)
data_sum_f1992 <- summarize(
  group_by(f1992_test, strain, media, treatment, well, plate.id),
  lag_time = lag_time(x = time_h, y = corrected, deriv = percap_deriv, blank = 0),
  max_percap = max(percap_deriv, na.rm = TRUE),
  max_percap_time = time_h[which_max_gc(percap_deriv)],
  max_dens = max(corrected),
  max_percap_dens = corrected[which_max_gc(percap_deriv)],
  min_dens = min_gc(corrected),
  auc = auc(y = corrected, x = as.numeric(time_h)))

gc_test<- rbind(f199_test, both_test, g7_test, g72_test, f1992_test)
gc_test2<- rbind(data_sum_g7, data_sum_g72, data_sum_f199, data_sum_f1992, data_sum_both)

gc_test2 %>% dplyr::select(strain, media, treatment) %>% distinct()
gc_test2 %>% group_by(strain, media, treatment) %>% summarize(count=n())
sum_gc<- gc_test2 %>% group_by(strain, media, treatment) %>% summarise(growthrate_mean = mean(max_percap), 
                                                                       growthrate_se = sd(max_percap) / sqrt(n()), 
                                                                       dens_mean = mean(max_dens), 
                                                                       dens_se = sd(max_dens)/sqrt(n()), 
                                                                       auc_mean = mean(auc), 
                                                                       auc_se = sd(auc)/sqrt(n()), 
                                                                       lag_mean = mean(lag_time), 
                                                                       lag_se = sd(lag_time)/sqrt(n()))
#changed the order of the sum_gc names for creating the table, but graphs are coded with original order - need to change in order for code to run
sum_gc<- sum_gc %>% mutate(strain = recode(strain, 
                                           n.aroma = "F199", 
                                           p.putida = "G7"), 
                           treatment = recode(treatment, 
                                              cap = "Capsule", 
                                              chit = "Chitosan", 
                                              free = "Planktonic")) #convert names for nicer looking table
#sumtable(gc_test2, group = c("strain", "media"))

gc_flex<- flextable(sum_gc) %>% colformat_double(digits = 2)

gc_flex<- gc_flex %>% separate_header() %>%
  align(align = "center", part = "all") %>%
  autofit() %>% theme_vanilla()

gc_flex<- labelizor(x = gc_flex, 
                    part = "header", 
                    labels = c("growthrate" = "Growth Rate", 
                               "dens" = "Density", 
                               "auc" = "AUC", 
                               "lag" = "Lag Time", 
                               "mean" = "Mean", 
                               "se" = "SE", 
                               "strain" = "Strain", 
                               "media" = "Media", 
                               "treatment" = "Treatment"))
gc_flex

##showing calculations via plots in file
library(ggplot2)
library(dplyr)
library(cowplot)    # <- NEW: for plot_grid
library(htmltools)

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
    
    ## --- Plot 0: raw corrected OD
    p0 <- ggplot(data_group, aes(x = time_h, y = corrected)) +
      geom_point() +
      labs(title = paste("Corrected OD:", group_name),
           y = "Corrected OD", x = "Time")
    
    ## --- Plot 1: log(corrected OD) with tangent
    p1 <- ggplot(data_group, aes(x = time_h, y = log(corrected))) +
      geom_point() +
      geom_abline(
        data = summ,
        aes(slope = max_percap,
            intercept = log(max_percap_dens) - max_percap * max_percap_time),
        color = "red", inherit.aes = FALSE
      ) +
      geom_vline(
        data = summ, aes(xintercept = lag_time),
        linetype = "dashed", color = "blue", inherit.aes = FALSE
      ) +
      geom_hline(
        data = summ, aes(yintercept = log(min_dens)),
        linetype = "dotted", color = "black", inherit.aes = FALSE
      ) +
      labs(title = "Log(corrected OD)",
           y = "log(corrected OD)", x = "Time")
    
    ## --- Plot 2: derivative over time
    p2 <- ggplot(data_group, aes(x = time_h, y = percap_deriv)) +
      geom_line() +
      geom_point(
        data = summ, aes(x = max_percap_time, y = max_percap),
        color = "red", size = 2, inherit.aes = FALSE
      ) +
      geom_vline(
        data = summ, aes(xintercept = lag_time),
        linetype = "dashed", color = "blue", inherit.aes = FALSE
      ) +
      labs(title = "Per-capita derivative",
           y = "Derivative", x = "Time") +
      coord_cartesian(ylim = c(-1, NA))
    
    ## --- Combine vertically: cowplot is more stable than patchwork here
    combined <- cowplot::plot_grid(p0, p1, p2, ncol = 1, align = "v")
    
    plots[[group_name]] <<- combined
  })

# non-interactive HTML output
outdir <- tempdir()

htmltools::save_html(
  tagList(lapply(names(plots), function(nm) {
    f <- file.path(outdir, paste0(nm, ".png"))
    # save combined grid to PNG
    ggsave(f, plots[[nm]], width = 8, height = 8, dpi = 120)
    tags$div(
      style = "page-break-after: always;",
      tags$h2(nm),
      tags$img(src = f, style = "width:100%;")
    )
  })),
  file = "growthcurve_comparison_3_redo.html"
)

##After inspecting plots for individual wells, it appears some smoothing is required. 
#For example, well B5 in F199 and F1992 plates exhibits variability/noise that causes the incorrect placement of lag time and max growth rate. 
#Let's apply a moving-median and moving-average approach as suggested by gcplyr. 

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

#smoothing
f199.sm <- f199 %>%
  mutate(group_by(f199, well, media, strain, treatment, plate.id), 
         smoothed_no=corrected, 
         sm_med3= smooth_data(x=time_h,y=corrected, sm_method="moving-median",window_width_n=3), 
         #Notethatforthesecondround,we'reusingthe 
         #firstsmoothingastheinputy 
         smoothed_yes= smooth_data(x=time_h,y=sm_med3, sm_method="moving-average",window_width_n=3))
f199.sm.wide<- 
  pivot_longer(f199.sm,cols=starts_with("smoothed"), 
               names_to="smoothed",
               names_prefix="smoothed_")

ggplot(data=dplyr::filter(f199.sm.wide), 
       aes(x=time_h,y=value,color=smoothed))+ 
  geom_line(linewidth=0.6,alpha= 0.75)+ 
  scale_color_grey(start=0.8,end=0)+ 
  facet_wrap(~well,scales="free_y")+ 
  geom_line(data=dplyr::filter(f199.sm.wide,smoothed=="no"), 
            lty=2,color="red") + #red = not smoothed
  #scale_y_log10()+ 
  ggtitle("medianthenaveragesmoothing")+ 
  theme_bw()

ggplot(data=dplyr::filter(f199.sm), 
       aes(x = time_h, y=sm_med3))+
  geom_line(linewidth=0.6,alpha= 0.75)+ 
  scale_color_grey(start=0.8,end=0)+ 
  facet_wrap(~well,scales="free_y")+ 
  geom_line(data=dplyr::filter(f199.sm, 
            lty=2,color="red")+
  #scale_y_log10()+ 
  ggtitle("median")+ 
  theme_bw()
  
  
  
  ggplot(f199.sm, aes(x = time_h)) +
    geom_line(aes(y = sm_med3, color = "sm_med3")) +
    geom_line(aes(y = smoothed_no, color = "smoothed_no")) +
    labs(
      x = "Time (h)",
      y = "Value",
      color = "Variable"
    ) +
    theme_minimal()+
    facet_wrap(~well,scales="free_y") 
    
  f199.sm <- f199 %>%
    group_by(well, media, strain, treatment, plate.id) %>%
    mutate(
      smoothed_no = corrected, 
      sm_med3     = smooth_data(x = time_h, y = corrected,
                                sm_method = "moving-median",
                                window_width_n = 11),
      sm_mean7    = smooth_data(x = time_h, y = corrected,
                                sm_method = "moving-average",
                                window_width_n = 11)
    ) %>%
    ungroup() %>%
    pivot_longer(cols = c(smoothed_no, sm_med3, sm_mean7),
                 names_to = "Variable", values_to = "Value")
  
  ggplot(f199.sm, aes(x = time_h, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~well + Variable) +
    labs(x = "Time (h)", y = "Value") +
    theme_minimal()
#focusing on 2 problem wells
  f199.sm %>%
    filter(well %in% c("B5", "C5")) %>%
    ggplot(aes(x = time_h, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap(~well, scales = "free_y") +
    labs(x = "Time (h)", y = "Value") +
    theme_minimal()
  #raw data in gray
  f199.sm %>%
    #filter(well %in% c("B5", "C5")) %>%
    ggplot(aes(x = time_h, y = Value)) +
    # draw raw in gray
    geom_line(data = ~filter(.x, Variable == "smoothed_no"),
              color = "gray80", linewidth = 0.6) +
    # draw smoothed in color
    geom_line(data = ~filter(.x, Variable != "smoothed_no"),
              aes(color = Variable), linewidth = 0.8) +
    facet_wrap(~well, scales = "free_y") +
    labs(x = "Time (h)", y = "Value", color = "Smoother") +
    theme_minimal()
  
  f199.sm %>%
     filter(well %in% c("B5", "C5")) %>%
    ggplot(aes(x = time_h, y = Value)) +
    # draw raw in gray + dashed
    geom_line(
      data = ~filter(.x, Variable == "smoothed_no"),
      color = "gray70", linewidth = 0.6, linetype = "solid"
    ) +
    # draw smoothed in color + solid
    geom_line(
      data = ~filter(.x, Variable != "smoothed_no"),
      aes(color = Variable), linewidth = 0.8, linetype = "dashed"
    ) +
    facet_wrap(~well, scales = "free_y") +
    labs(x = "Time (h)", y = "Value", color = "Smoother") +
    theme_minimal()
  #it looks like a moving average with a window of 5 will be a good smoothing method. Let's apply this to our workflow and compare results
  
  
  ##smoothing
  # helper function
  smooth_plate <- function(df) {
    df %>%
      group_by(well, media, strain, treatment, plate.id) %>%
      mutate(
        smoothed_no = corrected, 
        sm_med3     = smooth_data(
          x = time_h, y = corrected,
          sm_method = "moving-median",
          window_width_n = 3
        ),
        sm_mean    = smooth_data(
          x = time_h, y = corrected,
          sm_method = "moving-average",
          window_width_n = 5
        )
      ) %>%
      ungroup()
  }
  
  # apply to each plate
  f199.sm  <- smooth_plate(f199)
  both.sm  <- smooth_plate(both)
  g7.sm    <- smooth_plate(g7)
  g72.sm   <- smooth_plate(g72)
  f1992.sm <- smooth_plate(f1992)
  
  ##updated functions for gcplyr analysis
  # Derivatives based on smoothed + thresholded data
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
  
  # Summarize growth metrics from the same thresholded curve
  summarize_plate <- function(df) {
    df %>%
      group_by(strain, media, treatment, well, plate.id) %>%
      summarize(
        lag_time = lag_time(x = time_h, y = sm_mean_thres, deriv = percap_deriv, blank = 0),
        max_percap = max(percap_deriv, na.rm = TRUE),
        max_percap_time = time_h[which_max_gc(percap_deriv)],
        max_dens = max(sm_mean_thres, na.rm = TRUE),
        max_percap_dens = sm_mean_thres[which_max_gc(percap_deriv)],
        min_dens = min_gc(sm_mean_thres),
        auc = auc(y = sm_mean_thres, x = as.numeric(time_h)),
        .groups = "drop"
      )
  }
  
  
  
  
# apply to all plates
  # derivatives
  f199_test  <- calc_plate_deriv(f199.sm)
  both_test  <- calc_plate_deriv(both.sm)
  g7_test    <- calc_plate_deriv(g7.sm)
  g72_test   <- calc_plate_deriv(g72.sm)
  f1992_test <- calc_plate_deriv(f1992.sm)
  
  # summaries
  data_sum_f199  <- summarize_plate(f199_test)
  data_sum_both  <- summarize_plate(both_test)
  data_sum_g7    <- summarize_plate(g7_test)
  data_sum_g72   <- summarize_plate(g72_test)
  data_sum_f1992 <- summarize_plate(f1992_test)
  
#combine all results
  gc_test  <- bind_rows(f199_test, both_test, g7_test, g72_test, f1992_test)
  gc_test2 <- bind_rows(data_sum_f199, data_sum_both, data_sum_g7, data_sum_g72, data_sum_f1992)
  
# summary table
  sum_gc <- gc_test2 %>%
    group_by(strain, media, treatment) %>%
    summarise(
      growthrate_mean = mean(max_percap, na.rm = TRUE),
      growthrate_se   = sd(max_percap, na.rm = TRUE) / sqrt(n()),
      dens_mean       = mean(max_dens, na.rm = TRUE),
      dens_se         = sd(max_dens, na.rm = TRUE) / sqrt(n()),
      auc_mean        = mean(auc, na.rm = TRUE),
      auc_se          = sd(auc, na.rm = TRUE) / sqrt(n()),
      lag_mean        = mean(lag_time, na.rm = TRUE),
      lag_se          = sd(lag_time, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(
      strain = recode(strain,
                      n.aroma = "F199",
                      p.putida = "G7"),
      treatment = recode(treatment,
                         cap = "Capsule",
                         chit = "Chitosan",
                         free = "Planktonic")
    )
#flextable
  gc_flex <- flextable(sum_gc) %>%
    colformat_double(digits = 2) %>%
    separate_header() %>%
    align(align = "center", part = "all") %>%
    autofit() %>%
    theme_vanilla()
  
  gc_flex <- labelizor(
    x = gc_flex, part = "header",
    labels = c("growthrate" = "Growth Rate",
               "dens" = "Density",
               "auc" = "AUC",
               "lag" = "Lag Time",
               "mean" = "Mean",
               "se" = "SE",
               "strain" = "Strain",
               "media" = "Media",
               "treatment" = "Treatment")
  )
  gc_flex
  
## plots of individual wells and calculations for smoothed data
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
      
      ## --- Plot 0: raw corrected OD
      p0 <- ggplot(data_group, aes(x = time_h, y = sm_mean)) +
        geom_point() +
        labs(title = paste("Corrected OD:", group_name),
             y = "Corrected OD", x = "Time")
      
      ## --- Plot 1: log(corrected OD) with tangent
      p1 <- ggplot(data_group, aes(x = time_h, y = log(sm_mean))) +
        geom_point() +
        geom_abline(
          data = summ,
          aes(slope = max_percap,
              intercept = log(max_percap_dens) - max_percap * max_percap_time),
          color = "red", inherit.aes = FALSE
        ) +
        geom_vline(
          data = summ, aes(xintercept = lag_time),
          linetype = "dashed", color = "blue", inherit.aes = FALSE
        ) +
        geom_hline(
          data = summ, aes(yintercept = log(min_dens)),
          linetype = "dotted", color = "black", inherit.aes = FALSE
        ) +
        labs(title = "Log(corrected OD)",
             y = "log(corrected OD)", x = "Time")
      
      ## --- Plot 2: derivative over time
      p2 <- ggplot(data_group, aes(x = time_h, y = percap_deriv)) +
        geom_line() +
        geom_point(
          data = summ, aes(x = max_percap_time, y = max_percap),
          color = "red", size = 2, inherit.aes = FALSE
        ) +
        geom_vline(
          data = summ, aes(xintercept = lag_time),
          linetype = "dashed", color = "blue", inherit.aes = FALSE
        ) +
        labs(title = "Per-capita derivative",
             y = "Derivative", x = "Time") +
        coord_cartesian(ylim = c(-1, NA))
      
      ## --- Combine vertically: cowplot is more stable than patchwork here
      combined <- cowplot::plot_grid(p0, p1, p2, ncol = 1, align = "v")
      
      plots[[group_name]] <<- combined
    })
  
  # non-interactive HTML output
  outdir <- tempdir()
  
  htmltools::save_html(
    tagList(lapply(names(plots), function(nm) {
      f <- file.path(outdir, paste0(nm, ".png"))
      # save combined grid to PNG
      ggsave(f, plots[[nm]], width = 8, height = 8, dpi = 120)
      tags$div(
        style = "page-break-after: always;",
        tags$h2(nm),
        tags$img(src = f, style = "width:100%;")
      )
    })),
    file = "growthcurve_comparison_3_smooth_odcutoff.html"
  )
  
  
  phase1_time_map <- tibble::tribble(
    ~strain,     ~media,  ~treatment, ~min_time, ~max_time,
    "n.aroma",   "sRB15", "free",       10,        60,    # long lag, start at 10h, end at 60h
    NA,          NA,      "cap",         0,        20,    # cap treatments start immediately
    NA,          NA,      "chit",        0,        20,    # chit treatments same
    "n.aroma",   "LB",    "free",        0,        30     # LB medium shorter lag
  )

    ##attempt IDing and ploting 2-phase growth now that we have smoothed data
  ## --- Load libraries ---
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gcplyr)
  library(flextable)
  library(cowplot)
  library(htmltools)
  
## --- 1. Smoothing function --- 
  smooth_plate <- function(df) { df %>% 
      group_by(well, media, strain, treatment, plate.id) %>% 
      mutate( smoothed_no = corrected, 
              sm_med3 = smooth_data( x = time_h, y = corrected, sm_method = "moving-median", window_width_n = 7 ), 
              sm_mean = smooth_data( x = time_h, y = corrected, sm_method = "moving-average", window_width_n = 7 ) ) %>% 
      ungroup() } # Apply to all plates 
  f199.sm <- smooth_plate(f199) 
  both.sm <- smooth_plate(both) 
  g7.sm <- smooth_plate(g7) 
  g72.sm <- smooth_plate(g72) 
  f1992.sm <- smooth_plate(f1992) 
  ## --- 2. Calculate per-capita derivative --- 
  calc_plate_deriv <- function(df, window = 5, od_threshold = 0.01) { df %>% mutate(sm_mean_thres = ifelse(sm_mean < od_threshold, NA, sm_mean)) %>% 
      group_by(well, plate.id) %>% 
      mutate( percap_deriv = calc_deriv( y = sm_mean_thres, x = time_h, percapita = TRUE, blank = 0, window_width_n = window, trans_y = "log" ) ) %>% 
      ungroup() } 
  # Apply 
  f199_test <- calc_plate_deriv(f199.sm) 
  both_test <- calc_plate_deriv(both.sm) 
  g7_test <- calc_plate_deriv(g7.sm) 
  g72_test <- calc_plate_deriv(g72.sm) 
  f1992_test <- calc_plate_deriv(f1992.sm)
  
  # --- corrected: phase1_time_map default = NULL (not phase1_time_map) ---
  library(dplyr)
  library(stringr)
  
  summarize_plate_two_phase <- function(df, 
                                        max_shift_time = 50,
                                        two_phase_treatments = c("cap", "chit"),
                                        # global defaults for phase1 window
                                        min_time_for_phase1 = 0,
                                        max_time_for_phase1 = 15,
                                        min_od_for_phase1 = 0.0,
                                        min_percap_rate = 0.0,
                                        fallback_to_global = FALSE,
                                        phase1_time_map = NULL) {
    
    # ---- Validation of phase1_time_map (clear errors) ----
    if (!is.null(phase1_time_map)) {
      if (!is.data.frame(phase1_time_map)) {
        stop("phase1_time_map must be a data.frame or tibble or NULL.")
      }
      required_cols <- c("strain", "media", "treatment")
      missing_cols <- setdiff(required_cols, names(phase1_time_map))
      if (length(missing_cols) > 0) {
        stop("phase1_time_map is missing required columns: ", paste(missing_cols, collapse = ", "),
             ". Required columns: strain, media, treatment. Optional: min_time, max_time.")
      }
      # Optional numeric columns
      if ("min_time" %in% names(phase1_time_map) && !is.numeric(phase1_time_map$min_time)) {
        stop("phase1_time_map$min_time must be numeric (or NA).")
      }
      if ("max_time" %in% names(phase1_time_map) && !is.numeric(phase1_time_map$max_time)) {
        stop("phase1_time_map$max_time must be numeric (or NA).")
      }
      
      # Normalize text columns (trim whitespace, coerce to character) to reduce mismatches
      phase1_time_map <- phase1_time_map %>%
        mutate(across(all_of(c("strain","media","treatment")), ~ ifelse(is.na(.), NA_character_, str_trim(as.character(.)))))
    }
    
    # small helper: choose effective min/max from phase1_time_map for a single group
    get_eff_window <- function(s_val, m_val, t_val, map_df, default_min, default_max) {
      if (is.null(map_df) || nrow(map_df) == 0) {
        return(tibble(eff_min = default_min, eff_max = default_max))
      }
      
      # normalize group values too
      s_val <- ifelse(is.na(s_val), NA_character_, str_trim(as.character(s_val)))
      m_val <- ifelse(is.na(m_val), NA_character_, str_trim(as.character(m_val)))
      t_val <- ifelse(is.na(t_val), NA_character_, str_trim(as.character(t_val)))
      
      matched <- map_df %>%
        filter(
          (is.na(strain) | strain == s_val) &
            (is.na(media)  | media == m_val) &
            (is.na(treatment) | treatment == t_val)
        )
      
      if (nrow(matched) == 0) {
        return(tibble(eff_min = default_min, eff_max = default_max))
      }
      
      matched <- matched %>%
        mutate(spec = (!is.na(strain)) + (!is.na(media)) + (!is.na(treatment)))
      
      best <- matched %>% arrange(desc(spec)) %>% slice(1)
      eff_min <- if ("min_time" %in% names(best) && !is.na(best$min_time)) as.numeric(best$min_time) else default_min
      eff_max <- if ("max_time" %in% names(best) && !is.na(best$max_time)) as.numeric(best$max_time) else default_max
      
      tibble(eff_min = eff_min, eff_max = eff_max)
    }
    
    # ---- Main summarization (same logic as before, with per-group window lookup) ----
    df %>%
      group_by(strain, media, treatment, well, plate.id) %>%
      summarize(
        .strain = first(strain),
        .media = first(media),
        .treatment = first(treatment),
        
        { # lookup window
          win <- get_eff_window(.strain, .media, .treatment, phase1_time_map, min_time_for_phase1, max_time_for_phase1)
          tibble(eff_min_time_for_phase1 = win$eff_min, eff_max_time_for_phase1 = win$eff_max)
        },
        
        { # phase1 detection using the group window (prefer strongest candidate)
          max_candidates <- find_local_extrema(x = time_h, y = percap_deriv,
                                               return = "index", return_maxima = TRUE, return_minima = FALSE,
                                               window_width_n = 15)
          
          if (length(max_candidates) > 0) {
            max_candidates <- max_candidates[
              time_h[max_candidates] >= eff_min_time_for_phase1 &
                time_h[max_candidates] <= eff_max_time_for_phase1
            ]
          }
          
          phase1_idx <- NA_integer_
          max_percap1 <- NA_real_; max_percap_time1 <- NA_real_; max_percap_dens1 <- NA_real_; lag_time1 <- NA_real_
          
          if (length(max_candidates) > 0) {
            ok_cands <- max_candidates[
              !is.na(sm_mean[max_candidates]) & sm_mean[max_candidates] >= min_od_for_phase1 &
                !is.na(percap_deriv[max_candidates]) & percap_deriv[max_candidates] >= min_percap_rate
            ]
            if (length(ok_cands) > 0) {
              chosen <- ok_cands[which.max(percap_deriv[ok_cands])]
              idx <- chosen
              phase1_idx <- idx
              max_percap1 <- percap_deriv[idx]
              max_percap_time1 <- time_h[idx]
              max_percap_dens1 <- sm_mean[idx]
              lag_time1 <- lag_time(x = time_h[1:idx], y = sm_mean[1:idx], deriv = percap_deriv[1:idx], blank = 0)
            }
          }
          if (is.na(phase1_idx) && fallback_to_global) {
            gm <- which_max_gc(percap_deriv)
            if (length(gm) > 0) {
              idx <- gm[1]
              phase1_idx <- idx
              max_percap1 <- percap_deriv[idx]; max_percap_time1 <- time_h[idx]; max_percap_dens1 <- sm_mean[idx]
              lag_time1 <- lag_time(x = time_h[1:idx], y = sm_mean[1:idx], deriv = percap_deriv[1:idx], blank = 0)
            }
          }
          
          tibble(phase1_idx = phase1_idx, lag_time1 = lag_time1,
                 max_percap1 = max_percap1, max_percap_time1 = max_percap_time1,
                 max_percap_dens1 = max_percap_dens1)
        },
        
        { # diauxie (unchanged)
          diauxie_idx <- NA_integer_; diauxie_time <- NA_real_; diauxie_dens <- NA_real_
          if (!is.na(.treatment) && (.treatment %in% two_phase_treatments) && !is.na(phase1_idx)) {
            min_candidates <- find_local_extrema(x = time_h, y = percap_deriv, return = "index",
                                                 return_maxima = FALSE, return_minima = TRUE, window_width_n = 15)
            min_candidates <- min_candidates[min_candidates > phase1_idx & time_h[min_candidates] < max_shift_time]
            if (length(min_candidates) > 0) {
              diauxie_idx <- min_candidates[1]
              diauxie_time <- time_h[diauxie_idx]
              diauxie_dens <- sm_mean[diauxie_idx]
            }
          }
          tibble(diauxie_idx = diauxie_idx, diauxie_time = diauxie_time, diauxie_dens = diauxie_dens)
        },
        
        { # phase2 (unchanged)
          max_percap2 <- NA_real_; max_percap_time2 <- NA_real_; max_percap_dens2 <- NA_real_; lag_time2 <- NA_real_
          if (!is.na(diauxie_idx) && !is.na(.treatment) && (.treatment %in% two_phase_treatments)) {
            max2_candidates <- find_local_extrema(x = time_h, y = percap_deriv, return = "index", return_maxima = TRUE, return_minima = FALSE, window_width_n = 15)
            max2_candidates <- max2_candidates[max2_candidates > diauxie_idx]
            if (length(max2_candidates) > 0) {
              max2_idx <- max2_candidates[1]
              max_percap2 <- percap_deriv[max2_idx]; max_percap_time2 <- time_h[max2_idx]; max_percap_dens2 <- sm_mean[max2_idx]
              lag_time2 <- lag_time(x = time_h[max2_idx:length(time_h)], y = sm_mean[max2_idx:length(time_h)], deriv = percap_deriv[max2_idx:length(time_h)], blank = 0)
            }
          }
          tibble(max_percap2 = max_percap2, max_percap_time2 = max_percap_time2, max_percap_dens2 = max_percap_dens2, lag_time2 = lag_time2)
        },
        
        max_dens = max(sm_mean, na.rm = TRUE),
        min_dens = min_gc(sm_mean),
        auc = auc(y = sm_mean, x = as.numeric(time_h)),
        
        .groups = "drop"
      ) %>%
      relocate(eff_min_time_for_phase1, eff_max_time_for_phase1, .before = phase1_idx)
  }
  
  
  
  # Apply
  data_sum_f199  <- summarize_plate_two_phase(f199_test)
  data_sum_both  <- summarize_plate_two_phase(both_test)
  data_sum_g7    <- summarize_plate_two_phase(g7_test)
  data_sum_g72   <- summarize_plate_two_phase(g72_test)
  data_sum_f1992 <- summarize_plate_two_phase(f1992_test)
  
  ## --- 4. Combine results ---
  gc_test  <- bind_rows(f199_test, both_test, g7_test, g72_test, f1992_test)
  gc_test2 <- bind_rows(data_sum_f199, data_sum_both, data_sum_g7, data_sum_g72, data_sum_f1992)
  
  ## --- 5. Summary table (means ± SE for both phases) ---
  sum_gc <- gc_test2 %>%
    group_by(strain, media, treatment) %>%
    summarise(
      growthrate1_mean = mean(max_percap1, na.rm = TRUE),
      growthrate1_se   = sd(max_percap1, na.rm = TRUE) / sqrt(n()),
      growthrate2_mean = mean(max_percap2, na.rm = TRUE),
      growthrate2_se   = sd(max_percap2, na.rm = TRUE) / sqrt(n()),
      dens_mean       = mean(max_dens, na.rm = TRUE),
      dens_se         = sd(max_dens, na.rm = TRUE) / sqrt(n()),
      auc_mean        = mean(auc, na.rm = TRUE),
      auc_se          = sd(auc, na.rm = TRUE) / sqrt(n()),
      lag1_mean       = mean(lag_time1, na.rm = TRUE),
      lag1_se         = sd(lag_time1, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(
      strain = recode(strain, n.aroma = "F199", p.putida = "G7"),
      treatment = recode(treatment,
                         cap = "Capsule",
                         chit = "Chitosan",
                         free = "Planktonic")
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
  gc_flex
  
  ## --- 6. Plots with both phases ---
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
      
      # Raw OD
      p0 <- ggplot(data_group, aes(x = time_h, y = sm_mean)) +
        geom_point() +
        labs(title = paste("Corrected OD:", group_name),
             y = "Corrected OD", x = "Time")
      
      # Log OD with tangents
      p1 <- ggplot(data_group, aes(x = time_h, y = log(sm_mean))) +
        geom_point() +
        geom_abline(data = summ,
                    aes(slope = max_percap1,
                        intercept = log(max_percap_dens1) - max_percap1 * max_percap_time1),
                    color = "red", inherit.aes = FALSE) +
        geom_abline(data = summ %>% filter(!is.na(max_percap2)),
                    aes(slope = max_percap2,
                        intercept = log(max_percap_dens2) - max_percap2 * max_percap_time2),
                    color = "orange", inherit.aes = FALSE) +
        geom_vline(data = summ %>% filter(!is.na(diauxie_time)),
                   aes(xintercept = diauxie_time),
                   linetype = "dashed", color = "blue", inherit.aes = FALSE) +
        labs(title = "Log(corrected OD)", y = "log(corrected OD)", x = "Time")
      
      # Per-capita derivative
      p2 <- ggplot(data_group, aes(x = time_h, y = percap_deriv)) +
        geom_line() +
        geom_point(data = summ,
                   aes(x = max_percap_time1, y = max_percap1),
                   color = "red", size = 2, inherit.aes = FALSE) +
        geom_point(data = summ %>% filter(!is.na(max_percap2)),
                   aes(x = max_percap_time2, y = max_percap2),
                   color = "orange", size = 2, inherit.aes = FALSE) +
        geom_vline(data = summ %>% filter(!is.na(diauxie_time)),
                   aes(xintercept = diauxie_time),
                   linetype = "dashed", color = "blue", inherit.aes = FALSE) +
        labs(title = "Per-capita derivative", y = "Derivative", x = "Time") +
        coord_cartesian(ylim = c(-1, NA))
      
      combined <- cowplot::plot_grid(p0, p1, p2, ncol = 1, align = "v")
      plots[[group_name]] <<- combined
    })
  
  # Export all plots into an HTML file
  outdir <- tempdir()
  htmltools::save_html(
    tagList(lapply(names(plots), function(nm) {
      f <- file.path(outdir, paste0(nm, ".png"))
      ggsave(f, plots[[nm]], width = 8, height = 8, dpi = 120)
      tags$div(
        style = "page-break-after: always;",
        tags$h2(nm),
        tags$img(src = f, style = "width:100%;")
      )
    })),
    file = "growthcurve_comparison_two_phase_smoothed3.html"
  )
  
  
  # ensure phase1_time_map exists in the environment and is a tibble like you showed earlier
  data_sum_both <- summarize_plate_two_phase(
    both_test,
    two_phase_treatments = c("cap", "chit"),
    min_time_for_phase1 = 0,
    max_time_for_phase1 = 15,
    phase1_time_map = phase1_time_map  # pass the tibble variable, no extra parentheses
  )
  data_sum_both %>%
    filter(is.na(phase1_idx)) %>%
    select(strain, media, treatment, well, plate.id, eff_min_time_for_phase1, eff_max_time_for_phase1) %>%
    arrange(strain, media, treatment, plate.id, well) %>%
    print(n = 200)
  
  