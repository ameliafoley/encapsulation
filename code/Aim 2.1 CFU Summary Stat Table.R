##Aim 2.1 plate counts summary stats tables
##run the above code FIRST

library(broom)

anova_total <-
  bind_rows(
    tidy(stats_putida$fit) %>% mutate(strain = "p.putida"),
    tidy(stats_avenet$fit) %>% mutate(strain = "a.venet"),
    tidy(stats_naroma$fit) %>% mutate(strain = "n.aroma"),
    tidy(stats_npenta$fit) %>% mutate(strain = "n.penta")
  ) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  )

means_total <-
  summary_total %>%
  mutate(
    Mean_SE = sprintf("%.2e ± %.2e", mean, se)
  )

summary_component #descriptive statistics
anova_component <-
  bind_rows(
    tidy(stats_comp_putida$fit) %>% mutate(strain="p.putida"),
    tidy(stats_comp_avenet$fit) %>% mutate(strain="a.venet"),
    tidy(stats_comp_naroma$fit) %>% mutate(strain="n.aroma"),
    tidy(stats_comp_npenta$fit) %>% mutate(strain="n.penta")
  )
letters_component <-
  bind_rows(
    letters_comp_putida %>% mutate(strain="p.putida"),
    letters_comp_avenet %>% mutate(strain="a.venet"),
    letters_comp_naroma %>% mutate(strain="n.aroma"),
    letters_comp_npenta %>% mutate(strain="n.penta")
  )
##global anova
global_total <- total %>%
  mutate(
    log_cfu = log10(total_cfu)
  )

global_total_anova <- aov(
  log_cfu ~ strain * rx * day,
  data = global_total
)

###############################################################
# Workbook of statistics for manuscript
###############################################################

library(openxlsx)
library(broom)
library(dplyr)
library(emmeans)

wb <- createWorkbook()

###############################################################
# Sheet 1 - Total reactor means ± SE
###############################################################

means_total <-
  summary_total %>%
  mutate(
    Mean_SE = sprintf("%.2e ± %.2e", mean, se)
  ) %>%
  arrange(strain, rx, day)

addWorksheet(wb, "Total_Means_SE")
writeData(wb, "Total_Means_SE", means_total)

###############################################################
# Sheet 2 - Component means ± SE
###############################################################

means_component <-
  summary_component %>%
  mutate(
    Mean_SE = sprintf("%.2e ± %.2e", mean, se)
  ) %>%
  arrange(strain, sample, day)

addWorksheet(wb, "Component_Means_SE")
writeData(wb, "Component_Means_SE", means_component)

###############################################################
# Sheet 3 - Total reactor ANOVAs
###############################################################

anova_total <-
  bind_rows(
    tidy(stats_putida$fit) %>% mutate(strain = "p.putida"),
    tidy(stats_avenet$fit) %>% mutate(strain = "a.venet"),
    tidy(stats_naroma$fit) %>% mutate(strain = "n.aroma"),
    tidy(stats_npenta$fit) %>% mutate(strain = "n.penta")
  ) %>%
  filter(term %in% c("rx","day","rx:day")) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  ) %>%
  select(
    strain,
    term,
    df,
    statistic,
    p.value,
    sig
  )

addWorksheet(wb, "Total_ANOVA")
writeData(wb, "Total_ANOVA", anova_total)

###############################################################
# Sheet 4 - Component ANOVAs
###############################################################

anova_component <-
  bind_rows(
    tidy(stats_comp_putida$fit) %>% mutate(strain = "p.putida"),
    tidy(stats_comp_avenet$fit) %>% mutate(strain = "a.venet"),
    tidy(stats_comp_naroma$fit) %>% mutate(strain = "n.aroma"),
    tidy(stats_comp_npenta$fit) %>% mutate(strain = "n.penta")
  ) %>%
  filter(term %in% c("sample","day","sample:day")) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  ) %>%
  select(
    strain,
    term,
    df,
    statistic,
    p.value,
    sig
  )

addWorksheet(wb, "Component_ANOVA")
writeData(wb, "Component_ANOVA", anova_component)

###############################################################
# Sheet 5 - Total reactor Tukey comparisons
###############################################################

tukey_total <-
  bind_rows(
    
    emmeans(stats_putida$fit, ~ rx * day) %>%
      pairs(adjust = "tukey") %>%
      broom::tidy() %>%
      mutate(strain = "p.putida"),
    
    emmeans(stats_avenet$fit, ~ rx * day) %>%
      pairs(adjust = "tukey") %>%
      broom::tidy() %>%
      mutate(strain = "a.venet"),
    
    emmeans(stats_naroma$fit, ~ rx * day) %>%
      pairs(adjust = "tukey") %>%
      broom::tidy() %>%
      mutate(strain = "n.aroma"),
    
    emmeans(stats_npenta$fit, ~ rx * day) %>%
      pairs(adjust = "tukey") %>%
      broom::tidy() %>%
      mutate(strain = "n.penta")
    
  ) %>%
  mutate(
    sig = case_when(
      adj.p.value < 0.001 ~ "***",
      adj.p.value < 0.01  ~ "**",
      adj.p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  ) %>%
  select(
    strain,
    contrast,
    estimate,
    std.error,
    df,
    statistic,
    adj.p.value,
    sig
  )

addWorksheet(wb, "Total_Tukey")
writeData(wb, "Total_Tukey", tukey_total)

###############################################################
# Sheet 6 - Component Tukey comparisons
###############################################################

tukey_component <-
  bind_rows(
    
    emmeans(stats_comp_putida$fit, ~ sample * day) %>%
      pairs(adjust = "tukey") %>%
      broom::tidy() %>%
      mutate(strain = "p.putida"),
    
    emmeans(stats_comp_avenet$fit, ~ sample * day) %>%
      pairs(adjust = "tukey") %>%
      broom::tidy() %>%
      mutate(strain = "a.venet"),
    
    emmeans(stats_comp_naroma$fit, ~ sample * day) %>%
      pairs(adjust = "tukey") %>%
      broom::tidy() %>%
      mutate(strain = "n.aroma"),
    
    emmeans(stats_comp_npenta$fit, ~ sample * day) %>%
      pairs(adjust = "tukey") %>%
      broom::tidy() %>%
      mutate(strain = "n.penta")
    
  ) %>%
  mutate(
    sig = case_when(
      adj.p.value < 0.001 ~ "***",
      adj.p.value < 0.01  ~ "**",
      adj.p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  ) %>%
  select(
    strain,
    contrast,
    estimate,
    std.error,
    df,
    statistic,
    adj.p.value,
    sig
  )

addWorksheet(wb, "Component_Tukey")
writeData(wb, "Component_Tukey", tukey_component)

global_total_table <-
  broom::tidy(global_total_anova) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  ) %>%
  select(
    term,
    df,
    statistic,
    p.value,
    sig
  )

addWorksheet(wb, "Global_Total_ANOVA")
writeData(wb, "Global_Total_ANOVA", global_total_table)
global_component <-
  clean %>%
  mutate(
    sample = case_when(
      rx == "free" ~ "planktonic",
      rx == "encapsulated" & sample_type == "super" ~ "supernatant",
      rx == "encapsulated" & sample_type == "cap" ~ "capsule"
    ),
    log_cfu = log10(avg_cfu)
  )

global_component_anova <- aov(
  log_cfu ~ strain * sample * day,
  data = global_component
)

global_component_table <-
  broom::tidy(global_component_anova) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  ) %>%
  select(
    term,
    df,
    statistic,
    p.value,
    sig
  )

addWorksheet(wb, "Global_Component_ANOVA")
writeData(wb, "Global_Component_ANOVA", global_component_table)

###############################################################
# Save workbook
###############################################################

saveWorkbook(
  wb,
  "Aim2.1_CFU_statistics_summary.xlsx",
  overwrite = TRUE
)