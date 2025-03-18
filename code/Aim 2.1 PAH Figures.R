library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(ggplot2)
library("writexl", )
library(vtable)
library(gtsummary)
library(gridGraphics)
library(ggsignif)
library(multcompView)
library(rstatix)
library(ggpubr)
library(ggbreak)
library(scales)
library(cowplot)
library(ggprism)
library(gridExtra)
library(agricolae) #for assigning Tukey letters
library(ggsignif)
library(broom)


#load data. 
fla <- read_excel(here::here("data", "Exp 2.1 fluor and code copy.xlsx"))
glimpse(fla)

#clean and remove h.taenio (contaminated with A. venet)
clean <- fla %>% dplyr::filter(sample.no != "6") %>% filter(strain != "h.taenio")
clean$strain <- clean$strain %>% factor(levels = c("abiotic", 
                                                   "p.putida", 
                                                   "a.venet", 
                                                   "n.aroma", 
                                                   "n.penta", 
                                                   "m.fred"))

sum_fla<- clean %>% group_by(strain, day, treatment) %>% summarise(fluoranthene_mean = mean(concentration.ngml), 
                                                                       fluoranthene_se = sd(concentration.ngml) / sqrt(n()), 

                                                                                                                                          )

bacteria_names <- list(
  'a.venet'="A. venetianus",
  'h.taenio'="H. taeniospiralis",
  'm.fred'="M. fredericksbergense",
  'n.aroma'="N. aromaticivorans", 
  'n.penta' = 'N. pentaromativorans', 
  'p.putida' = 'P. putida', 
  'abiotic' = "Abiotic"
)
bacteria_labeller<- function(variable,value){
  return(bacteria_names[value])}

bac_label = c('a.venet'="A. venetianus",
              'h.taenio'="H. taeniospiralis",
              'm.fred'="M. fredericksbergense",
              'n.aroma'="N. aromaticivorans", 
              'n.penta' = 'N. pentaromativorans', 
              'p.putida' = 'P. putida', 
              'abiotic' = "Abiotic")

#plot concentration over time
ggplot(data = sum_fla, aes(x = day, y = fluoranthene_mean, fill = strain, alpha = treatment)) +
  geom_bar(stat='identity', position=position_dodge(20)) +
  geom_errorbar(aes(ymin=fluoranthene_mean-fluoranthene_se, ymax=fluoranthene_mean+fluoranthene_se, 
                    group = interaction(sum_fla$strain,sum_fla$treatment)), 
                position=position_dodge(20, preserve = "single"), width = 3, alpha = 1)+
  theme_pubr() + xlab("Treatment") + ylab("Fluoranthene (ng/mL)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~ strain, labeller = as_labeller(bac_label))+
  theme(strip.text = element_text(face = "italic", size = 12), axis.title.y = element_markdown())+
  scale_alpha_manual(name = "Treatment", 
                     values = c(1, 0.4), 
                     labels=c("Capsule", "Planktonic"))+
  guides(fill = "none")

#concentration over time as line plot with stats annotations
linetype = rep(c('solid', 'dashed'),2) #set linetypes for scale function

p_fla.line<- ggplot(data = sum_fla, aes(x = day, y = fluoranthene_mean, color = strain, group = treatment)) +
  geom_line(aes(linetype = treatment))+
  geom_point(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=fluoranthene_mean-fluoranthene_se, ymax=fluoranthene_mean+fluoranthene_se, 
                    group = interaction(sum_fla$strain,sum_fla$treatment)), 
                position=position_dodge(.1, preserve = "single"), width = 3, alpha = 1)+
  theme_pubr() + xlab("Treatment") + ylab("Fluoranthene (ng/mL)") + 
  #theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~ strain, labeller = as_labeller(bac_label), scales = 'free_x')+
  theme(strip.text = element_text(face = "italic", size = 12), axis.title.y = element_markdown())+
  scale_linetype_manual(name = "Treatment", 
                        values = linetype,
                     labels=c("Capsule", "Planktonic"))+
  guides(color = "none")+
  scale_x_continuous(breaks = seq(0, 42, 21)) #syntax is seq(start, end, step-by)
p_fla.line
p_fla.line.brac<- ggdraw(p_fla.line)+ #asterisks represent significant treatment term in RM anova separated by strain
  annotate("text", x = 0.23, y = 0.83, label = "****", size = 5)+ #abiotic
  annotate("text", x = 0.84, y = 0.83, label = "*", size = 5)+ #a.venet
  annotate("text", x = 0.84, y = 0.4, label = "*", size = 5) #m.fred
p_fla.line.brac


#percent removal
removal <- clean %>%
  group_by(strain, sample, treatment) %>%
  summarise(
    initial = concentration.ngml[day == 0], 
    mid = concentration.ngml[day == 21], 
    final = concentration.ngml[day == 42], 
    percent_removal_21 = (initial - mid) / initial * 100, 
    percent_removal_42 = (initial - final) / initial * 100
  )
removal_long <- removal %>% pivot_longer(cols = !c(strain, sample, treatment, initial, mid, final), 
                                             names_to = c(".value", "day"), 
                                             names_pattern = "(.*)_(\\d+)")
sum_removal <- removal %>%
  group_by(strain, treatment) %>%
  summarise(
    mean_removal_21 = mean(percent_removal_21), 
    se_removal_21 = sd(percent_removal_21) / sqrt(n()), 
    mean_removal_42 = mean(percent_removal_42), 
    se_removal_42 = sd(percent_removal_42) / sqrt(n())
  )
sum_removal_long <- sum_removal %>% pivot_longer(cols = !c(strain, treatment), 
                                 names_to = c(".value", "day"), 
                            names_pattern = "(.*)_(\\d+)")
#plot % removal
ggplot(data = sum_removal_long, aes(x = day, y = mean_removal, fill = strain, alpha = treatment)) +
  geom_bar(stat='identity', position=position_dodge(1)) +
  geom_errorbar(aes(ymin=mean_removal-se_removal, ymax=mean_removal+se_removal, 
                    group = interaction(sum_removal_long$strain,sum_removal_long$treatment)),  
                position=position_dodge(1, preserve = "single"), width = .3, alpha = 1)+
  theme_pubr() + xlab("Day") + ylab("Fluoranthene Removal (%)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~ strain, labeller = as_labeller(bac_label))+
  theme(strip.text = element_text(face = "italic", size = 12), axis.title.y = element_markdown())+
  scale_alpha_manual(name = "Treatment", 
                     values = c(1, 0.4), 
                     labels=c("Capsule", "Planktonic"))+
  guides(fill = "none")
  
#STATS

fla_anova<- aov(concentration.ngml ~ strain*treatment*day, data = clean) 
summary(fla_anova)
tuk_fla<- fla_anova %>% tukey_hsd()
letters_fla<- HSD.test(fla_anova, c("strain", "treatment", "day"), group=TRUE, console = TRUE)

# Perform two-factor ANOVA for each strain
anova_results <- clean %>%
  group_by(strain) %>%
  do({
    model <- aov(concentration.ngml ~ treatment * day + 
                   Error(sample/day), data = .)  # Repeated measures structure
    tidy(model)  # Produces a clean summary of ANOVA output
  })

# Display results
print(anova_results)

# Extract and summarize significant p-values for clarity
significant_results <- anova_results %>%
  filter(p.value < 0.05) %>%
  select(strain, term, p.value)

print(significant_results)

##REMOVAL %%%
fla.r_anova<- aov(percent_removal ~ strain*treatment*day, data = removal_long) 
summary(fla.r_anova)
tuk_fla<- fla.r_anova %>% tukey_hsd()
letters_fla<- HSD.test(fla.r_anova, c("strain", "treatment", "day"), group=TRUE, console = TRUE)

unique_strains <- unique(removal_long$strain)

# Create an empty list to store Tukey results for each strain
tukey_results <- list()

# Perform Tukey's HSD pairwise comparisons within each strain
for (s in unique_strains) {
  subset_data <- subset(removal_long, strain == s)
  aov_strain <- aov(percent_removal ~ treatment * day, data = subset_data)
  tukey_results[[s]] <- TukeyHSD(aov_strain)
}

# View Tukey HSD results for each strain
tukey_results

##new copy paste
# Perform Tukey HSD test (pairwise comparisons for the interaction between treatment and day)
tukey_results <- list()

# Loop through unique strains and perform Tukey HSD for the interaction term
for (s in unique(removal_long$strain)) {
  subset_data <- subset(removal_long, strain == s)
  aov_strain <- aov(percent_removal ~ treatment * day, data = subset_data)  # Focus on treatment and day interaction
  tukey_results[[s]] <- TukeyHSD(aov_strain, "treatment:day")  # Specify interaction term here
}

# Create a dataframe of significant pairwise comparisons for the interaction term
significant_pairs <- data.frame()

# Extract comparisons for the interaction term
for (s in names(tukey_results)) {
  tukey <- tukey_results[[s]]
  
  # Extract comparisons for the interaction term (treatment:day)
  interaction_comparisons <- data.frame(
    comparison = rownames(tukey$`treatment:day`),
    p_value = tukey$`treatment:day`[, "p adj"],
    factor = "treatment:day",
    strain = s
  )
  
  # Combine comparisons
  significant_pairs <- rbind(significant_pairs, interaction_comparisons)
}

# Filter for significant pairs (p-value < 0.05)
significant_pairs <- significant_pairs[significant_pairs$p_value < 0.05, ]

# View the significant pairwise comparisons for the interaction term
print(significant_pairs)

#add significance to plot  
p_f.removal<- ggplot(data = sum_removal_long, aes(x = day, y = mean_removal, fill = strain, alpha = treatment)) +
  geom_bar(stat='identity', position=position_dodge(1)) +
  geom_errorbar(aes(ymin=mean_removal-se_removal, ymax=mean_removal+se_removal, 
                    group = interaction(sum_removal_long$strain,sum_removal_long$treatment)),  
                position=position_dodge(1, preserve = "single"), width = .3, alpha = 1)+
  theme_pubr() + xlab("Day") + ylab("Fluoranthene Removal (%)") + 
  #theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~ strain, labeller = as_labeller(bac_label))+
  theme(strip.text = element_text(face = "italic", size = 12), axis.title.y = element_markdown())+
  scale_alpha_manual(name = "Treatment", 
                     values = c(1, 0.4), 
                     labels=c("Capsule", "Planktonic"))+
  guides(fill = "none")+
  ylim(NA, 121)
p_f.removal
 
p_f.removal.brac<- ggdraw(p_f.removal)+ #with pairwise brackets
  draw_line(x = c(.14, .2), y = c(.79, .79))+ #abiotic 21
  annotate("text", x = 0.17, y = 0.8, label = "****", size = 5)+
  draw_line(x = c(.27, .33), y = c(.79, .79))+ #abiotic 42
  annotate("text", x = 0.3, y = 0.8, label = "****", size = 5)+
  draw_line(x = c(.87, .93), y = c(.79, .79))+ # a.venet 42
  annotate("text", x = 0.9, y = 0.8, label = "*", size = 5)

p_f.removal.brac



