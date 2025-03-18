library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(writexl)
library(ggpubr)
library(scales)
library(cowplot)
library(ggpattern)
library(ggeasy)
library(plotrix)
library(rstatix)
library(ggbreak)

data_location2 <- here::here("data", "Aim 2.1 Plate Counts copy.xlsx")
counts<- read_excel(data_location2)

counts$strain <- counts$strain %>% factor(levels = c("abiotic", 
                                                   "p.putida", 
                                                   "a.venet", 
                                                   "n.aroma", 
                                                   "n.penta", 
                                                   "m.fred"))

#label info
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

#clean dataset
clean<- counts %>% select(exp, strain, sample,rep, day, rx, sample_type, avg_cfu)
clean<- clean%>% na.omit()
clean<- clean %>% mutate(avg_cfu = ifelse(avg_cfu == 0, .1, avg_cfu)) #convert zeroes to very small values so that they show up on plot

#manually calculate mean and SE
clean_sum <- clean %>% select(-sample) %>% select(-exp) %>% group_by(day, rx, sample_type, strain) %>% summarize(mean_cfu = mean(avg_cfu), se = std.error(avg_cfu))

#attempting bar plot, formatting weird
ggplot(data = clean, aes(x = day, y = avg_cfu, fill = rx, pattern = sample_type)) +
  stat_summary(geom="col", fun = mean) +
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +facet_wrap(~strain)

#testing plot with points - looks OK but I think could be hard to interpret
ggplot(clean, aes(day, avg_cfu, color = interaction(rx, sample_type), shape = sample_type, group = interaction(rx, sample_type))) +
  stat_summary(geom = "point", width = .1, position = position_dodge(5), size = 4)+
  stat_summary(geom = "errorbar", width = .7, position = position_dodge(5), size = .5, color = "black")+
  facet_wrap(~strain, labeller = as_labeller(bac_label))+
  scale_y_continuous(transform = "log10", limits = c(NA, 1e10), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  theme_pubr()+
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  scale_x_continuous(breaks = c(0, 21, 42))+
  theme(strip.text = element_text(face = "italic", size = 12))+ #italicize
  scale_color_manual(
  name = "Treatment",
  values = c("encapsulated.cap" = "aquamarine2", "encapsulated.super" = "forestgreen", "free.super" = "cornflowerblue"), 
  guide = guide_legend(override.aes = list(pattern_alpha = 0)))

#trying to add lines to the above plot
ggplot(clean, aes(day, avg_cfu, color = interaction(rx, sample_type), shape = sample_type, group = interaction(rx, sample_type))) +
  stat_summary(geom = "point", position = position_dodge(5), size = 4)+
  stat_summary(geom = "errorbar", width = .7, position = position_dodge(5), size = .5, color = "black")+
  stat_summary(fun.y=mean, geom="line", size = .8, aes(group = interaction(clean$rx, clean$sample_type)), position = position_dodge(5))+
  facet_wrap(~strain, labeller = as_labeller(bac_label))+
  scale_y_continuous(transform = "log10", limits = c(NA, 1e10), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  theme_pubr()+
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  scale_x_continuous(breaks = c(0, 21, 42))+
  theme(strip.text = element_text(face = "italic", size = 12))+ #italicize
  scale_color_manual(
    name = "Treatment",
    values = c("encapsulated.cap" = "chartreuse1", "encapsulated.super" = "forestgreen", "free.super" = "cornflowerblue"), 
    guide = guide_legend(override.aes = list(pattern_alpha = 0)))+
  labs(shape = "Sample Type")+
  xlab("Day") + 
  ylab("CFU/mL")


#attempting with geom_point instead of stat_summary
ggplot(data = clean_sum, aes(x = day, y = mean_cfu, color = rx, shape = sample_type)) +
  geom_point(stat='identity', size = 3) +
  geom_errorbar(aes(ymin=mean_cfu-se, ymax=mean_cfu+se), width=.2,
                position=position_dodge(.9), size = .9)+
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+facet_wrap(~strain)+
  scale_y_continuous(transform = "log10", limits = c(NA, 1e10), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  theme_pubr()

#striped bar plots
class(clean$rx)
#clean<- clean %>% mutate(
  #rx = as.factor(rx, levels = c(unique(rx))),
  #sample_type = as.factor(sample_type, levels = c(unique(sample_type)))
#)
ggplot(clean, aes(as.factor(day), avg_cfu, pattern = sample_type)) +
  stat_summary(aes(fill = rx), 
               geom = "bar_pattern", 
               color= "black",
               pattern_fill="black", 
               pattern_density = .03, 
               width = 1,
               pattern_spacing = .06,
               pattern_key_scale_factor = .2,
               position = position_dodge(width=1, preserve = "single"))+
  stat_summary(geom = "errorbar", width = .15, position = position_dodge(width=1, preserve = "single"), size = .5,
               color = "forestgreen")+
  theme_pubr()+
  scale_y_continuous(transform = "log10", limits = c(-1e1, 1e10), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  facet_wrap(~strain, strip.position = "bottom", scales = "free_x")+
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside")+
ggtitle("Concentrations over Time")+
  xlab("Days") + 
  ylab("CFU/mL")+
    ggeasy::easy_labs()+
  scale_pattern_manual(
    name = "Sample Type",
    values = c(
      "cap" = "stripe",
      "super" = "none",
      "encapsulated" = "none", 
      "free" = "none"), 
    guide = guide_legend(override.aes = list(fill = "whitesmoke")))+
  scale_fill_manual(
    name = "Treatment",
    values = c("encapsulated" = "#FF27D5", "free" = "cyan"), 
    guide = guide_legend(override.aes = list(pattern_alpha = 0))) #remove pattern lines from fill legend
 
test<- clean %>% group_by(rx, sample_type, strain, day) %>% summarise(
  se = sd(avg_cfu)
)




#plotting one strain at a time 
clean %>% filter (strain == "n.aroma") %>% ggplot(aes(as.factor(day), avg_cfu, pattern = sample_type)) +
  stat_summary(aes(fill = rx), 
               geom = "bar_pattern", 
               color= "black",
               pattern_fill="black", 
               pattern_density = .03, 
               width = 1,
               pattern_spacing = .06,
               pattern_key_scale_factor = .2,
               position = position_dodge(width=1, preserve = "single"))+
  stat_summary(geom = "errorbar", width = .15, position = position_dodge(width=1, preserve = "total"), size = .5)+
  theme_pubr()+
  scale_y_continuous(transform = "log10", limits = c(-1e1, 1e10), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside")+
  ggtitle("Concentrations over Time")+
  xlab("Days") + 
  ylab("CFU/mL")+
  ggeasy::easy_labs()+
  scale_pattern_manual(
    name = "Sample Type",
    values = c(
      "cap" = "stripe",
      "super" = "none",
      "encapsulated" = "none", 
      "free" = "none"), 
    guide = guide_legend(override.aes = list(fill = "whitesmoke")))+
  scale_fill_manual(
    name = "Treatment",
    values = c("encapsulated" = "#FF27D5", "free" = "cyan"), 
    guide = guide_legend(override.aes = list(pattern_alpha = 0))) #remove pattern lines from fill legend

#scatterplot over time
ggplot(data = clean_sum, aes(x = day, y = mean_cfu, color = rx, shape = sample_type)) +
  geom_point(stat='identity', position=position_jitterdodge(8), dodge.width = 1, size = 3) +
  geom_errorbar(aes(ymin=mean_cfu-se, ymax=mean_cfu+se), width=1.1,
                position=position_dodge(5), size = .6, color = "black")+
  theme_classic() + 
  xlab("Day") + 
  ylab("CFU Value")+facet_wrap(~strain) +
  scale_y_continuous(transform = "log10", limits = c(-1e1, 1e10)) 
#geom_point isn't working well because values are fairly similar

clean1 <- clean

#converting concentrations to total values
clean <- clean %>% select(!"sample") %>% pivot_wider(names_from = sample_type, values_from = avg_cfu)

freetest<- clean %>% filter(rx == "free")
captest<- clean %>% filter(rx != "free")
freetest <- freetest %>% mutate(super_total = (super*10), 
                                total = super*10)
captest <- captest %>% mutate(super_total = (super*10), 
                              capsule_total = (cap*2), 
                              total = (super*10) + (cap*2))
alltest<- freetest %>% full_join(captest)
alltest$capsule_total[is.na(alltest$capsule_total)] <- 0
alltest<- alltest %>% mutate(total_conc = (super_total+capsule_total)/12) #calculate total concentrations
#pivot to long
long<- alltest %>% pivot_longer(cols = !c(exp, strain, rep, day, rx), names_to = "sample", values_to = "value") %>% na.omit()
#keep total (absolute) values
total<- long %>% filter(sample == "super_total"|sample == "capsule_total"|sample == "total_conc")

#manually calculate mean and SE
sum<- total %>% group_by(strain, day, rx, sample) %>% summarise(mean = mean(value), se = sd(value) / sqrt(n()))
  
total.all<- total %>% filter (sample == "total_conc")



#trying to plot total concentrations (first draft)
total_conc_only <- sum %>% filter (sample == "total_conc") # subset data frame to be only total concentrations
ggplot(data = total_conc_only, aes(x = day, y = mean, fill = rx)) +
  geom_bar(stat='identity', position=position_dodge(8), width = 8) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.7,
                position=position_dodge(8), size = .7)+
  xlab("Day") + 
  ylab("RFU Value")+facet_wrap(~strain)+
  scale_y_continuous(transform = "log10", limits = c(NA, 1e10), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  theme_pubr()+
  scale_x_continuous(breaks = c(0, 21, 42), expand=c(0,0))


total_conc_only$strain<- factor(total_conc_only$strain, 
                                levels = c("p.putida", "a.venet", "h.taenio", "n.aroma", "n.penta", "m.fred"))

#Figure with different colors for each bacteria species!!
ggplot(data = total_conc_only, aes(x = day, y = mean, fill = strain, alpha = factor(rx))) +
  scale_alpha_manual(values = c(0.5, 1)) +
  geom_bar(stat='identity', position=position_dodge(15), width = 15 ) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=4,
                position=position_dodge(15), size = 0.5, group = total_conc_only$rx)+
  xlab("Day") + 
  ylab("Total CFU/mL per reactor")+
  facet_wrap(~strain, labeller = as_labeller(bac_label))+
  scale_y_continuous(transform = "log10", limits = c(NA, 1e10), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  theme_pubr()+
  scale_x_continuous(breaks = c(0, 21, 42), expand=c(0,0))+
  guides(fill = "none")+
  labs(alpha = "Treatment")+
theme(strip.text = element_text(face = "italic", size = 12)) +
  coord_cartesian (ylim=c(1e5, 1e9)) #start at 1e5 to visualize differences more easily since we don't have near-zero values here

##FIXED transparency of error bars
p<- ggplot(data = total_conc_only, aes(x = as.factor(day), y = mean, fill = strain, alpha = factor(rx))) +
  scale_alpha_manual(values = c(0.5, 1)) +
  geom_bar(stat='identity', position=position_dodge(preserve = "single")) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, group = interaction(total_conc_only$strain, total_conc_only$rx)), 
                width=.3,
                position=position_dodge(.9, preserve = "single"), 
                size = 0.5, 
                alpha = 1)+
  xlab("Day") + 
  ylab("Total CFU/mL per reactor")+
  facet_wrap(~strain, scales = "free_y", labeller = as_labeller(bac_label), nrow=2)+
  scale_y_continuous(transform = "log10", limits = c(NA, 1e10), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  theme_pubr()+
  scale_x_discrete(breaks = c(0, 21, 42), expand=c(0,0))+
  guides(fill = "none")+
  labs(alpha = "Treatment")+
  theme(strip.text = element_text(face = "italic", size = 12)) #+
pg<- p + scale_y_break(c(1e1, 1e5), space = 0.4, scales = "free")
print(p)
print(pg)
#facet_wrap works with ggbreak, but only if 1 row is used
  coord_cartesian (ylim=c(1e5, 1e9))


  

# I think logtotal is not representing the data correctly. Need to look more into this. Like the look of the stacked
#bar graphs but challenging to fit scale and to get error bars correct

data_location2 <- here::here("data", "aim2.1platecounts_clean.xlsx")
write_xlsx(total, data_location2)
data_location3 <- here::here("data", "aim2.1platecounts_sum.xlsx")
write_xlsx(summary_data, data_location3)



#removing h.taenio due to Sanger results indicating contamination
total_conc_only <- total_conc_only %>% filter(strain != "h.taenio")

###switching to dot plots based on feedback about log scale/bars
p_total<- ggplot(data = total_conc_only, aes(x = as.factor(day), y = mean, color = strain, group = factor(rx))) +
  geom_line(aes(linetype = rx), position=position_dodge(.2))+
  geom_point(stat='identity', position=position_dodge(.2)) +
  #geom_bar(stat='identity', position=position_dodge(preserve = "single")) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, group = interaction(total_conc_only$strain, total_conc_only$rx)), 
                width=.2,
                position=position_dodge(.2, preserve = "single"), 
                size = 0.5, 
                alpha = 1)+
  xlab("Day") + 
  ylab("Total CFU/mL per reactor")+
  facet_wrap(~strain, scales = "free_x", labeller = as_labeller(bac_label), nrow=2)+
  scale_y_continuous(transform = "log10", limits = c(NA, 1e10), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  theme_pubr()+
  scale_x_discrete(breaks = c(0, 21, 42), expand=c(0,0))+
  guides(color = "none")+
  scale_linetype_manual(name = "Treatment", 
                        values = linetype,
                        labels=c("Capsule", "Planktonic"))+
  theme(strip.text = element_text(face = "italic", size = 12))+
  scale_color_manual(values = c( "p.putida" = "#ABA300",
                                 "a.venet" = "#00BE67", 
                                 "n.aroma" = "#00BFC4", 
                                 "n.penta" = "#00A9FF", 
                                 "m.fred" = "#ED68ED"))
p_total
p_total_brac<- ggdraw(p_total)+ #asterisks represent significant treatment term in RM anova separated by strain
  annotate("text", x = 0.23, y = 0.4, label = "*", size = 5) #n. penta
p_total_brac
  
#TOTAL STATS
# Perform two-factor ANOVA for each strain
anova_results.total <- total.all %>%
  group_by(strain) %>%
  do({
    model <- aov(value ~ rx * day + 
                   Error(rep/day), data = .)  # Repeated measures structure
    tidy(model)  # Produces a clean summary of ANOVA output
  })

# Display results
print(anova_results.total)

# Extract and summarize significant p-values for clarity
significant_results.total <- anova_results.total %>%
  filter(p.value < 0.05) %>%
  select(strain, term, p.value)

print(significant_results.total) #shows that only sig effect of Rx was for N.penta <.05 



###

##include all types, not just total
#removing h.taenio due to Sanger results indicating contamination
clean_sum <- clean_sum %>% filter(strain != "h.taenio")
#scatterplot over time
linetype2 = rep(c('dotted','solid', 'longdash'),3) #set linetypes for scale function
ggplot(data = clean_sum, aes(x = day, y = mean_cfu, color = strain, 
                             #shape = sample_type, 
                             group = interaction(clean_sum$sample_type, clean_sum$rx))) +
  geom_line(aes(linetype = interaction(clean_sum$sample_type, clean_sum$rx), group = interaction(clean_sum$sample_type, clean_sum$rx)), 
            position=position_dodge(width = 9),
            size = .7)+
  geom_point(stat='identity', position=position_dodge(width =9), size = 2) +
  geom_errorbar(aes(ymin=mean_cfu-se, ymax=mean_cfu+se, 
                    group = interaction(clean_sum$sample_type, clean_sum$rx)),
                width=4,
                position=position_dodge(width = 9), size = .7)+
  theme_pubr()+
  xlab("Day") + 
  ylab("CFU/mL")+
  facet_wrap(~strain, labeller = as_labeller(bac_label), ncol = 3, scales = "free_x") +
  scale_y_continuous(transform = "log10", limits = c(-1e1, 10^(9.5)), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  scale_x_continuous(limits = c(-5, 50), 
                     breaks = seq(0, 42, 21))+ #syntax is seq(start, end, step-by)
  coord_cartesian (ylim=c(10^(5.5), 10^(9.5)))+
  theme(strip.text = element_text(face = "italic", size = 12), axis.title.y = element_markdown())+
  scale_linetype_manual(name = "Treatment/Sample", 
                      values = linetype2,
                      labels=c("Capsule", "Supernatant", "Planktonic"))+
  guides(linetype = guide_legend(override.aes = list(size = 10))) +
  guides(color = "none")+
  scale_color_manual(values = c( "p.putida" = "#ABA300",
                                 "a.venet" = "#00BE67", 
                                 "n.aroma" = "#00BFC4", 
                                 "n.penta" = "#00A9FF", 
                                 "m.fred" = "#ED68ED"))

##COMPONENT STATS
clean.m <- clean %>%
  mutate(sample_type = ifelse(rx == "free", "planktonic", sample_type))
# Perform two-factor ANOVA for each strain
anova_results.comp <- clean.m  %>%
  group_by(strain) %>%
  do({
    model <- aov(avg_cfu ~ day * sample_type + 
                   Error(rep/day), data = .)  # Repeated measures structure
    tidy(model)  # Produces a clean summary of ANOVA output
  })

# Display results
print(anova_results.comp)

# Extract and summarize significant p-values for clarity
significant_results.comp <- anova_results.comp %>%
  filter(p.value < 0.05) %>%
  select(strain, term, p.value)

print(significant_results.comp) #no significant differences among component CFU concentrations
