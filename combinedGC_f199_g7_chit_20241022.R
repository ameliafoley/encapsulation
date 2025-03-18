library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(writexl)
library(RColorBrewer)
library(ggbump)
library(plotrix)
library(ggtext)
library(ggpubr)
library(cowplot)

data_location2 <- here::here("data", "f199chit_20241011.xlsx")
data_location3 <- here::here("data", "gc_minusa_norm_20240930.xlsx")
data_location4 <- here::here("data", "g7chit_20241011.xlsx")
data_location5 <- here::here("data", "g7chit_clean_202410126.xlsx")
data_location6 <- here::here("data", "f199chit_clean_202410131.xlsx")


#load data
f199<- read_excel(data_location2)
both<- read_excel(data_location3)
g7<- read_excel(data_location4)
g72<- read_excel(data_location5)
f1992<- read_excel(data_location6)

all<- rbind(f199, both, g7, g72, f1992)

unique(all$time_h)
test<- all %>% group_by(well, media, strain, treatment) %>% mutate(rank = rank(time_h, ties.method = "min"))
test$time_h<- format(all$time_h, digits = 1) %>% as.numeric()
data_location2 <- here::here("data", "gc_combo20241028.xlsx")
write_xlsx(test, data_location2)
unique(test$time_h) #ID time points with slightly different decimals
edit1<- test %>% dplyr::filter(time_h == 	
                         53.7)
edit1$time_h <- 53.6
edit2<- test %>% dplyr::filter(time_h == 	
                                60.9)
edit2$time_h <- 60.8
edit3<- test %>% dplyr::filter(time_h == 	
                                76.6)
edit3$time_h <- 76.5
edit4<- test %>% dplyr::filter(time_h == 	
                                99.5)
edit4$time_h <- 99.4
rest<- test %>% filter(time_h != 53.7) %>% filter(time_h != 60.9) %>% filter(time_h != 76.6) %>% filter(time_h != 99.5)
fixed<- rbind(rest, edit1, edit2, edit3, edit4)
unique(fixed$time_h) #now there are no repeat values within 1 decimal of each other
test<- test %>% filter(rank<175)
glimpse(test)
glimpse(all)


#sum<- test %>% dplyr::select(-well) %>% group_by(strain, media, treatment, rank) %>% summarize(avgOD = mean(corrected), se = std.error(corrected))

#ggplot(data = sum, aes(x = rank, y = avgOD, color = treatment)) + 
  #geom_point()+
  #geom_errorbar(aes(ymin=avgOD-se, ymax=avgOD+se), width=.2,
                #position=position_dodge(.9), size = .9)+facet_wrap(~strain + media)+
  #geom_smooth(se = TRUE) + facet_wrap(~strain + media)
ggplot(data = test, aes(x = time_h, y = corrected, color = strain)) +
  geom_point()+
  #stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("F199 and G7 Growth Curves")+
  facet_wrap(~media + treatment, ncol = 2)

ggplot(data = test, aes(x = time_h, y = corrected, color = treatment)) +
  geom_line() + 
  #stat_summary(geom="point", fun = mean) +
  #stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("F199 and G7 Growth Curves")+
  facet_wrap(~strain + media, ncol = 2)

ggplot(data = test, aes(x = time_h, y = corrected, color = treatment)) +
  stat_summary(geom="point", fun = mean) +
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("F199 and G7 Growth Curves")+
  facet_wrap(~strain + media, ncol = 2)

f199<- fixed %>% filter(strain == "n.aroma") %>% filter(time_h < 113)
g7<- fixed %>% filter(strain == "p.putida") %>% filter(time_h < 71)

#split figures since time on x axis differs
p_g7<- ggplot(data = g7, aes(x = time_h, y = corrected, color = treatment)) +
  stat_summary(geom="point", fun = mean) +
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"), 
        plot.title = element_markdown(), 
        legend.text = element_text(size = 13), 
        legent.title = element_text(size = 14)) +
  ggtitle("*Pseudomonas putida* G7 Growth Curves (n=10)") +facet_wrap(~media)+
  scale_color_manual(values = c( "cap" = "aquamarine4",
                                 "chit" = "darkgoldenrod3",
                                 "free" = "sienna3"), 
                     labels=c("cap"="capsule", 
                              "chit" = "chitosan-coated capsule", 
                              "free" = "planktonic"))+
  labs(color = "Treatment")
p_g7
ggdraw(p_g7) + 
  draw_line(x = c(.78, .78), y = c(.2, .3), size = .5)

p_f199<- ggplot(data = f199, aes(x = time_h, y = corrected, color = treatment)) +
  stat_summary(geom="point", fun = mean) +
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"), 
        plot.title = element_markdown(), 
        legend.text = element_text(size = 13), 
        legent.title = element_text(size = 14)) +
  ggtitle("*Novosphingobium aromaticivorans* F199 Growth Curves (n = 10)") + facet_wrap(~media)+
  scale_color_manual(values = c( "cap" = "aquamarine4",
                                 "chit" = "darkgoldenrod3",
                                 "free" = "sienna3"), 
                     labels=c("cap"="capsule", 
                              "chit" = "chitosan-coated capsule", 
                              "free" = "planktonic"))+
  labs(color = "Treatment")
p_f199

#save data for analysis in JMP
fix_combo<- rbind(g7, f199)
data_location2 <- here::here("data", "gc_combo_20241031.xlsx")
write_xlsx(fix_combo, data_location2)

#ARRANGE FIGURES (.)
p_g7.<- ggplot(data = g7, aes(x = time_h, y = corrected, color = treatment)) +
  stat_summary(geom="point", fun = mean) +
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Time (hr)") + ylab("OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"), 
        plot.title = element_markdown(), 
        legend.text = element_text(size = 13), 
        legent.title = element_text(size = 14)) +
  ggtitle("G7") +
  facet_wrap(~media)+
  scale_color_manual(values = c( "cap" = "aquamarine4",
                                 "chit" = "darkgoldenrod3",
                                 "free" = "sienna3"), 
                     labels=c("cap"="capsule", 
                              "chit" = "chitosan-coated capsule", 
                              "free" = "planktonic"))+
  labs(color = "Treatment")
p_g7.
p_f199.<- ggplot(data = f199, aes(x = time_h, y = corrected, color = treatment)) +
  stat_summary(geom="point", fun = mean) +
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Time (hr)") + ylab("OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"), 
        plot.title = element_markdown(), 
        legend.text = element_text(size = 13), 
        legent.title = element_text(size = 14)) +
  ggtitle("F199") + 
  facet_wrap(~media)+
  scale_color_manual(values = c( "cap" = "aquamarine4",
                                 "chit" = "darkgoldenrod3",
                                 "free" = "sienna3"), 
                     labels=c("cap"="capsule", 
                              "chit" = "chitosan-coated capsule", 
                              "free" = "planktonic"))+
  labs(color = "Treatment")
p_f199.

bothgc<- grid.arrange(p_g7., p_f199.)
bothgc.<- plot_grid(p_g7., p_f199., nrow = 2, labels = c('A', 'B'), label_size = 12)
bothgc.
ggsave(here("results", "growthcurves.png"), bothgc., width = 7, height = 7, units = "in")


#combo of both strains (not as easy to interpret)
ggplot(data = fixed, aes(x = time_h, y = corrected, color = strain)) +
  stat_summary(geom="point", fun = mean) +
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("F199 and G7 Growth Curves")+
  facet_wrap(~treatment + media, ncol = 3)

free.only <-fixed %>% filter(treatment == "free")
ggplot(data = free.only, aes(x = time_h, y = OD600, color = strain)) +
  stat_summary(geom="point", fun = mean) +
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("F199 and G7 Growth Curves")+
  facet_wrap(~media, ncol = 2)

#JMP analysis
#Global ANOVA
#G7 sRB15 cap/chit p = 0.0137, cap /free p = 0.0277 (encompasses whole progression of GC)
#have to split after hr 37 because crossover masks differences - then, free/chit p = 0.0218

#G7 LB capchit NS, cap/free p = 0.0084, chit/free p = 0.0002
#split to examine final ending point (hr 55, point of intersection) - at that point, there is an effect of time in the ANOVA, but no effect of treatment
#or intersection of treatment/time

#F199 srb15 - cap/chit p = 0.0105, cap/free NS, chit/free p = 0.0119
#crossover masks difference between free and cap, split at hr 76 - cap.free p = .0014, others become NS

#F199 LB - cap/chit p = 0.0472, cap/free NS, chit/free p = 0.0128
#split at intersection point hr 35 - ANOVA all sig, then Tukey cap/chit p = 0.0116, cap/free NS, chit/free p = .0007

