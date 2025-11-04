#trying gcplyr
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
  mutate(plate.id = "f199")

both <- read_excel(data_location3) %>%
  mutate(plate.id = "both")

g7 <- read_excel(data_location4) %>%
  mutate(plate.id = "g7")

g72 <- read_excel(data_location5) %>%
  mutate(plate.id = "g72")

f1992 <- read_excel(data_location6) %>%
  mutate(plate.id = "f1992")

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
save_as_docx(path = here::here("results", "gctable.docx"), 
  "Growth Curve Table" = gc_flex) 
                                                                                      
#labels for plotting
bacteria_names <- list(
  'a.venet'="A. venetianus",
  'h.taenio'="H. taeniospiralis",
  'm.fred'="M. fredericksbergense",
  'n.aroma'="N. aromaticivorans", 
  'n.penta' = 'N. pentaromativorans', 
  'p.putida' = 'P. putida'
)
bacteria_labeller<- function(variable,value){
  return(bacteria_names[value])}

#plots

p_gr<- ggplot(data = sum_gc, aes(x = treatment, y = mean_growthrate, fill = treatment, alpha = media)) +
  scale_alpha_manual(values = c(2, 0.5)) +
  geom_bar(stat='identity', position=position_dodge(1)) +
  geom_errorbar(aes(ymin=mean_growthrate-se_growthrate, ymax=mean_growthrate+se_growthrate, group = interaction(sum_gc$strain,sum_gc$media)), 
                position=position_dodge(1, preserve = "single"), width = .3, alpha =1)+
  theme_pubr() + xlab("Treatment") + ylab("Growth Rate (h<sup>-1</sup>)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~ strain, labeller = as_labeller(bac_label))+
  scale_fill_manual(values = c( "cap" = "aquamarine4",
                                                     "chit" = "darkgoldenrod", 
                                                     "free" = "sienna3"))+
  scale_x_discrete(labels=c("cap"="capsule", 
                            "chit" = "chitosan", 
                            "free" = "planktonic"))+
  theme(strip.text = element_text(face = "italic", size = 12), axis.title.y = element_markdown())
p_gr
p_gr.cld<- ggdraw(p_gr) + #with CLD
  annotate(geom = "text", x = .19, y = .4, label = "a")+
  annotate(geom = "text", x = .33, y = .38, label = "a")+
  annotate(geom = "text", x = .475, y = .835, label = "b")
p_gr.cld
ggsave(here("results", "GCgrowthrate.png"))
#plot points to see distribution
p_gr_point<- ggplot(data = gc_test2, aes(x = treatment, y = max_percap, fill = treatment, alpha = media)) +
  scale_alpha_manual(values = c(2, 0.5)) +
  geom_point(fill = "black", position = position_jitter(width = 0.1, height = 0.1))+
  #stat_summary(geom="col", fun = mean) +
  #stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Treatment") + ylab("Growth Rate (h<sup>-1</sup>)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~ strain+media, labeller = as_labeller(bac_label))+
  scale_fill_manual(values = c( "cap" = "aquamarine4",
                                "chit" = "darkgoldenrod", 
                                "free" = "sienna3"))+
  scale_x_discrete(labels=c("cap"="capsule", 
                            "chit" = "chitosan", 
                            "free" = "planktonic"))+
  theme(strip.text = element_text(face = "italic", size = 12), 
        axis.title.y = element_markdown()) #italicize
p_gr_point

p_max<- ggplot(data = sum_gc, aes(x = treatment, y = mean_dens, fill = treatment, alpha = media)) +
  scale_alpha_manual(values = c(2, 0.5)) +
  geom_bar(stat='identity', position=position_dodge(1)) +
  geom_errorbar(aes(ymin=mean_dens-se_dens, ymax=mean_dens+se_dens, group = interaction(sum_gc$strain,sum_gc$media)), 
                position=position_dodge(1, preserve = "single"), width = .3, alpha =1)+
  theme_pubr() + xlab("Treatment") + ylab("Carrying Capacity (OD600)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~strain, labeller = as_labeller(bac_label))+
  scale_fill_manual(values = c( "cap" = "aquamarine4",
                                "chit" = "darkgoldenrod", 
                                "free" = "sienna3"))+
  scale_x_discrete(labels=c("cap"="capsule", 
                            "chit" = "chitosan", 
                            "free" = "planktonic"))+
  theme(strip.text = element_text(face = "italic", size = 12)) #italicize
p_max
p_max.cld<- ggdraw(p_max) + #with CLD
  annotate(geom = "text", x = .13, y = .72, label = "a")+
  annotate(geom = "text", x = .2, y = .31, label = "b")+
  annotate(geom = "text", x = .27, y = .84, label = "b")+
  annotate(geom = "text", x = .34, y = .33, label = "c")+
  annotate(geom = "text", x = .41, y = .62, label = "c")+
  annotate(geom = "text", x = .48, y = .38, label = "d")+
  annotate(geom = "text", x = .59, y = .64, label = "a")+ #p.putida starts here
  annotate(geom = "text", x = .66, y = .27, label = "c")+
  annotate(geom = "text", x = .73, y = .66, label = "a")+
  annotate(geom = "text", x = .8, y = .37, label = "b")+
  annotate(geom = "text", x = .873, y = .68, label = "a")+
  annotate(geom = "text", x = .94, y = .28, label = "c")
p_max.cld
ggsave(here("results", "GCcarrycap.png"))
p_auc<- ggplot(data = sum_gc, aes(x = treatment, y = mean_auc, fill = treatment, alpha = media)) +
  scale_alpha_manual(values = c(2, 0.5)) +
  geom_bar(stat='identity', position=position_dodge(1)) +
  geom_errorbar(aes(ymin=mean_auc-se_auc, ymax=mean_auc+se_auc, group = interaction(sum_gc$strain,sum_gc$media)), 
                position=position_dodge(1, preserve = "single"), width = .3, alpha =1)+
  theme_pubr() + xlab("Treatment") + ylab("AUC (OD600*h)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~strain, labeller = as_labeller(bac_label))+
  scale_fill_manual(values = c( "cap" = "aquamarine4",
                                "chit" = "darkgoldenrod", 
                                "free" = "sienna3"))+
  scale_x_discrete(labels=c("cap"="capsule", 
                            "chit" = "chitosan", 
                            "free" = "planktonic"))+
  theme(strip.text = element_text(face = "italic", size = 12)) #italicize
p_auc
p_auc.cld<- ggdraw(p_auc) + #with CLD
  annotate(geom = "text", x = .115, y = .76, label = "ab")+ #lb cap
  annotate(geom = "text", x = .2, y = .31, label = "c")+ #srb cap
  annotate(geom = "text", x = .27, y = .84, label = "a")+ #lb chit
  annotate(geom = "text", x = .34, y = .34, label = "c")+ #srb chit
  annotate(geom = "text", x = .41, y = .72, label = "b")+ #lb free
  annotate(geom = "text", x = .48, y = .32, label = "c")+ #srb free
  annotate(geom = "text", x = .59, y = .61, label = "a")+ #p.putida starts here
  annotate(geom = "text", x = .66, y = .25, label = "c")+
  annotate(geom = "text", x = .73, y = .45, label = "ab")+
  annotate(geom = "text", x = .8, y = .25, label = "c")+
  annotate(geom = "text", x = .873, y = .62, label = "a")+
  annotate(geom = "text", x = .94, y = .28, label = "bc")
p_auc.cld
ggsave(here("results", "GCauc.png"))
#this might not be correct due to different lengths of time across experiments

p_lag<- ggplot(data = sum_gc, aes(x = treatment, y = mean_lag, fill = treatment, alpha = media)) +
  scale_alpha_manual(values = c(2, 0.5)) +
  geom_bar(stat='identity', position=position_dodge(1, preserve = "single")) +
  geom_errorbar(aes(ymin=mean_lag-se_lag, ymax=mean_lag+se_lag, group = interaction(sum_gc$strain,sum_gc$media)), 
                position=position_dodge(1, preserve = "single"), width = .3, alpha =1)+
  theme_pubr() + xlab("Treatment") + ylab("Lag Time (h)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~strain, labeller = as_labeller(bac_label))+
  scale_fill_manual(values = c( "cap" = "aquamarine4",
                                "chit" = "darkgoldenrod", 
                                "free" = "sienna3"))+
  scale_x_discrete(labels=c("cap"="capsule", 
                            "chit" = "chitosan", 
                            "free" = "planktonic"))+
  theme(strip.text = element_text(face = "italic", size = 12)) #italicize
p_lag
p_lag.cld<- ggdraw(p_lag) + #with CLD
  draw_line(x = c(.1, .42), y = c(.33, .33))+ #capsule yes v no
  annotate("text", x = 0.26, y = 0.35, label = "b", size = 4)+
  #annotate(geom = "text", x = .115, y = .76, label = "b")+
  #annotate(geom = "text", x = .2, y = .31, label = "e")+
  #annotate(geom = "text", x = .27, y = .84, label = "a")+
  #annotate(geom = "text", x = .34, y = .34, label = "de")+
  #annotate(geom = "text", x = .41, y = .72, label = "c")+
  annotate(geom = "text", x = .475, y = .83, label = "a")+
  annotate(geom = "text", x = .95, y = .82, label = "NS") #p.putida starts here
p_lag.cld
ggsave(here("results", "GClag.png"))
grid.arrange(p_gr.cld, p_max.cld, nrow = 1)
grid.arrange(p_auc.cld, p_lag.cld, nrow = 1)
p_gr.cld
p_max.cld
p_auc.cld
p_lag.cld

plot_grid(p_gr.cld, p_max.cld,p_auc.cld,p_lag.cld)

library(magick)
growth <- image_read(here("results", "GCgrowthrate.png"))
carrycap <- image_read(here("results", "GCcarrycap.png"))
auc <- image_read(here("results", "GCauc.png"))
lag <- image_read(here("results", "GClag.png"))
combo<- c(growth, carrycap, auc,)
image_append(combo)
image_montage(combo, geometry = "1200x", tile = '2x2') #this method works to combine pngs. need to adjust annotations, and then can use this

gc_test2_g7<- gc_test2 %>% filter(strain == "p.putida")
gc_test2_f199<- gc_test2 %>% filter(strain == "n.aroma")
#STATS
anova_gr<- aov(max_percap ~ strain*media*treatment, data = gc_test2) 
summary(anova_gr)
tuk_new<- anova_gr %>% tukey_hsd()
letters_gr<- HSD.test(anova_gr, c("strain", "media", "treatment"), group=TRUE, console = TRUE)

anova_dens_g7<- aov(max_dens ~ media*treatment, data = gc_test2_g7) 
summary(anova_dens_g7)
tuk_dens_g7<- anova_dens_g7 %>% tukey_hsd()
letters_dens_g7<- HSD.test(anova_dens_g7, c("media", "treatment"), group=TRUE, console = TRUE)

anova_dens_f199<- aov(max_dens ~ media*treatment, data = gc_test2_f199) 
summary(anova_dens_f199)
tuk_dens_f199<- anova_dens_f199 %>% tukey_hsd()
letters_dens_f199<- HSD.test(anova_dens_f199, c("media", "treatment"), group=TRUE, console = TRUE)

#AUC
anova_auc_g7<- aov(auc ~ media*treatment, data = gc_test2_g7) 
summary(anova_auc_g7)
tuk_auc_g7<- anova_auc_g7 %>% tukey_hsd()
letters_auc_g7<- HSD.test(anova_auc_g7, c("media", "treatment"), group=TRUE, console = TRUE)

anova_auc_f199<- aov(auc ~ media*treatment, data = gc_test2_f199) 
summary(anova_auc_f199)
tuk_auc_f199<- anova_auc_f199 %>% tukey_hsd()
letters_auc_f199<- HSD.test(anova_auc_f199, c("media", "treatment"), group=TRUE, console = TRUE)

#LAG TIME
anova_lag_g7<- aov(lag_time ~ media*treatment, data = gc_test2_g7) 
summary(anova_lag_g7)
tuk_lag_g7<- anova_lag_g7 %>% tukey_hsd()
letters_lag_g7<- HSD.test(anova_lag_g7, c("media", "treatment"), group=TRUE, console = TRUE)

anova_lag_f199<- aov(lag_time ~ media*treatment, data = gc_test2_f199) 
summary(anova_lag_f199)
tuk_lag_f199<- anova_lag_f199 %>% tukey_hsd()
letters_lag_f199<- HSD.test(anova_lag_f199, c("media", "treatment"), group=TRUE, console = TRUE)

## plot per cap deriv over time
ggplot(data = gc_test, aes(x = time_h, y = percap_deriv, color = treatment)) +
  geom_line(fill = "black", position = position_jitter(width = 0.1, height = 0.1))+
  #stat_summary(geom="col", fun = mean) +
  #stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Time") + ylab("Growth Rate (h<sup>-1</sup>)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~ strain+media+treatment)
ggplot(data = gc_test, aes(x = time_h, y = percap_deriv, color = treatment)) +
  geom_point(fill = "black", position = position_jitter(width = 0.1, height = 0.1)) +
  theme_pubr() + 
  xlab("Time") + 
  ylab("Growth Rate (h<sup>-1</sup>)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~ strain + media) +
  scale_color_manual(values = c("cap" = "aquamarine4",
                                "chit" = "darkgoldenrod", 
                                "free" = "sienna3")) +
  coord_cartesian(xlim = c(0, 100))   # <- this sets y-axis range

#look at them individually in f199_test, both_test, g7_test, g72_test, f1992_test
ggplot(data = f199_test, aes(x = time_h, y = percap_deriv, color = treatment)) +
  geom_line(fill = "black", position = position_jitter(width = 0.1, height = 0.1)) +
  theme_pubr() + 
  xlab("Time") + 
  ylab("Growth Rate (h<sup>-1</sup>)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~ strain + media) +
  scale_color_manual(values = c("cap" = "aquamarine4",
                                "chit" = "darkgoldenrod", 
                                "free" = "sienna3")) +
  coord_cartesian(xlim = c(0, 100))   # <- this sets y-axis range
ggplot(data = both_test, aes(x = time_h, y = percap_deriv, color = treatment)) +
  geom_line(fill = "black", position = position_jitter(width = 0.1, height = 0.1)) +
  theme_pubr() + 
  xlab("Time") + 
  ylab("Growth Rate (h<sup>-1</sup>)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~ strain + media) +
  scale_color_manual(values = c("cap" = "aquamarine4",
                                "chit" = "darkgoldenrod", 
                                "free" = "sienna3")) +
  coord_cartesian(xlim = c(0, 100))   # <- this sets y-axis range
ggplot(data = g7_test, aes(x = time_h, y = percap_deriv, color = treatment)) +
  geom_line(fill = "black", position = position_jitter(width = 0.1, height = 0.1)) +
  theme_pubr() + 
  xlab("Time") + 
  ylab("Growth Rate (h<sup>-1</sup>)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~ strain + media) +
  scale_color_manual(values = c("cap" = "aquamarine4",
                                "chit" = "darkgoldenrod", 
                                "free" = "sienna3")) +
  coord_cartesian(xlim = c(0, 100))   # <- this sets y-axis range
ggplot(data = g72_test, aes(x = time_h, y = percap_deriv, color = treatment)) +
  geom_line(fill = "black", position = position_jitter(width = 0.1, height = 0.1)) +
  theme_pubr() + 
  xlab("Time") + 
  ylab("Growth Rate (h<sup>-1</sup>)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~ strain + media) +
  scale_color_manual(values = c("cap" = "aquamarine4",
                                "chit" = "darkgoldenrod", 
                                "free" = "sienna3")) +
  coord_cartesian(xlim = c(0, 100))   # <- this sets y-axis range
ggplot(data = f1992_test, aes(x = time_h, y = percap_deriv, color = treatment)) +
  geom_line(fill = "black", position = position_jitter(width = 0.1, height = 0.1)) +
  theme_pubr() + 
  xlab("Time") + 
  ylab("Growth Rate (h<sup>-1</sup>)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~ strain + media) +
  scale_color_manual(values = c("cap" = "aquamarine4",
                                "chit" = "darkgoldenrod", 
                                "free" = "sienna3")) +
  coord_cartesian(xlim = c(0, 100))   # <- this sets y-axis range



########testing visualization fo calculations

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
    
    p <- ggplot(data_group, aes(x = time_h, y = percap_deriv)) +
      geom_line() +
      geom_point(
        data = summ,
        aes(x = max_percap_time, y = max_percap),
        color = "red", size = 2, inherit.aes = FALSE
      ) +
      geom_vline(
        data = summ,
        aes(xintercept = lag_time),
        color = "blue", linetype = "dashed", inherit.aes = FALSE
      ) +
      labs(title = paste("Growth curve:", group_name)) +
      coord_cartesian(ylim = c(-1, NA))
    
    plots[[group_name]] <<- p
  })

# save all plots into one HTML file
save_html(
  tagList(lapply(plots, plotly::ggplotly)), # optional: make plots interactive
  file = "growthcurve_plots.html"
)

##two plots
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
    
    ## --- Plot 1: log(corrected OD) over time with tangent ---
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
      labs(title = paste("OD (log-scale):", group_name),
           y = "log(corrected OD)", x = "Time")
    
    ## --- Plot 2: derivative over time ---
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
      labs(title = "Per-capita derivative", y = "Derivative", x = "Time") +
      coord_cartesian(ylim = c(-1, NA))
    
    ## Combine vertically
    combined <- p1 / p2
    plots[[group_name]] <<- combined
  })

#non-interactive
outdir <- tempdir()

htmltools::save_html(
  tagList(lapply(names(plots), function(nm) {
    # file path
    f <- file.path(outdir, paste0(nm, ".png"))
    # save combined patchwork to PNG
    ggsave(f, plots[[nm]], width = 8, height = 6, dpi = 150)
    # embed in HTML
    tags$div(
      style = "page-break-after: always;",
      tags$h2(nm),
      tags$img(src = f, style = "width:100%;")
    )
  })),
  file = "growthcurve_comparison.html"
)
##grouping

##back to original
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
  file = "growthcurve_comparison_3.html"
)


