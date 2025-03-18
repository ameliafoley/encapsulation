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
library(patchwork)

data_location2 <- here::here("data", "cell viability_09052024.xlsx")
cell<- read_excel(data_location2)

cell<- cell %>% dplyr::select(1:3, 12)

free<- cell %>% filter(sample == "pbs" | sample == "sc")
im<- cell %>% filter(sample == "im-sc")
#bars
ggplot(data = free, aes(x = time, y = avg_cfu, fill = sample)) +
  stat_summary(geom="col", fun = mean, position = position_dodge(11, preserve = "single"), width = 10) +
  stat_summary(geom = "errorbar", width = .7, position = position_dodge(11, preserve = "single"), size = .5)+
  theme_pubr() + xlab("Exposure Time (min)") + ylab("CFU/mL") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))+
  scale_y_continuous(transform = "log10", limits = c(NA, 1e10), 
                   breaks = trans_breaks('log10', function(x) 10^x), 
                   labels = trans_format('log10', math_format(10^.x)))+
  scale_x_continuous(breaks = c(0, 15, 30))

#points
p_plank<- ggplot(data = free, aes(x = time, y = avg_cfu, color = sample)) +
  stat_summary(geom="point", fun = mean) +
  stat_summary(aes(group = sample), geom="line", fun = mean) +
  stat_summary(geom = "errorbar", width = .7)+
  theme_pubr() + xlab("Exposure Time (min)") + ylab("CFU/mL") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))+
  scale_y_continuous(transform = "log10", limits = c(NA, NA), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  scale_x_continuous(breaks = c(0, 15, 30))+
  labs(color = "Treatment")+
  scale_color_manual(values = c( "pbs" = "skyblue2",
                                  "sc" = "black"), 
                     labels=c("pbs"="1x PBS", 
                              "sc" = "0.1M Sodium Citrate"))+
  annotate("text", x = 15, y = 10^(7.54), label = "p <.001", size = 4)+
  guides(color = guide_legend(keywidth = .5))

p_plank
#bars
ggplot(data = im, aes(x = time, y = avg_cfu)) +
  stat_summary(geom="col", fun = mean, position = position_dodge(20, preserve = "single"), width = 4) +
  stat_summary(geom = "errorbar", width = .7, position = position_dodge(14, preserve = "single"), size = .5)+
  theme_pubr() + xlab("Exposure Time (min)") + ylab("CFU/mL") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))+
  scale_y_continuous(transform = "log10", limits = c(NA, 1e8), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  scale_x_continuous(breaks = c(0, 5, 15, 30))

#points
p_im<- ggplot(data = im, aes(x = time, y = avg_cfu)) +
  stat_summary(geom="point", fun = mean) +
  stat_summary(geom="line", fun = mean) +
  stat_summary(geom = "errorbar", width = .7)+
  theme_pubr() + xlab("Exposure Time (min)") + ylab("CFU/mL") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))+
  scale_y_continuous(transform = "log10", limits = c(NA, NA), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  scale_x_continuous(breaks = c(0, 5, 15, 30))+
  annotate("text", x = 29, y = 10^(5), label = "p <.01", size = 4)
p_im

p_citrate<- p_plank + p_im + plot_layout(ncol = 2)
p_citrate
p_citrate.<- plot_grid(p_plank, p_im, nrow = 1, labels = c('A', 'B'), label_size = 12, align = "h")
p_citrate.
ggsave(here("results", "citrate.png"), p_citrate., width = 7, height = 4, units = "in")



#STATS
free$time<- as.factor(free$time)
anova_free<- aov(avg_cfu ~ sample*time, data = free) 
summary(anova_free)
tuk_free<- anova_free %>% tukey_hsd()
letters_free<- HSD.test(anova_free, c("sample", "time"), group=TRUE, console = TRUE)
#significant effect of treatment *** p< .001

sc<- free %>% filter(sample == "sc")
anova_free.sc<- aov(avg_cfu ~ time + Error(replicate), data = sc) 
summary(anova_free.sc)
tuk_free.sc<- anova_free.sc %>% tukey_hsd()
letters_free.sc<- HSD.test(anova_free.sc, c("time"), group=TRUE, console = TRUE)
#IM
im$time<- as.factor(im$time)
anova_im<- aov(avg_cfu ~ time, data = im) 
summary(anova_im)
tuk_im<- anova_im %>% tukey_hsd()
letters_im<- HSD.test(anova_im, c("time"), group=TRUE, console = TRUE)
#significant effect of time ** p< .01
