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
library(ggtext)


#load data. 
final_join <- read_excel(here::here("data", "final_join.xlsx"))

#widen final data to calculate total concentration in reactors
test <- final_join %>% pivot_wider(names_from = sample_type.x, values_from = avg_cfu) %>% filter(strain != "blank")

freetest<- test %>% filter(encapsulation_treat == "planktonic")
captest<- test %>% filter(encapsulation_treat != "planktonic")
#freetest <- freetest %>% mutate(total = (supernatant)) %>% mutate(release = 1)
captest <- captest %>% mutate(total = ((supernatant*10)+(capsule*2))/12)
alltest<- freetest %>% full_join(captest)

#goal: figure presenting total, encapsulated, planktonic, and supernatant together
#create a variable that includes all four types as "components"
plank <- alltest %>% filter(encapsulation_treat == "planktonic") %>% mutate(planktonic = supernatant)
encap <- alltest %>% filter(encapsulation_treat == "encapsulated")
join_alltest <- plank %>% full_join(encap)
longalltest<- join_alltest %>% 
  pivot_longer(cols = supernatant:planktonic, 
               names_to = "component", 
               values_to = "concentration") #master component dataset
longalltest_1<- longalltest[!is.na(longalltest$concentration),] #remove NAs in order for this to work
longalltest_1 <- longalltest_1 %>% filter(!component == "supernatant" | !encapsulation_treat == "planktonic")
#break down into exp A
a_exp<- longalltest_1 %>% filter(exp == "a")
level_order<- c("capsule", "supernatant", "total", "planktonic") #order appearance of components for figures
a_exp$component <- factor(a_exp$component)
a_g7<- a_exp %>% filter(strain == "G7")
a_g7_anova <- aov(concentration ~ component, data = a_g7) 
a_anova <- aov(concentration ~ component*strain, data = a_exp)
summary(a_anova) #interaction effect directs us to subdivide
a_g7_tukey<- TukeyHSD(a_g7_anova, conf.level=.95)

#stat.test method works well for getting pairwise comparisons into table

stat.test.g7 <- aov(concentration~component, data = a_g7) %>% tukey_hsd() #view stats
#stat.test <- stat.test %>% mutate(y.position = 10^8)
##assign letters
anova_ag7<- aov(concentration~component, data = a_g7) #anova alone
letters_ag7<- HSD.test(anova_ag7, c("component"), group=TRUE, console = TRUE) #Tukey letters
brks<- c(10^6, 10^7, 10^8, 10^9)
p_ag7.cld<- ggplot(data = a_g7, aes(x = factor(component, level = level_order), y = concentration, color = component)) +
  stat_summary(geom="point", fun = mean, size =3) +
  stat_summary(geom = "errorbar", width = .15, position = position_dodge(0.8), size = .6)+
  theme_pubr() + 
  #theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  theme(plot.title = element_markdown())+
  #ggtitle("*P. putida* G7")+
  xlab("Group") + 
  ylab("CFU/mL")+
  scale_y_continuous(transform = "log10", limits = c(10^(6), 10^(9.3)),
                     breaks = brks,
                     #breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)), 
                     n.breaks = 4) + 
  #facet_wrap(~strain, strip.position = "bottom", scales = "free_x")+
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside", 
        legend.position = "none")+
  scale_color_manual(values = c( "capsule" = "olivedrab3",
                                 "supernatant" = "cyan3", 
                                 "total" = "aquamarine4", 
                                 "planktonic" = "sienna3"))+
  scale_x_discrete(labels=c("supernatant"="super"))+
  annotate(geom = "text", x = 1, y = 10^(8.78), label = "a")+
  annotate(geom = "text", x = 2, y = 10^(7.62), label = "b")+
  annotate(geom = "text", x = 3, y = 10^(8.14), label = "b")+
  annotate(geom = "text", x = 4, y = 10^(7.88), label = "b")
p_ag7.cld
p_ag7.brac<- ggplot(data = a_g7, aes(x = factor(component, level = level_order), y = concentration, color = component)) +
  stat_summary(geom="point", fun = mean, size =3) +
  stat_summary(geom = "errorbar", width = .15, position = position_dodge(0.8), size = .6)+
  theme_pubr() + 
  #theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  theme(plot.title = element_markdown())+
  ggtitle("*P. putida* G7")+
  xlab("Group") + 
  ylab("CFU/mL")+
  scale_y_continuous(transform = "log10", limits = c(10^(6), 10^(9.3)),
                     breaks = brks,
                     #breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)), 
                     n.breaks = 4) + 
  #facet_wrap(~strain, strip.position = "bottom", scales = "free_x")+
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside", 
        legend.position = "none")+
  scale_color_manual(values = c( "capsule" = "olivedrab3",
                                 "supernatant" = "cyan3", 
                                 "total" = "aquamarine4", 
                                 "planktonic" = "sienna3"))+
  scale_x_discrete(labels=c("supernatant"="super"))+ 
  geom_signif(comparisons = list(c("capsule", "planktonic"), 
                                 c("capsule", "supernatant")), 
              annotation = c("****", "***"), 
              y_position=c(9, 8.8, 8.8, 8.8), color = "black", 
              tip_length = 0)
p_ag7.brac

#Exp A, F199
a_f199<- a_exp %>% filter(strain == "F199") #subset dataframe
stat.test.f199 <- aov(concentration~component, data = a_f199) %>% tukey_hsd() #ANOVA and Tukey
#assign letters
anova_af199<- aov(concentration~component, data = a_f199) #anova alone
letters_af199<- HSD.test(anova_af199, c("component"), group=TRUE, console = TRUE) #Tukey letters

p_af199.cld<- ggplot(data = a_f199, aes(x = factor(component, level = level_order), y = concentration, color = component)) +
  stat_summary(geom="point", fun = mean, size =3) +
  stat_summary(geom = "errorbar", width = .15, position = position_dodge(0.8), size = .6)+
  theme_pubr() + 
  #theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  theme(plot.title = element_markdown())+
  #ggtitle("*N. aromaticivorans* F199")+
  xlab("Group") + 
  ylab("CFU/mL")+
  scale_y_continuous(transform = "log10", limits = c(10^(6), 10^(9.3)),
                     breaks = brks,
                     #breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)), 
                     n.breaks = 4) + 
  #facet_wrap(~strain, strip.position = "bottom", scales = "free_x")+
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside")+ 
        #legend.position = "none")+
  scale_color_manual(values = c( "capsule" = "olivedrab3",
                                 "supernatant" = "cyan3", 
                                 "total" = "aquamarine4", 
                                 "planktonic" = "sienna3"))+
  scale_x_discrete(labels=c("supernatant"="super"))+
annotate(geom = "text", x = 1, y = 10^(8.97), label = "a")+
  annotate(geom = "text", x = 2, y = 10^(6.83), label = "c")+
  annotate(geom = "text", x = 3, y = 10^(8.2), label = "b")+
  annotate(geom = "text", x = 4, y = 10^(6.6), label = "c")
p_af199.cld

p_af199.brac<- ggplot(data = a_f199, aes(x = factor(component, level = level_order), y = concentration, color = component)) +
  stat_summary(geom="point", fun = mean, size =3) +
  stat_summary(geom = "errorbar", width = .15, position = position_dodge(0.8), size = .6)+
  theme_pubr() + 
  #theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  theme(plot.title = element_markdown())+
  ggtitle("*N. aromaticivorans* F199")+
  xlab("Group") + 
  ylab("CFU/mL")+
  scale_y_continuous(transform = "log10", limits = c(10^(6), 10^(9.3)),
                     breaks = brks,
                     #breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)), 
                     n.breaks = 4) + 
  #facet_wrap(~strain, strip.position = "bottom", scales = "free_x")+
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside", 
        legend.position = "none")+
  scale_color_manual(values = c( "capsule" = "olivedrab3",
                                 "supernatant" = "cyan3", 
                                 "total" = "aquamarine4", 
                                 "planktonic" = "sienna3"))+
  scale_x_discrete(labels=c("supernatant"="super"))+
  geom_signif(comparisons = list(c("capsule", "planktonic"), 
                                 c("total", "planktonic"), 
                                 c("capsule", "supernatant"), 
                                 c("supernatant", "total")), 
              annotation = c("****", "****", "****", "****"), 
              y_position=c(9.1, 8.5, 8.95, 8.7), color = "black", 
              tip_length = 0) 
p_af199.brac
#combine g7 and f199 figs
p_strain.cld<- grid.arrange(p_ag7.cld, p_af199.cld, nrow = 1)
p_strain.cld
p_af199.cld

#strain_legend
legend <- get_plot_component(p_af199.cld, "guide-box-top", return_all = TRUE)
ggdraw(legend)
p_strain.cld.<- plot_grid(p_ag7.cld , 
                          p_af199.cld + theme(legend.position = "none"),
                          nrow = 1, labels = c('A', 'B'), label_size = 12)
p_strain.cld.
strainfinal<- ggdraw(p_strain.cld.) + annotate(geom = "text", 
                                               x = .15, 
                                               y = .97,
                                               size = 5,
                                               hjust = .5, 
                                               label = "G7")+
  annotate(geom = "text", 
             x = .66, 
             y = .97,
             size = 5,
             hjust = .5, 
             label = "F199")
strainfinal
strainfinal_leg<- plot_grid(legend, strainfinal, ncol = 1, rel_heights = c(.1, 1))
strainfinal_leg
ggsave(here("results", "strain_fig.png"), strainfinal_leg, width = 6.5, height = 5, units = "in", bg = "white")
grid.arrange(p_ag7.brac, p_af199.brac, nrow = 1)

#exp b - alginate
exp_b<- longalltest_1 %>% filter(exp == "b") 
exp_b<- exp_b %>% dplyr::select(-coating)
exp_b$alginate <- exp_b$alginate %>% replace_na("planktonic") 
exp_b$alginate <- exp_b$alginate %>% factor(levels = c("one", "two", "planktonic"))
#get stats
stat.test.alg <- aov(concentration~component*alginate, data = exp_b) %>% tukey_hsd() %>% na.omit()
anova_b<- aov(concentration~component*alginate, data = exp_b)
letters_b<- HSD.test(anova_b, c("component", "alginate"), group=TRUE, console = TRUE) #this assigns Tukey letters
#can assign letters using this method, and then manually annotate the figures
b_label<- c("one" = "1% alginate", 
            "two" = "2% alginate", 
            "planktonic" = "planktonic")
p_b<- ggplot(data = exp_b, aes(x = factor(component, level = level_order), y = concentration, color = component)) +
  stat_summary(geom="point", fun = mean, size =3) +
  stat_summary(geom = "errorbar", width = .15, position = position_dodge(0.8), size = .6)+
  theme_pubr() + 
  #theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  #ggtitle("Alginate %")+
  xlab("Treatment") + 
  ylab("CFU/mL")+
  scale_y_continuous(transform = "log10", limits = c(10e6,10^(8.5)),
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x))) + 
  facet_grid(~alginate, scales = "free_x", space = "free", switch = "x", labeller = as_labeller(b_label))+
  #theme(panel.spacing = unit(0, "lines"), 
        #strip.background = element_blank(),
        #strip.placement = "outside", 
        #legend.position = "none")+
  scale_color_manual(values = c( "capsule" = "olivedrab3",
                                 "supernatant" = "cyan3", 
                                 "total" = "aquamarine4", 
                                 "planktonic" = "sienna3"))+
  scale_x_discrete(labels=c("supernatant"="super"))

p_b
p_b.cld<- ggdraw(p_b)+ #with CLD
  annotate(geom = "text", x = .165, y = .65, label = "b")+
  annotate(geom = "text", x = .278, y = .39, label = "d")+
  annotate(geom = "text", x = .395, y = .47, label = "cd")+
  annotate(geom = "text", x = .542, y = .78, label = "a")+
  annotate(geom = "text", x = .655, y = .37, label = "d")+
  annotate(geom = "text", x = .77, y = .54, label = "c")+
  annotate(geom = "text", x = .92, y = .65, label = "b")
p_b.cld
p_b.brac<- ggdraw(p_b)+ #with pairwise brackets
  draw_line(x = c(.17, .54), y = c(.79, .79))+ #capsule 1 vs 2
  annotate("text", x = 0.35, y = 0.8, label = "****", size = 5)+
  draw_line(x = c(.39, .91), y = c(.83, .83))+ #one total vs plank
  annotate("text", x = 0.65, y = 0.84, label = "****", size = 5)+
  draw_line(x = c(.77, .91), y = c(.79, .79))+ # two total v plank
  annotate("text", x = 0.84, y = 0.8, label = "****", size = 5)
p_b.brac
  
#experiment c - chitosan coating
exp_c<- longalltest_1 %>% filter(exp == "c") 
exp_c$coating <- exp_c$coating %>% replace_na("planktonic") 
exp_c$coating <- exp_c$coating %>% factor(levels = c("no", "yes", "planktonic"))
#get stats and assign letters
stat.test.chit <- aov(concentration~component*coating, data = exp_c) %>% tukey_hsd() %>% na.omit()
anova_c<- aov(concentration~component*coating, data = exp_c)
summary(anova_c)
letters_c<- HSD.test(anova_c, c("component", "coating"), group=TRUE, console = TRUE)
c_label<- c("yes" = "chitosan coating", 
            "no" = "no coating", 
            "planktonic" = "planktonic")
p_c <- ggplot(data = exp_c, aes(x = factor(component, level = level_order), y = concentration, color = component)) +
  stat_summary(geom="point", fun = mean, size =3) +
  stat_summary(geom = "errorbar", width = .15, position = position_dodge(0.8), size = .6)+
  theme_pubr() + 
  #theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  #ggtitle("Chitosan Coating")+
  xlab("Treatment") + 
  ylab("CFU/mL")+
  scale_y_continuous(transform = "log10", limits = c(10e6,NA),
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x))) + 
  facet_grid(~coating, switch = "x", scales = "free_x", space = "free", labeller = as_labeller(c_label))+
  #theme(panel.spacing = unit(0, "lines"), 
       # strip.background = element_blank(),
       # strip.placement = "outside", 
       # legend.position = "none")+
  scale_color_manual(values = c( "capsule" = "olivedrab3",
                                 "supernatant" = "cyan3", 
                                 "total" = "aquamarine4", 
                                 "planktonic" = "sienna3"))+
  scale_x_discrete(labels=c("supernatant"="super"))
p_c
p_c.cld<- ggdraw(p_c)+ #with CLD
  annotate(geom = "text", x = .165, y = .84, label = "a")+ #could make annotations gray?
  annotate(geom = "text", x = .28, y = .365, label = "c")+
  annotate(geom = "text", x = .395, y = .57, label = "bc")+
  annotate(geom = "text", x = .543, y = .46, label = "bc")+
  annotate(geom = "text", x = .655, y = .485, label = "bc")+
  annotate(geom = "text", x = .77, y = .48, label = "bc")+
  annotate(geom = "text", x = .92, y = .64, label = "b")
p_c.cld
#strange that some of these are NS - exporting df to double check in JMP
write_xlsx(exp_c, here::here("data","exp_c.xlsx"))
#JMP gives same result. think this is the result of log scale and sample size (p values are .1, .3, and .5, so there are differences, they just don't meet our threshold). 

p_c_extraspace <- ggplot(data = exp_c, aes(x = factor(component, level = level_order), y = concentration, color = component)) +
  stat_summary(geom="point", fun = mean, size =3) +
  stat_summary(geom = "errorbar", width = .15, position = position_dodge(0.8), size = .6)+
  theme_pubr() + 
  #theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Chitosan Coating")+
  xlab("Treatment") + 
  ylab("CFU/mL")+
  scale_y_continuous(transform = "log10", limits = c(10e6,10^(8.6)),
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x))) + 
  facet_grid(~coating, switch = "x", scales = "free_x", space = "free")+
  #theme(panel.spacing = unit(0, "lines"), 
  # strip.background = element_blank(),
  # strip.placement = "outside", 
  # legend.position = "none")+
  scale_color_manual(values = c( "capsule" = "olivedrab3",
                                 "supernatant" = "cyan3", 
                                 "total" = "aquamarine4", 
                                 "planktonic" = "sienna3"))+
  scale_x_discrete(labels=c("supernatant"="super"))
p_c.brac<- ggdraw(p_c_extraspace)+ #with pairwise brackets
  draw_line(x = c(.17, .54), y = c(.82, .82))+ #capsule yes v no
  annotate("text", x = 0.35, y = 0.83, label = "****", size = 5)
  #draw_line(x = c(.39, .91), y = c(.85, .85))+ #no total vs plank
  #annotate("text", x = 0.65, y = 0.86, label = "***", size = 5)+
  #draw_line(x = c(.77, .91), y = c(.82, .82))+ # yes total v plank
  #annotate("text", x = 0.84, y = 0.83, label = "****", size = 5)+
  #draw_line(x = c(.39, .77), y = c(.79, .79))+ # total yes v total no
  #annotate("text", x = 0.58, y = 0.8, label = "***", size = 5)
p_c.brac
#experiment d - cell loading
exp_d<- longalltest_1 %>% filter(exp == "d") 
#get stats and assign letters
stat.test.cell <- aov(concentration~component*cell_loading, data = exp_d) %>% tukey_hsd() 
anova_d<- aov(concentration~component*cell_loading, data = exp_d)
letters_d<- HSD.test(anova_d, c("component", "cell_loading"), group=TRUE, console = TRUE)
p_d<- ggplot(data = exp_d, aes(x = factor(component, level = level_order), y = concentration, 
                               color = component, 
                               alpha = factor(cell_loading), 
                               shape = cell_loading)) +
  scale_alpha_manual(values = c(2, 0.5)) +
  stat_summary(geom="point", fun = mean, size =3, position = position_dodge(0.8)) +
  stat_summary(geom = "errorbar", width = .15, position = position_dodge(0.8), size = .6)+
  theme_pubr() + 
  #theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  #ggtitle("Cell Loading Concentration")+
  xlab("Group") + 
  ylab("CFU/mL")+
  scale_y_continuous(transform = "log10", limits = c(NA,NA),
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x))) + 
  #facet_wrap(~cell_loading, strip.position = "bottom", scales = "free_x")+
  #theme(panel.spacing = unit(0, "lines"), 
        #strip.background = element_blank(),
        #strip.placement = "outside", 
        #legend.position = "none")+
  scale_color_manual(values = c( "capsule" = "olivedrab3",
                                 "supernatant" = "cyan3", 
                                 "total" = "aquamarine4", 
                                 "planktonic" = "sienna3"))+
  scale_x_discrete(labels=c("supernatant"="super"))+
  labs(shape = "Density", 
       color = "Component")+
  guides(alpha = "none")
p_d #this essentially serves as the "bracket" image, since there are so many NS, unless I want to use this figure to highlight overall trends
p_d.cld<- ggdraw(p_d) + #with CLD
  annotate(geom = "text", x = .175, y = .88, label = "a")+
  annotate(geom = "text", x = .265, y = .76, label = "ab")+
  annotate(geom = "text", x = .392, y = .245, label = "d")+
  annotate(geom = "text", x = .475, y = .295, label = "d")+
  annotate(geom = "text", x = .605, y = .45, label = "cd")+
  annotate(geom = "text", x = .69, y = .43, label = "cd")+
  annotate(geom = "text", x = .82, y = .63, label = "bc")+
  annotate(geom = "text", x = .905, y = .52, label = "cd")
p_d.cld

#COMBINING FIGURES
#ggsave(here("results", "strain_fig.png"), p_strain, width = 6.5, height = 5, units = "in")
grid.arrange(p_ag7.brac, p_af199.brac, nrow = 1)
grid.arrange(p_b.cld, p_c.cld, p_d.cld, nrow = 2) #this doesn't look great, and also messes with my annotations
p_algchit.cld<- grid.arrange(p_b.cld, p_c.cld, nrow = 2)
p_algchit.cld
p_algchit.cld.<- plot_grid(p_b.cld, p_c.cld, nrow = 2, labels = c('A', 'B'), label_size = 12)
p_algchit.cld.
ggsave(here("results", "algchit_fig.png"), p_algchit.cld., width = 7, height = 8, units = "in")

p_b.cld
p_c.cld
p_d.cld

##SUMMARY TABLE
#table summarizing values
cleanall<- join_alltest %>% dplyr::select(exp, supernatant, capsule, total, planktonic, strain, encapsulation_treat, cell_loading, alginate, coating)
a_clean <- cleanall %>% filter(exp == "a") %>% dplyr::select(supernatant, capsule, total, planktonic, strain)
b_clean <- cleanall %>% filter(exp == "b") %>% dplyr::select(supernatant, capsule, total, planktonic, alginate)
b_clean$alginate <- b_clean$alginate %>% replace_na("planktonic") 
b_clean <- b_clean %>% dplyr::select(-planktonic)
b_clean$alginate <- factor(b_clean$alginate, levels = c("one", "two", "planktonic"))
c_clean <- cleanall %>% filter(exp == "c") %>% dplyr::select(supernatant, capsule, total, planktonic, coating)
c_clean$coating <- c_clean$coating %>% replace_na("planktonic") 
c_clean <- c_clean %>% dplyr::select(-planktonic)
c_clean$coating <- factor(c_clean$coating, levels = c("yes", "no", "planktonic"))
d_clean <- cleanall %>% filter(exp == "d") %>% dplyr::select(supernatant, capsule, total, planktonic, cell_loading)

#summary statistic tables by experiment
tbl_a <- a_clean %>% sumtable(group = "strain", numformat = formatfunc(scientific = TRUE, digits = 3))
tbl_b <- b_clean %>% sumtable(group = "alginate", numformat = formatfunc(scientific = TRUE, digits = 3))
tbl_c<- c_clean %>% sumtable(group = "coating", numformat = formatfunc(scientific = TRUE, digits = 3))
tbl_d<- d_clean %>% sumtable(group = "cell_loading", numformat = formatfunc(scientific = TRUE, digits = 3))

#overall cap vs. free summary statistic table
tbl_all<- cleanall %>% select(supernatant, capsule, total, encapsulation_treat) %>% sumtable(group = "encapsulation_treat", numformat = formatfunc(scientific = TRUE, digits = 3))


## CODE ARCHIVE
#many failed attempts to add p-values to graph automatically -> looks like manual will be best, or compact letter method
#stat_pvalue_manual(stat.test, label = "p.adj.signif", size = 3.7, y.position = 6e8, step.increase = 0.05, hide.ns = TRUE)+
#scale_y_continuous( limits = c(10^6, 10^9),
#breaks = trans_breaks('log10', function(x) 10^x), 
#labels = trans_format('log10', math_format(10^.x)))
#geom_text(label = dt$cld, vjust = -0.5)

#geom_pwc(aes(group=component), method = "tukey_hsd", label = "p.format")

#add_pvalue(stat.test, step.increase = .02)
#attempted to do automated letters for exp B, but it did not work
stat.test.b <- aov(concentration~component*alginate, data = exp_b) #ANOVA
b_tukey<- TukeyHSD(stat.test.b, conf.level=.95) #Tukey
btest<- tukey_hsd(stat.test.b)
cld_b <- multcompLetters4(stat.test.b, b_tukey) #generate compact letters
cld_af199 <- as.data.frame.list(cld_af199$component) #save as dataframe
cld_af199<- cld_af199 %>% rownames_to_column("component") #convert names to column
a_f199_cld<- a_f199 %>% left_join(cld_af199, by = join_by(component)) #join letters to original dataframe

+
  geom_signif(comparisons = list(c("capsule", "planktonic"), 
                                 c("total", "planktonic"), 
                                 c("capsule", "supernatant"), 
                                 c("supernatant", "total")), 
              annotation = c("****", "****", "****", "****"), 
              y_position=c(8, 8.9, 8.9, 8), color = "black") 
strip.position = "bottom"

# compact letter display for figures
cld <- multcompLetters4(a_g7_anova, a_g7_tukey)
glimpse(a_exp)
# table with factors and 3rd quantile
longalltest_1<- longalltest[!is.na(longalltest$concentration),] #remove NAs in order for this to work
longalltest_1 <- longalltest_1 %>% filter(!component == "supernatant" | !encapsulation_treat == "planktonic")
a_exp1<- longalltest_1 %>% filter(exp == "a") %>% filter(strain == "G7") #isolate exp A, G7 again
dt <- a_exp1 %>% dplyr::group_by(component) %>%
  dplyr::summarise(conc = mean(concentration), sd = sd(concentration)) %>% arrange(desc(conc)) #create the summary table
#problem is, I'm using stat_summary instead of the summary table to plot. could try plotting this way, or find another way to aling the labels

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$component) #careful not to do this twice, it will erase the df
dt$cld <- cld$Letters
#got CLD to work, but can't figure out how to annotate the figure with the letters yet. 

cld<- cld %>% rownames_to_column("component")
a_g7_cld<- a_g7 %>% left_join(cld, by = join_by(component))
#g_g7_cld.<- a_g7 %>% semi_join(cld, by = join_by(component))

cld_af199 <- as.data.frame.list(cld_af199$component) #save as dataframe
cld_af199<- cld_af199 %>% rownames_to_column("component") #convert names to column
a_f199_cld<- a_f199 %>% left_join(cld_af199, by = join_by(component)) #join letters to original dataframe