library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(ggplot2)
library("writexl", )
library(ggpubr)
library(scales)
library(cowplot)
library(ggpattern)
library(ggeasy)

#import final plate counts
final_plate <- read_excel(here::here("data", "finalplatecounts.xlsx"))
final_plate_clean<- final_plate %>% select(exp, day, reactor, sample_type, avg_cfu)

allexp_code<- read_excel(here::here("data", "allexp_code.xlsx"))
final_cfu<- final_plate_clean %>% left_join(allexp_code, by = c("reactor", "exp"))

wide_cfu <- final_cfu %>% 
  pivot_wider(names_from = sample_type, values_from = avg_cfu) %>% rename(
  sup_cfu = supernatant, 
  cap_cfu = capsule)

all_wide <- read_excel(here::here("data", "all_wide.xlsx"))

join<- wide_cfu %>% left_join(all_wide)
write_xlsx(join, here::here("data","wide_join.xlsx"))
#now I have a df where I can compare RFU values for different sample types to CFU values for different sample types! 

#visualizing capsule RFU vs CFU
join %>% filter(meas == "mVenus") %>% ggplot(aes(x = capsule, y = cap_cfu, color = strain)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("Capsule RFU") + 
  ylab("Capsule CFU/mL")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Capsule RFU vs CFU")+
  geom_smooth(method = 'lm')


#supernatant rfu vs cfu
#attempt to get equation and r-squared
df<- join %>% filter(meas == "mVenus") %>% filter(strain != "blank")
lm_eqn <- function(df){
  m <- lm(sup_cfu ~ supernatant, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
#visualize
join %>% filter(meas == "mVenus") %>% filter(strain != "blank") %>% 
  ggplot(aes(x = supernatant, y = sup_cfu)) +
  geom_point(aes(color=encapsulation_treat, stat='identity')) + #designate color here to keep geom_smooth one line instead of 2
  theme_classic() + 
  xlab("Supernatant RFU") + 
  ylab("Supernatant CFU/mL")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Supernatant RFU vs CFU")+geom_smooth(method = 'lm')+
  geom_text(x = 12000, y = 9e7, label = lm_eqn(df), parse = TRUE)

#calculate line of best fit for mScarlet
df_scar<- join %>% filter(meas == "mScarlet_g7") %>% filter(strain != "blank")
lm_eqn_scar <- function(df_scar){
  m <- lm(sup_cfu ~ supernatant, df_scar);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
join %>% filter(meas == "mScarlet_g7") %>% filter(strain != "blank") %>% 
  ggplot(aes(x = supernatant, y = sup_cfu)) +
  geom_point(aes(color=encapsulation_treat, stat='identity')) + #designate color here to keep geom_smooth one line instead of 2
  theme_classic() + 
  xlab("Supernatant RFU") + 
  ylab("Supernatant CFU/mL")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Supernatant RFU vs CFU")+geom_smooth(method = 'lm')+
  geom_text(x = 5000, y = 9e7, label = lm_eqn_scar(df_scar), parse = TRUE)

#calculate line of best fit for mScarlet_f199
df_scarf<- join %>% filter(meas == "mScarlet_f199") %>% filter(strain != "blank")
lm_eqn_scarf <- function(df_scarf){
  m <- lm(sup_cfu ~ supernatant, df_scarf);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
join %>% filter(meas == "mScarlet_f199") %>% filter(strain != "blank") %>% 
  ggplot(aes(x = supernatant, y = sup_cfu)) +
  geom_point(aes(color=encapsulation_treat, stat='identity')) + #designate color here to keep geom_smooth one line instead of 2
  theme_classic() + 
  xlab("Supernatant RFU") + 
  ylab("Supernatant CFU/mL")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Supernatant RFU vs CFU")+geom_smooth(method = 'lm')+
  geom_text(x = 5000, y = 6e7, label = lm_eqn_scarf(df_scarf), parse = TRUE)

#calculate line of best fit for GFP
df_gfp<- join %>% filter(meas == "GFP") %>% filter(strain != "blank")
lm_eqn_gfp <- function(df_gfp){
  m <- lm(sup_cfu ~ supernatant, df_gfp);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
join %>% filter(meas == "GFP") %>% filter(strain != "blank") %>% 
  ggplot(aes(x = supernatant, y = sup_cfu)) +
  geom_point(aes(color=encapsulation_treat, stat='identity')) + #designate color here to keep geom_smooth one line instead of 2
  theme_classic() + 
  xlab("Supernatant RFU") + 
  ylab("Supernatant CFU/mL")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Supernatant RFU vs CFU")+geom_smooth(method = 'lm')+
  geom_text(x = 5000, y = 6e7, label = lm_eqn_gfp(df_gfp), parse = TRUE)

#this seems to suggest that within the same sampling day, there is a relationship between CFU and RFU
#however, that relationship is shifting over time. If I can normalize the data, perhaps I can compare across time points? 

glimpse(join)
#dissolved capsule
join %>% filter(meas == "mVenus") %>% filter(strain != "blank") %>% 
  ggplot(aes(x = dissolved, y = cap_cfu)) +
  geom_point(aes(color=encapsulation_treat, stat='identity')) + #designate color here to keep geom_smooth one line instead of 2
  theme_classic() + 
  xlab("Supernatant RFU") + 
  ylab("Supernatant CFU/mL")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Dissolved Cap. RFU vs Cap. CFU")+geom_smooth(method = 'lm')

clean_cfu <- final_cfu %>% filter(strain != "blank")
write_xlsx(clean_cfu, here::here("data","clean_cfu.xlsx"))

#testing out ggpubr
my_comparisons <- list(c("encapsulated"))
p<- ggbarplot(clean_cfu, x= "encapsulation_treat", y = "avg_cfu", 
          add = c("mean_se", "jitter"), 
          color = "sample_type", 
          position = position_dodge(0.8)) +
  stat_compare_means(aes(group = encapsulation_treat), method = "anova")+
  yscale("log10", .format = TRUE)
p #view plot

##  FIGURES FOR ANALYSIS

#endpoint CFU encapsulated vs. free
attr(clean_cfu$sample_type, "label") <- "Sample Type"
p1<- ggplot(clean_cfu, aes(encapsulation_treat, avg_cfu, fill = encapsulation_treat, pattern = sample_type)) +
  stat_summary(aes(fill=encapsulation_treat), 
               geom = "bar_pattern", 
               pattern_fill="black", 
               pattern_density = .03, 
               width = .8,
               pattern_spacing = .06,
               pattern_key_scale_factor = .2,
               position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4) +
  theme_pubr()+
  scale_y_continuous(transform = "log10", limits = c(NA, 9e8), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  #geom_bracket(data = clean_cfu, aes(xmin = .8, xmax = 2, y.position = 8,
    #label = "****", size = 10, step.increase = .1, label.size = .5))+ #capsule vs free super
  #geom_bracket(data = clean_cfu, aes(xmin = 1.3, xmax = 2, y.position = 8,
                                     #label = "**", size = 10, step.increase = .2, label.size = .5))+ #super vs super
  #geom_bracket(data = clean_cfu, aes(xmin = .8, xmax = 1.1, y.position = 8,
                                     #label = "****", size = 10, step.increase = .2, label.size = .5))+ #capsule vs cap super
  ggtitle("Viability After 56 Days")+
  xlab("Treatment") + 
  ylab("CFU/mL")+
  scale_pattern_manual(values=c("stripe", "none"))+
  guides(pattern = guide_legend(override.aes = list(fill = "whitesmoke")), 
         fill = "none")+
  scale_fill_manual(values=c("#7CAE00", "cyan3")) + ggeasy::easy_labs()
p1
#using geom bracket, can customize pairwise comparison within groups that I couldn't with Prism or
ggdraw(p1) + 
  annotate("text", x = 0.48, y = 0.78, label = "****", size = 5) + #capsule vs free super
  draw_line(
    x= c(.28, .75), 
    y=c(.77, 0.77), 
    color = "black", size = .5) +
  annotate("text", x = 0.34, y = 0.81, label = "****", size = 5) + #capsule vs cap super
  draw_line(
    x= c(.28, .4), 
    y=c(.8, 0.8), 
    color = "black", size = .5) +
annotate("text", x = 0.6, y = 0.81, label = "**", size = 5) + #super vs cap super
  draw_line(
    x= c(.45, .77), 
    y=c(.8, 0.8), 
    color = "black", size = .5) 
ggsave(here("results", "all_viability.png"), width = 5000, height = 4000, units = "px", dpi = 800)

#exp B = alginate impact on bacterial survival
clean_b<- clean_cfu %>% filter(exp == "b") 
attr(clean_b$sample_type, "label") <- "Sample Type"
b<- ggplot(clean_b, aes(alginate, avg_cfu, fill = alginate, pattern = sample_type)) +
  stat_summary(aes(fill=alginate), geom = "bar_pattern", 
               pattern_fill="black", 
               pattern_density = .03, 
               width = .8,
               pattern_spacing = .06,
               pattern_key_scale_factor = .2,
               position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4) +
  annotate("text", x = 1, y = 3e8, label = "**", size = 5)+ #onecap vs super
  annotate("text", x = 2, y = 3e8, label = "**", size = 5)+ #twocap vs super
  #geom_bracket(data = clean_b, aes(xmin = .8, 
                                  # xmax = 1.8, 
                                  # y.position = 8,
                                   #label = "*", 
                                  # size = 10, step.increase = .3, 
                                  # label.size = .5)) + #one cap v. two cap+
  #geom_bracket(data = clean_b, aes(xmin = 1.8, 
                                  # xmax = 3, 
                                   #y.position = 8,
                                   #label = "*", 
                                   #size = 10, step.increase = .45, 
                                   #label.size = .5)) + #two cap v. free+
  #geom_bracket(data = clean_b, aes(xmin = 2.2, 
                                   #xmax = 3, 
                                   #y.position = 8,
                                  # label = "****", 
                                   #size = 10, step.increase = .3, 
                                  # label.size = .5)) + #free v. two super
  #geom_bracket(data = clean_b, aes(xmin = 1.2, 
                                   #xmax = 3, 
                                   #y.position = 8,
                                   #label = "****", 
                                   #size = 10, step.increase = .6, 
                                   #label.size = .5)) + #free cap v. one super
  theme_pubr()+
  scale_y_continuous(transform = "log10", limits = c(NA, 1e12), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  scale_x_discrete(labels=c('1%', '2%', 'planktonic'))+
  ggtitle("Viability After 56 Days: Alginate %")+
  xlab("Treatment") + 
  ylab("CFU/mL")+
  scale_pattern_manual(values=c("stripe", "none"), 
                       guide = guide_legend(override.aes = list(fill = "whitesmoke")))+ 
  guides(fill = "none")+
  scale_fill_manual(values = c("darkolivegreen2", "#7CAE00", "cyan3"), 
                    na.value = "cyan3")+ ggeasy::easy_labs()
b
ggdraw(b)  + 
  draw_line( #one cap vs two cap
    x= c(.25, .49), 
    y=c(.69, 0.69), 
    color = "black", size = .5) + 
  annotate("text", x = 0.37, y = 0.7, label = "*", size = 5) + 
  draw_line( #two cap vs free
    x= c(.52, .83), 
    y=c(.69, 0.69), 
    color = "black", size = .5) + 
  annotate("text", x = 0.68, y = 0.7, label = "*", size = 5)+ 
  draw_line( #free vs two super
    x= c(.61, .83), 
    y=c(.61, 0.61), 
    color = "black", size = .5) + 
  annotate("text", x = 0.71, y = 0.62, label = "****", size = 5)+ 
  draw_line( #free vs one super
    x= c(.35, .83), 
    y=c(.8, 0.8), 
    color = "black", size = .5) + 
  annotate("text", x = 0.55, y = 0.81, label = "****", size = 5)
ggsave(here("results", "expB.png"), width = 5000, height = 4000, units = "px", dpi = 800)

#experiment c: chitosan coating
clean_c<- clean_cfu %>% filter(exp == "c")
attr(clean_c$sample_type, "label") <- "Sample Type"
#create function for removing NA from facet label
facet_labeller<- function(variable, value) {
  c("no coating", 
    "coating", 
    ""
  )
}

c<- ggplot(clean_c, aes(encapsulation_treat, avg_cfu, pattern = sample_type, fill = coating)) +
  stat_summary(aes(fill=coating), 
               geom = "bar_pattern", 
               pattern_fill="black", 
               pattern_density = .03, 
               width = .8,
               pattern_spacing = .06,
               pattern_key_scale_factor = .2,
               position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4)+
  theme_pubr()+
  scale_y_continuous(transform = "log10", limits = c(NA, 1e10), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  ggtitle("Viability After 56 Days: Chitosan Coating")+
  xlab("Treatment") + 
  ylab("CFU/mL")+
  facet_wrap(~coating, strip.position = "bottom", scales = "free_x",
             labeller = labeller(coating = as_labeller(facet_labeller)))+
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside")+
  scale_pattern_manual(values=c("stripe", "none"), 
                       guide = guide_legend(override.aes = list(fill = "whitesmoke")))+ 
  guides(fill = "none")+
  scale_fill_manual(values = c("#7CAE00", "lightsalmon", "cyan3"), 
                    na.value = "cyan3")+ ggeasy::easy_labs()
c
#using ggdraw to show significance across facets (which geom_bracket won't work for)
ggdraw(c) + draw_line(
  x= c(.2, .5), 
  y=c(.75, 0.75), 
  color = "black", size = .5) + 
  annotate("text", x = 0.35, y = 0.76, label = "*", size = 5)+ #no coat cap vs coat cap
  annotate("text", x = 0.27, y = 0.7, label = "*", size = 5)+ #no coating cap vs super
  #annotate("text", x = 0.4, y = 0.81, label = "**", size = 5) + 
  #draw_line(
    #x= c(.2, .6), 
    #y=c(.8, 0.8), 
    #color = "black", size = .5)+ #line for cap no coat to cap sup coat
  annotate("text", x = 0.75, y = 0.76, label = "*", size = 5) + #planktonic vs super cap coat
  draw_line(
    x= c(.61, .9), 
    y=c(.75, 0.75), 
    color = "black", size = .5)+ #line for cap no coat to cap sup coat
#annotate("text", x = 0.71, y = 0.79, label = "*", size = 5) + #planktonic vs cap cap coat
 # draw_line(
    #x= c(.5, .9), 
    #y=c(.78, 0.78), 
    #color = "black", size = .5) + #line for super v cap cap coat
annotate("text", x = 0.55, y = 0.81, label = "*", size = 5) + #planktonic vs no coat sup
  draw_line(
    x= c(.32, .9), 
    y=c(.8, 0.8), 
    color = "black", size = .5) #line for planktonic vs no coat sup
ggsave(here("results", "expC.png"), width = 5000, height = 4000, units = "px", dpi = 800)


#Experiment D: cell loading
clean_d <- clean_cfu %>% filter(exp == "d")
attr(clean_d$sample_type, "label") <- "Sample Type"
d<- ggplot(clean_d, aes(cell_loading, avg_cfu, pattern = sample_type, fill = interaction(cell_loading, encapsulation_treat, sep=":"))) +
  stat_summary(aes(fill = interaction(cell_loading, encapsulation_treat, sep=":")), 
               geom = "bar_pattern", 
               pattern_fill="black", 
               pattern_density = .03, 
               width = .8,
               pattern_spacing = .06,
               pattern_key_scale_factor = .2,
               position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4)+
  theme_pubr()+
  scale_y_continuous(transform = "log10", limits = c(NA, 1e10), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  ggtitle("Viability After 56 Days: Cell Loading")+
  xlab("Treatment") + 
  ylab("CFU/mL")+
  facet_wrap(~encapsulation_treat, strip.position = "bottom", scales = "free_x")+
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside")+
  scale_pattern_manual(values=c("stripe", "none"), 
                       guide = guide_legend(override.aes = list(fill = "whitesmoke")))+ 
  guides(fill = "none")+
  scale_fill_manual(values = c("#7CAE00", "palegreen", "cyan3", "lightcyan1"))+ ggeasy::easy_labs()
d
#simplified graph
ggdraw(d) + annotate("text", x = 0.24, y = 0.7, 
                     label = "*", size = 5)
ggsave(here("results", "expD1.png"), width = 5000, height = 4000, units = "px", dpi = 800)
#compressed graph
d2<- ggplot(clean_d, aes(encapsulation_treat, avg_cfu, pattern = sample_type)) +
  stat_summary(aes(fill=encapsulation_treat), 
               geom = "bar_pattern", 
               pattern_fill="black", 
               pattern_density = .03, 
               width = .8,
               pattern_spacing = .06,
               pattern_key_scale_factor = .2,
               position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4)+
  theme_pubr()+
  scale_y_continuous(transform = "log10", limits = c(NA, 1e10), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  ggtitle("Viability After 56 Days: Cell Loading")+
  xlab("Treatment") + 
  ylab("CFU/mL")+
  #facet_wrap(~encapsulation_treat, strip.position = "bottom", scales = "free_x")+
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside")+
  scale_pattern_manual(values=c("stripe", "none"), 
                       guide = guide_legend(override.aes = list(fill = "whitesmoke")))+ 
  guides(fill = "none")+
  scale_fill_manual(values = c("#7CAE00", "cyan3"))+ ggeasy::easy_labs()
d2
#compressed graph with significance
ggdraw(d2) + annotate("text", x = 0.36, y = 0.67, 
                      label = "*", size = 5)+
  annotate("text", x = 0.6, y = 0.71, label = "*", size = 5) + #planktonic vs cap super
  draw_line(
    x= c(.45, .75), 
    y=c(.7, 0.7), 
    color = "black", size = .5)+ 
  annotate("text", x = 0.52, y = 0.78, label = "*", size = 5) + 
  draw_line(
    x= c(.3, .75), 
    y=c(.77, 0.77), 
    color = "black", size = .5) 
ggsave(here("results", "expD2.png"), width = 5000, height = 4000, units = "px", dpi = 800)

#graph split by cell loading with significance for encapsulation treat (CONFUSING)
ggdraw(d) + annotate("text", x = 0.24, y = 0.7, 
                               label = "*", size = 5) + #high cap vs super
annotate("text", x = 0.55, y = 0.77, label = "*", size = 5) + #planktonic vs cap super
  draw_line(
    x= c(.65, .85), 
    y=c(.75, 0.75), 
    color = "black", size = .5) + #line for planktonic super
  draw_line(
    x= c(.27, .47), 
    y=c(.75, 0.75), 
    color = "black", size = .5)+ #cap super
  draw_line(
    x= c(.41, .7), 
    y=c(.76, 0.76), 
    color = "black", size = .5)+
  draw_line(
    x= c(.41, .41), 
    y=c(.75, 0.76), 
    color = "black", size = .5)+
  draw_line(
      x= c(.7, .7), 
      y=c(.75, 0.76), 
      color = "black", size = .5)+
  draw_line(
    x= c(.83, .83), 
    y=c(.75, 0.81), 
    color = "black", size = .5)+
  draw_line(
    x= c(.2, .4), 
    y=c(.79, 0.79), 
    color = "black", size = .5)+ #cap to cap
  draw_line(
    x= c(.3, .3), 
    y=c(.79, 0.81), 
    color = "black", size = .5)+
  draw_line(
      x= c(.3, .83), 
      y=c(.81, 0.81), 
      color = "black", size = .5) + 
  annotate("text", x = 0.55, y = 0.82, label = "*", size = 5)

# cap high vs low = NS
# cap sup high vs low = NS
#free high vs lower = NS
#haven't tested all comparisons yet, need to continue
#2 factor ANOVA on supernatant values (encapsulation_treat, cell_loading, interaction) shows cell_loading NS, interaction NS
#only encapsulation treat significant --> subdivide
#basically, NS effect of cell loading, which just brings us back to the effect of encapsulation. 
#the data here show the same results as earlier - planktonic super concentrations are higher than
#cap super, but capsule concentrations are higher than planktonic super

##experiment A: strain
clean_a<- clean_cfu %>% filter(exp == "a")
attr(clean_a$sample_type, "label") <- "Sample Type"
a<- ggplot(clean_a, aes(encapsulation_treat, avg_cfu, pattern = sample_type, fill = interaction(strain, encapsulation_treat, sep=":"))) +
  stat_summary(aes(fill = interaction(strain, encapsulation_treat, sep=":")), 
               geom = "bar_pattern", 
               pattern_fill="black", 
               pattern_density = .03, 
               width = .8,
               pattern_spacing = .06,
               pattern_key_scale_factor = .2,
               position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4)+
  theme_pubr()+
  scale_y_continuous(transform = "log10", limits = c(NA, 1e10), 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  ggtitle("Viability After 56 Days: Strain Effects")+
  xlab("Treatment") + 
  ylab("CFU/mL")+
  facet_wrap(~strain, strip.position = "bottom", scales = "free_x",
             labeller = labeller(coating = as_labeller(facet_labeller)))+
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside")+ 
  scale_pattern_manual(values=c("stripe", "none"), 
                                                         guide = guide_legend(override.aes = list(fill = "whitesmoke")))+ 
  guides(fill = "none")+
  scale_fill_manual(values = c( "orchid4","#7CAE00", "lightpink", "cyan3"))+ ggeasy::easy_labs()
a
ggdraw(a) + 
  annotate("text", x = 0.24, y = 0.75, label = "**", size = 5) +
  annotate("text", x = 0.34, y = 0.79, label = "****", size = 5)+
  draw_line(
    x= c(.2, .48), 
    y=c(.78, 0.78), 
    color = "black", size = .5)+ 
  annotate("text", x = 0.76, y = 0.79, label = "*", size = 5)+
  draw_line(
    x= c(.62, .9), 
    y=c(.78, 0.78), 
    color = "black", size = .5)
##2 factor ANOVA, strain*encapsulation_treat is NS, strain is **
ggsave(here("results", "expA.png"), width = 5000, height = 4000, units = "px", dpi = 800)















