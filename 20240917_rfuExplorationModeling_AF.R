library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(ggplot2)
library("writexl", )
library(caret)
library(ggpubr)
library(interactions)
library(jtools)
library(broom.mixed)
library(officer)
library(flextable)
#library(qqplotr)
#library(ggResidpanel)
#library(qqplotr)
library(scales)

## EXPLORATION ##

##Are OD or RFU good predictors of CFU/mL? 

#standard curves suggest yes

standard <- read_excel(here::here("data", "standard_curves.xlsx"))
standard <- standard %>% select(OD600, avg_value, meas, strain, avg_cfu, day)

g7_std<- standard %>% filter(strain == "G7") %>% filter(day == 0)
ggplot(data = g7_std, aes(x = avg_value, y = avg_cfu, color = meas)) +
  geom_point(stat='identity') +
  theme_pubr() + 
  xlab("RFU Value") + 
  ylab("CFU/mL")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Value vs. CFU/mL") + 
  scale_y_continuous(transform = "log10", 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+ 
  geom_smooth(method = 'lm', se = FALSE)+
  annotate("text", x = 15000, y = 10^9, label = "R^2 == 0.97", 
           parse = TRUE,
           size = 5)+
  scale_color_manual(values = c("#F8766D", "#7CAE00"), 
                     name = "Fluorophore")
ggsave(here("results", "std_curve.png"), width = 5000, height = 4000, units = "px", dpi = 800)

g7_std_od<- g7_std[8:15,] 

ggplot(data = g7_std_od, aes(x = OD600, y = avg_cfu)) +
  geom_point(stat='identity') +
  theme_pubr() + 
  xlab("OD600") + 
  ylab("CFU/mL")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("OD600 vs. CFU/mL") + 
  scale_y_continuous(transform = "log10", 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+ 
  geom_smooth(method = 'lm', se = FALSE)+
  scale_color_manual(values = c("#F8766D", "#7CAE00"), 
                     name = "Fluorophore")+
  annotate("text", x = .5, y = 10^9, label = "R^2 == 0.97", 
           parse = TRUE,
           size = 5)
ggsave(here("results", "od_std_curve.png"), width = 5000, height = 4000, units = "px", dpi = 800)
std_od<- lm(avg_cfu ~ OD600, data = g7_std_od)
summary(std_od) #r2 = 0.97

g7_std_s<- g7_std %>% filter(meas == "mScarlet_g7")
std<- lm(avg_cfu ~ avg_value, data = g7_std_s)
summary(std) # r2 = 0.9707

g7_std_v<- g7_std %>% filter(meas == "mVenus")
std_v<- lm(avg_cfu ~ avg_value, data = g7_std_v)
summary(std_v) # r2 = 0.9711

#OD is a standard proxy for bacterial concentration, so let's start there. We see that OD is not a sensitive enough measure
od<- read_excel(here::here("data", "od_cfu.xlsx"))
od<- od %>% filter(sample_type == "supernatant") %>% filter(strain != "blank") #only look at supernatant samples and don't include blank (negative control)
ggplot(data = od, aes(x = avg_OD, y = avg_cfu)) +
  geom_point(stat='identity') +
  theme_pubr() + 
  xlab("OD600") + 
  ylab("CFU/mL")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("OD600 vs. CFU/mL") + 
  scale_y_continuous(transform = "log10", 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+ geom_smooth(method = 'lm', se = TRUE)
ggsave(here("results", "od.png"), width = 5000, height = 4000, units = "px", dpi = 800)
#now on to RFU (relative fluorescence units)
#load data.
#narrow to only the G7 strain and its 2 fluorescent proteins

g7 <- od %>% filter(strain =="G7") %>% filter(meas == c("mVenus", "mScarlet_g7"))

#log transformation 
cfu <- od %>% filter(!is.na(avg_cfu)) 
logcfu <- cfu %>% mutate(logcfu = log10(avg_cfu))
hist(logcfu$logcfu)
logcfu.1<- logcfu %>% filter(meas != "mScarlet_f199") %>% filter( meas != "GFP") %>% filter(strain == "G7") #keep only G7, get rid of F199 proteins


#RFU vs CFU throughout experiment looks pretty bad!
ggplot(data = logcfu.1, aes(x = avg_value, y = avg_cfu, color = meas)) +
  geom_point(stat='identity') +
  theme_pubr() + 
  xlab("RFU Value") + 
  ylab("CFU/mL")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"), 
        legend.position = "none") +
  ggtitle("RFU vs. CFU/mL") + facet_wrap(~meas)+
  scale_y_continuous(transform = "log10", 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  scale_color_manual(values = c("#F8766D", "#7CAE00"), 
                     name = "Fluorophore")
ggsave(here("results", "rfu_cfu_bad.png"), width = 5000, height = 4000, units = "px", dpi = 800)
ggplot(data = logcfu.1, aes(x = avg_value, y = avg_cfu, color = meas)) +
  geom_point(stat='identity') +
  theme_pubr() + 
  xlab("RFU Value") + 
  ylab("CFU/mL")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"), 
        legend.position = "none") +
  ggtitle("RFU vs. CFU/mL") + facet_wrap(~meas)+
  scale_y_continuous(transform = "log10", 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+
  scale_color_manual(values = c("#F8766D", "#7CAE00"), 
                     name = "Fluorophore")+
    geom_smooth(method = 'lm')
ggsave(here("results", "rfu_cfu_bad_combine.png"), width = 5000, height = 4000, units = "px", dpi = 800)

#but, this is because fluorescent proteins are accumulating over time
ggplot(data = g7, aes(x = day, y = avg_value, color = meas)) +
  geom_point(stat='identity') +
  theme_pubr() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Over Time") + facet_wrap(~meas)
ggsave(here("results", "rfu_time.png"), width = 5000, height = 4000, units = "px", dpi = 800)
#so, if you break down RFV v CFU by day, we see that this linear relationship might be preserved
ggplot(data = logcfu.1, aes(x = avg_value, y = avg_cfu, color = as.factor(day))) +
  geom_point(stat='identity') +
  theme_pubr() + 
  xlab("RFU Value") + 
  ylab("CFU/mL")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU vs. CFU/mL by Day") + facet_wrap(~meas)+
  scale_y_continuous(transform = "log10", 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+ 
  geom_smooth(method = 'lm', se = TRUE)+ 
  scale_color_discrete(name = "Day") #rename legend
ggsave(here("results", "rfu_cfu_byday.png"), width = 5000, height = 4000, units = "px", dpi = 800)

## MODELING ##

#linear regression 
model1<- lm(logcfu~avg_value, data = logcfu.1)
summary(model1) #alone, avg_value is not explanatory and even shows a negative relationship with avg_cfu (poor r2)
model4.1 <- lm(logcfu ~ avg_value + meas + day, data = logcfu.1)
summary(model4.1) #improves model fit to .7534!
plot_summs(model4.1, scale = TRUE)
ggsave(here("results", "model4.1_cf.png"), width = 5000, height = 4000, units = "px", dpi = 800)
plot(model4.1$residuals)
par(mfrow = c(2,2)) 
plot(model4.1)
ggsave(here("results", "model4.1_residuals.png"), width = 5000, height = 4000, units = "px", dpi = 800)

#resid_panel(model4.1)
ggplot(data = logcfu.1, aes(x = avg_value, y = avg_cfu, color = as.factor(day))) +
  geom_point(stat='identity') +
  theme_pubr() + 
  xlab("RFU Value") + 
  ylab("CFU/mL")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Supernatant Over Time") + facet_wrap(~meas)+
  scale_y_continuous(transform = "log10", 
                     breaks = trans_breaks('log10', function(x) 10^x), 
                     labels = trans_format('log10', math_format(10^.x)))+ geom_smooth(method = 'lm', se = TRUE)+ 
  scale_color_discrete(name = "Day") #rename legend
#plus standard curve
#add in standard curve data to better calibrate? 
standard <- read_excel(here::here("data", "standard_curves.xlsx"))
standard <- standard %>% select(OD600, avg_value, meas, strain, avg_cfu, day)
cfuplus<- cfu %>% full_join(standard) #add standard curve to experimental data set
logcfuplus<- cfuplus %>% mutate(logcfu = log10(avg_cfu)) #log transform
logcfuplus1<- logcfuplus %>% filter(meas != "mScarlet_f199") %>% filter( meas != "GFP") %>% filter(strain == "G7") %>% filter(avg_cfu != 0) #filter for strain G7, no blanks, no zero
model2<- lm(logcfu~ avg_value, data = logcfu.1) #predict avg rfu value with new dataset
summary(model2) #very poor fit
model5.1<- lm(logcfu ~ avg_value + meas + day, data = logcfuplus1) #predict avg rfu value with time variable with new data
summary(model5.1) #with std curve data added, r2 decreases (due to more variability than experimental data alone?)
plot_summs(model5.1, scale = TRUE) #show coefficients
ggsave(here("results", "model5.1_cf.png"), width = 5000, height = 4000, units = "px", dpi = 800)
export_summs(model1, model4.1, model5.1, scale = TRUE)
check<- lm(logcfu ~ avg_value + meas, data = logcfu.1) #model without time/day variable
summary(check)
anova(check, model4.1)
#export to word file
export_summs(check, model4.1, model5.1, scale = TRUE, to.word = TRUE, word.file = here("results", "test.docx"))

ggplot(logcfuplus1, aes(avg_value, avg_cfu, color = as.factor(day))) +
  stat_summary(geom = "point", width = .8, position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4) +
  theme_pubr() +
  xlab("RFU Value") + 
  ylab("CFU/mL")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  scale_y_continuous(transform = "log10", limits = c(1e7, 1e9),
                                    breaks = trans_breaks('log10', function(x) 10^x), 
                                    labels = trans_format('log10', math_format(10^.x))) + 
  geom_smooth(method = 'lm') + 
  facet_wrap(~meas) + scale_color_discrete(name = "Day") #rename legend
ggsave(here("results", "rfu_cfu_std_byday.png"), width = 5000, height = 4000, units = "px", dpi = 800)
 
#attempting f-test to see if models are significantly different 
anova(model2, model4.1)




# conclusion: a multiple linear regression can highlight important variables and give us *rough* predictions based on RFU data?

# an additional thought: what does a model with just time as the predictor look like? is time really the important variable, does avg RFU value add anything? 
model_t<- lm(logcfu ~ day, data = logcfu.1)
summary(model_t)
#r2 is still high here - day/time explains a good bit of the data

anova(model4.1, model_t) #is the model with time significant from the model with avg RFU value AND time? 
