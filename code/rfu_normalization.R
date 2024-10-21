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

#load data. 
df <- read_excel(here::here("data", "allrfu_cfu.xlsx"))
super<- df %>% filter(sample_type == "supernatant") %>% filter(strain != "blank")
g7<- super %>% filter(strain == "G7") %>% filter(meas == c("mVenus", "mScarlet_g7"))
ggplot(data = g7, aes(x = day, y = avg_value, color = meas)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Supernatant Over Time") + facet_wrap(~encapsulation_treat+ strain) + geom_smooth(method = 'loess')
modelrfu<- lm(avg_value ~ day, data = g7)
summary(modelrfu)

ggplot(g7, aes(day, avg_value, color = meas)) +
  stat_summary(geom = "point", width = .8, position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4) +
  theme_pubr()+ facet_wrap(~encapsulation_treat+strain)

a<- g7 %>% group_by(day) 
process<-  preProcess(as.data.frame(a), method=c("range"))
norm_scale<- predict(process, as.data.frame(a))

# custom function to implement min max scaling
minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
norm<- as.data.frame(lapply(a$avg_value, minMax))

b<- a %>% group_by(day) %>% mutate_at("avg_value", ~scale(.) %>% as.vector)
#plot scaled data
ggplot(b, aes(day, avg_value, color = meas)) +
  stat_summary(geom = "point", width = .8, position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4) +
  theme_pubr()+ facet_wrap(~encapsulation_treat+strain)
#scaled
ggplot(b, aes(avg_value, avg_cfu, color = as.factor(day))) +
  geom_point() + geom_smooth(method = 'lm') + scale_y_continuous(transform = "log10")+facet_wrap(~meas)
#not scaled
ggplot(g7, aes(avg_value, avg_cfu, color = as.factor(day))) +
  geom_point() + geom_smooth(method = 'lm') + scale_y_continuous(transform = "log10")+facet_wrap(~meas)

#scaling doesn't solve the issue of changing relationship over time; however, I have a portion of data that 
#has RFU and CFU data as well as day, which maybe I can use to build a linear regression model to predict CFU
#on the rest of my samples based on the available meta data

#plot scaled data for day 56 alone
b %>% filter(day == 56) %>% 
  ggplot(aes(avg_value, avg_cfu, color = meas)) + 
  geom_point(aes(color=encapsulation_treat, stat='identity')) + 
  geom_smooth(method = 'lm')
d56<- b %>% filter(day ==56) 
model56<- lm(avg_cfu ~ avg_value + meas + day + encapsulation_treat, data = g7)
summary(model56)

#attempt multiple linear regression
#subset df into training data to build model (data containing cfu info)
cfu <- super %>% filter(!is.na(avg_cfu))
model <- lm(avg_cfu ~ avg_value + meas + day + strain, data = cfu)
summary(model)
plot_summs(model)

hist(cfu$avg_cfu)


modela<- lm(avg_cfu ~ avg_value + day + meas, data = g7)
summary(modela)
plot_summs(modela)

cfu %>% ggplot(aes(avg_value, avg_cfu, color = as.factor(day))) + 
  geom_point() + 
  geom_smooth(method = 'lm') + facet_wrap(~meas)#inner filter effect on day 14?

modelb<- lm(avg_cfu ~ avg_value+day, data = g7)
summary(modelb)

#add in standard curve data to better calibrate? 
standard <- read_excel(here::here("data", "standard_curves.xlsx"))
standard <- standard %>% select(OD600, avg_value, meas, strain, avg_cfu, day)
cfuplus<- cfu %>% full_join(standard)

#cfuplus<- cfuplus %>% filter(day != 14) #trying without day 14 data

model2 <- lm(avg_cfu ~ avg_value + meas + strain + day, data = cfuplus)
summary(model2) #in this model, avg_value positively associated and significant
sigma(model2)/mean(cfuplus$avg_cfu)
sigma(model)/mean(cfu$avg_cfu)

cfuplus %>% filter(meas == "mScarlet_g7") %>% filter(day != 14) %>%
  ggplot(aes(avg_value, avg_cfu, color = as.factor(day))) + 
  geom_point() + 
  geom_smooth(method = 'lm') + facet_wrap(~meas)#inner filter effect on day 14?

model3 <- lm(avg_cfu ~ avg_value + meas + day, data = g7)
summary(model3)
plot_summs(model3)

#log transformation before linear regression
logcfu <- cfu %>% mutate(logcfu = log10(avg_cfu))
hist(logcfu$logcfu)
model3.1 <- lm(logcfu ~ avg_value + meas, data = logcfu) #model does not fit well with just RFU value data
summary(model3.1)
model4 <- lm(logcfu ~ avg_value + meas + day, data = logcfu)
summary(model4) #adding day greatly improves model fit, supports hypothesis that relationship changes over time
plot(model4$residuals)
plot(model4)
plot_summs(model4, scale = TRUE)
plot_coefs(model4, plot.distributions = TRUE, inner_ci_level = .9)
#still looks like model4 is best model (no standard curve data), should probably remove scarletf199

logcfu.1<- logcfu %>% filter(meas != "mScarlet_f199") %>% filter(meas != "GFP")
model4.1 <- lm(logcfu ~ avg_value + meas + day, data = logcfu.1)
summary(model4.1) #improves model fit to .7534!
plot_summs(model4.1, scale = TRUE)

allavg <- read_excel(here::here("data", "allavg.xlsx"))
allavg <- allavg %>% filter(strain != "blank") %>% filter(meas!="mScarlet_f199") %>% filter(strain == "G7")

allavg$pred <- predict(model4.1, newdata = allavg)
allavg$predcfu <- 10^(allavg$pred)

ggplot(allavg, aes(day, predcfu, color = encapsulation_treat)) +
  stat_summary(geom = "point", width = .8, position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4) +
  theme_pubr()+ facet_wrap(~strain)
ggplot(allavg, aes(day, avg_value)) +
  stat_summary(geom = "point", width = .8, position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4) +
  theme_pubr()+ facet_wrap(~encapsulation_treat+strain)
g7pred<- allavg %>% filter(strain == "G7")
ggplot(g7pred, aes(day, predcfu, color = alginate)) +
  stat_summary(geom = "point", width = .8, position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4) +
  theme_pubr()
ggplot(g7pred, aes(day, avg_value, color = alginate)) +
  stat_summary(geom = "point", width = .8, position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4) +
  theme_pubr()
ggplot(g7pred, aes(day, predcfu, color = encapsulation_treat)) +
  stat_summary(geom = "point", width = .8, position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4) +
  theme_pubr() + scale_y_continuous(transform = "log10", limits = c(1e7, 1e9),
                                    breaks = trans_breaks('log10', function(x) 10^x), 
                                    labels = trans_format('log10', math_format(10^.x)))
ggplot(logcfu, aes(day, avg_cfu, color = encapsulation_treat)) +
  stat_summary(geom = "point", width = .8, position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4) +
  theme_pubr() + scale_y_continuous(transform = "log10", limits = c(1e7, 1e9),
                                    breaks = trans_breaks('log10', function(x) 10^x), 
                                    labels = trans_format('log10', math_format(10^.x)))


#log transformation provides much better residuals since data is not so inflated... better model fit!

#log transformation plus standard curve data
logcfuplus<- cfuplus %>% mutate(logcfu = log10(avg_cfu)) %>% filter(avg_cfu != 0)
model5 <- lm(logcfu ~ avg_value + meas + day + strain, data = logcfuplus)
summary(model5)
plot(model5$residuals)

#test with standard curve, but only G7
logcfuplus1<- logcfuplus %>% filter(strain == "G7") %>% filter(meas == c("mScarlet_g7", "mVenus"))
model5.1<- lm(logcfu ~ avg_value + meas + day, data = logcfuplus1)
summary(model5.1)

ggplot(logcfuplus1, aes(avg_value, avg_cfu, color = as.factor(day))) +
  stat_summary(geom = "point", width = .8, position = position_dodge(0.8))+
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8), size = .4) +
  theme_pubr() + scale_y_continuous(transform = "log10", limits = c(1e7, 1e9),
                                    breaks = trans_breaks('log10', function(x) 10^x), 
                                    labels = trans_format('log10', math_format(10^.x))) + geom_smooth(method = 'lm')

#test model with all variables
#they don't all run, and adding individually does not seem to benefit model
model6 <- lm(logcfu ~ avg_value + meas + day + strain + encapsulation_treat, data = logcfu)
summary(model6)

#add OD to model
od<- read_excel(here::here("data", "od_cfu.xlsx"))
od <- od %>% filter(strain != "blank") %>% filter(avg_cfu != 0)
logod<- od %>% mutate(logcfu = log10(avg_cfu)) 
model7<- lm(logcfu ~ avg_value + meas + day + avg_OD, data = logod)
summary(model7)
par(mfrow=c(2,2))
plot_summs(model7, scale = TRUE)

#OD and standard curve in model
standard<- rename(standard, avg_OD = OD600)
test<- od %>% full_join(standard)
test<- test %>% mutate(logcfu = log10(avg_cfu)) 
test<- test %>% filter(avg_cfu != 0) %>% 
  select(day, meas, strain, encapsulation_treat, avg_value, avg_OD, logcfu, avg_cfu)
is.na(test) %>% summary()
model8<- lm(logcfu ~ avg_value + meas + day + avg_OD + strain, data = test)
summary(model8)

test %>% ggplot(aes(avg_value, avg_cfu, shape = as.factor(day), color = avg_OD)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + facet_wrap(~meas) #day 14 still the only one exhibiting weird negative behavior
#maybe try removing day 14 data and trying model? 
test<- test %>% filter(day !=14)
modelc<- lm(avg_cfu ~ avg_value*day + meas + avg_OD + strain, data = test)
summary(modelc)#this gives a positive value for avg_value intercept
interact_plot(modelc, pred = avg_value, modx = day, plot.points = TRUE)
plot_coefs(modelc, scale = TRUE)
summ(modelc, scale = TRUE)
plot_summs(modelc, scale = TRUE) #this reflects the values I'm seeing in summary()!! 

#try with just G7 to remove model variable
test1<- test %>% filter(strain == "G7") %>% filter(meas != "mScarlet_f199")
modeld<- lm(logcfu ~ avg_value + day + meas + avg_OD, data = test1)
summary(modeld)
plot_summs(modeld, scale = TRUE)

modele<- lm(logcfu ~ avg_value + day, data = test1)
summary(modele)
plot_summs(modele, scale = TRUE)


#interaction term in moel
model9<- lm(logcfu ~ avg_value + day + meas + avg_OD + strain, data = logod)
summary(model9)
plot_summs(model9)
#adding interaction term did not improve model, and interaction term was not significant
cat<- logod 
cat$day <- as.factor(cat$day)
glimpse(cat)
model10<- lm(logcfu ~ avg_value*day + meas + avg_OD + strain, data = cat)
summary(model10) #model looks better with day as categorical data
interact_plot(model10, pred = avg_value, modx = day, plot.points = TRUE) #this recreates the same thing I was seeing in my earlier plots





#day as categorical - doesn't improve in this combo
cattest<- test
cattest$day<- as.factor(cattest$day)
#cattest<- cattest %>% filter(avg_value < 25000)
model11<- lm(logcfu ~ avg_value*day + meas + avg_OD + strain, data = cattest)
summary(model11)
interact_plot(model11, pred = avg_value, modx = day, plot.points = TRUE)

effect_plot(model11, pred = avg_value, interval = TRUE, plot.points = TRUE, jitter = 0.05)
plot_summs(model11)
10^3.415e-05


