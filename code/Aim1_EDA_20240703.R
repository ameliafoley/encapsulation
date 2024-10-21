#now, need code to combine plate count data with RFUs for comparison
#load data
long_plate <- read_excel(here::here("data", "longterm_platecounts.xlsx"))
mix <- all_exp %>% filter(sample_type == "mixsup")
plate_join<- long_plate %>% 
  left_join(mix, by = c("exp", "day", "reactor")) 
big_join <- long_plate %>% 
  left_join(all_exp, by = c("exp", "day", "reactor")) 
write_xlsx(big_join, here::here("data", "big_join.xlsx"))
ggplot(data = plate_join, aes(x = value, y = cfu, color = meas)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("RFU") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type + meas) + geom_abline(aes(intercept = 2e+6, slope = 60016)) + 
  geom_abline(aes(intercept = -2e+7, slope = 33585)) +
  geom_smooth(method='lm')
#+ xlim (0, 12500) + ylim(2e7, 8e8)
#this plot looks less crazy when axis is expanding..really these are all similar samples at the same time point, so it's
#like looking at a really zoomed in plot. hopefully linear standard curve will appear when more diverse data points are added. 
#really, it's good that all of these samples from the same day (14) are in a similar range of cfu/mL??

#with the lines from the standard curve, these data points actually look a little better. wonder if I could get an r squared value to see how
#well these points fit those lines. when I put in excel, line of best fit is actually negative...that's not good. 
#looking at this plot again on 6/5/24 - points and line of best fit for mScarlet are actually not too far off from the 
#standard curve! this could actually be working??
ggplot(data = big_join, aes(x = value, y = cfu, color = meas)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("RFU") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type + meas) + 
  geom_smooth(method='lm')
#here, see best relationship between dissolved RFU and CFU 

ggplot(data = big_join, aes(x = OD600, y = cfu)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("RFU") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type) + 
  geom_smooth(method='lm')
cap<- big_join %>% filter(sample_type == c("capsule", "dissolved"))
ggplot(data = cap, aes(x = value, y = cfu, color = meas)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("RFU") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type + meas) + geom_smooth(method='lm')

ggplot(data = big_join, aes(x = day, y = cfu)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("Day") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("CFU Values Over Time") + geom_smooth() + facet_wrap(~encapsulation_treat)

ggplot(data = all_sample, aes(x = day, y = value, color = meas)) +
  geom_boxplot()+
  theme_classic() + 
  xlab("RFU") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type)

samples<- all_sample %>% filter(strain != "blank") %>% filter(sample_type != "super")
super<- samples %>% filter(sample_type=="mixsup")
ggplot(data = super, aes(x = factor(day), y = value, color = meas)) +
  geom_boxplot()+
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type +encapsulation_treat)

btest<- B_all %>% filter(strain != "blank") %>% filter(sample_type != "super") %>% filter(meas == "mVenus")
ggplot(data = btest, aes(x = factor(day), y = value, color = encapsulation_treat)) +
  geom_point()+
  stat_summary(fun = mean, 
               fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)), 
               fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom = 'errorbar',  width = 0.25) +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = 'errorbar',  width = 0.5) +
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type)

#widen data
test <- spread(all_sample, sample_type, value)
test <- all_sample %>% select(!well) %>% select(!OD600) %>% select(!sample) %>% pivot_wider(names_from = sample_type, values_from = value)

#maybe I can try to separate and then rejoin the data 
#separate into sample types
cap<- all_sample %>% filter(sample_type == "capsule") %>% spread(sample_type, value)
dis<- all_sample %>% filter(sample_type == "dissolved") %>% spread(sample_type, value)
mixsup<- all_sample %>% filter(sample_type == "mixsup") %>% spread(sample_type, value)

jointest<- cap %>% left_join(dis, join_by(day, meas, reactor, tech_rep, strain, 
                                          encapsulation_treat, coating, bio_rep, exp, cell_loading))
jointest<- jointest %>% left_join(mixsup, join_by(day, meas, reactor, tech_rep, strain, 
                                                  encapsulation_treat, coating, bio_rep, exp, cell_loading))
#this worked!
ggplot(data = jointest, aes(x = capsule, y = dissolved, color = encapsulation_treat)) +
  geom_point() +
  theme_classic() + 
  xlab("Capsule RFU") + 
  ylab("Dissolved RFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Capsule vs Dissolved RFU")

#does relationship depend on day? 
ggplot(data = jointest, aes(x = capsule, y = dissolved, color = as.factor(day))) +
  geom_point() +
  theme_classic() + 
  xlab("Capsule RFU") + 
  ylab("Dissolved RFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Capsule vs Dissolved RFU Colored by Day")

ggplot(data = jointest, aes(x = dissolved, y = capsule, color = encapsulation_treat)) +
  geom_point() +
  theme_classic() + 
  xlab("Dissolved RFU") + 
  ylab("Capsule RFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Capsule vs Dissolved RFU")

jointest_sample<- jointest %>% filter(strain != "blank")
ggplot(data = jointest_sample, aes(x = capsule, y = mixsup, color = coating)) +
  geom_point() +
  theme_classic() + 
  xlab("Caspsule RFU") + 
  ylab("Supernatant RFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Capsule vs Supernatant RFU") + facet_wrap(~encapsulation_treat+strain) +
  geom_smooth(method='lm', )
#could be interesting result here: while RFU in F199 capsules increased, this did not result in an increase of RFU in the supernatant (as opposed to G7, which saw greater growth in supernatant)

g7 <- big_join %>% filter(strain == "G7") %>% filter(sample_type != "super")
#comparing exp. data points to standard curve lines of best fit
ggplot(data = g7, aes(x = value, y = cfu, color = as.factor(day))) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("RFU") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type + meas, ncol = 2) + geom_abline(aes(intercept = -1e+8, slope = 42602)) + 
  geom_abline(aes(intercept = -1e+8, slope = 39569)) + xlim (0, 40000) + ylim(1, 1e9) 
#putting these figures on the same scale as the standard curves shows that the points cluster along the std curve
#while the small # of data points may create a different trend line, when you zoom out these points make sense (we are just looking at very similar values and therefore seeing artifacts of that)

ggplot(data = g7, aes(x = OD600, y = cfu, color = as.factor(day))) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("OD600") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("OD600 vs CFU Values") +facet_wrap(~sample_type) + geom_abline(aes(intercept = -1e+8, slope = 42602)) + ylim(1, 1e9) 

a_sup<- A_sample %>% filter(sample_type == "mixsup") %>% filter(strain != "blank")
f199<- a_sup %>% filter(strain == "F199")
#trying to look at ExpA supernatant
ggplot(data = a_sup, aes(x = day, y = value, color = encapsulation_treat)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("Day") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time in Exp A") +facet_wrap(~sample_type + strain + meas)
#F199 CFU counts seem to be higher in encapsulated sysmems compared to planktonic, however RFUs are not reflecting that
#perhaps there is fluorescence quenching? however, this did not show up in standard curve. need to look back at that and compare. 

#looking just at f199 on smaller scale
ggplot(data = f199, aes(x = day, y = value, color = encapsulation_treat)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time for F199 in MixSup") +facet_wrap(~sample_type + meas)