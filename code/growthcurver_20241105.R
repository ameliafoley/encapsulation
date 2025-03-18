#trying growthcurver
library(growthcurver)

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

#transpose to format with time in columns and well in rows - F199
f199_w<- f199 %>% dplyr::select (-OD600, -media, -strain, -treatment, -initial) %>% pivot_wider(names_from = well, values_from = corrected)

gc_out <- SummarizeGrowthByPlate(f199_w, bg_correct = "none")
plot(gc_out)
#add sample info
data_location4 <- here::here("data","f199_GC_PL_20241016.xlsx")
#load data. 
plate <- read_excel(data_location4)
plate$well <- paste(plate$row, plate$column, sep="") 
plate <- plate %>% dplyr::select(3:6)

#join names to sample data file
f199.s <- left_join(gc_out, plate, join_by(sample == well))

#transpose to format with time in columns and well in rows - BOTH
both_w<- both %>% dplyr::select (-OD600, -media, -strain, -treatment, -initial) %>% pivot_wider(names_from = well, values_from = corrected)

gc_both <- SummarizeGrowthByPlate(both_w, bg_correct = "none")

#path to data
data_location4 <- here::here("data","f199_g7_GC_PL_20240910.xlsx")

#load data. 
plate <- read_excel(data_location4)
plate$well <- paste(plate$row, plate$column, sep="") 
plate <- plate %>% dplyr::select(3:6)

#join names to sample data file
both.s <- left_join(gc_both, plate, join_by(sample == well))

#transpose to format with time in columns and well in rows - G7
g7_w<- g7 %>% dplyr::select (-OD600, -media, -strain, -treatment, -initial) %>% pivot_wider(names_from = well, values_from = corrected)

gc_g7 <- SummarizeGrowthByPlate(g7_w, bg_correct = "none")

#path to data
#path to data
data_location4 <- here::here("data","g7_GC_PL_20241011.xlsx")

#load data. 
plate <- read_excel(data_location4)
plate$well <- paste(plate$row, plate$column, sep="") 
plate <- plate %>% dplyr::select(3:6)

#join names to sample data file
g7.s <- left_join(gc_g7, plate, join_by(sample == well))

#transpose to format with time in columns and well in rows - G72
g72_w<- g72 %>% dplyr::select (-OD600, -media, -strain, -treatment, -initial) %>% pivot_wider(names_from = well, values_from = corrected)

gc_g72 <- SummarizeGrowthByPlate(g72_w, bg_correct = "none")

#path to data
#path to data
data_location4 <- here::here("data","g7_GC_PL_20241011.xlsx")

#load data. 
plate <- read_excel(data_location4)
plate$well <- paste(plate$row, plate$column, sep="") 
plate <- plate %>% dplyr::select(3:6)

#join names to sample data file
g72.s <- left_join(gc_g72, plate, join_by(sample == well))

#transpose to format with time in columns and well in rows - F1992
f1992_w<- f1992 %>% dplyr::select (-OD600, -media, -strain, -treatment, -initial) %>% pivot_wider(names_from = well, values_from = corrected)

gc_f1992 <- SummarizeGrowthByPlate(f1992_w, bg_correct = "none")

#path to data
data_location4 <- here::here("data","f199_GC_PL_20241016.xlsx")

#load data. 
plate <- read_excel(data_location4)
plate$well <- paste(plate$row, plate$column, sep="") 
plate <- plate %>% dplyr::select(3:6)

#join names to sample data file
f1992.s <- left_join(gc_f1992, plate, join_by(sample == well))

gc<- rbind(f199.s, both.s, g7.s, g72.s, f1992.s)
plot(gc)

#k - carrying capacity
ggplot(data = gc, aes(x = treatment, y = k, fill = treatment)) +
  stat_summary(geom="col", fun = mean) +
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Treatment") + ylab("K") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + facet_wrap(~strain + media)
#r - intrinsic growth rate
ggplot(data = gc, aes(x = treatment, y = r, fill = treatment)) +
  stat_summary(geom="col", fun = mean) +
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Treatment") + ylab("r") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + facet_wrap(~strain + media)
#t-mid, time to point of inflection (1/2K) (bacterial growth reaches midpoint)
ggplot(data = gc, aes(x = treatment, y = t_mid, fill = treatment)) +
  stat_summary(geom="col", fun = mean) +
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Treatment") + ylab("t-mid") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + facet_wrap(~strain + media)
#t-gen, generation time
ggplot(data = gc, aes(x = treatment, y = t_gen, fill = treatment)) +
  stat_summary(geom="col", fun = mean) +
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Treatment") + ylab("t-gen") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + facet_wrap(~strain + media)

clean_sum <- gc %>% dplyr::select(-sample) %>% group_by(media, strain, treatment) %>% 
  #summarize(mean_k = mean(k), se_k = std.error(k)#
    
gc_anova<- aov(r ~ strain*media*treatment, data = gc) 
summary(gc_anova)
gc_lb<- gc %>% filter (media == "LB")
lb_anova <- aov(r ~ strain*treatment, data = gc_lb) %>% tukey_hsd()
summary(lb_anova)
lb_tukey<- TukeyHSD(lb_anova, conf.level=.95) #Tukey

gc_srb<- gc %>% filter (media == "sRB15")
srb_anova <- aov(r ~ strain*treatment, data = gc) #%>% tukey_hsd()
summary(lb_anova)
                                                                                                                                                                                            mean_r = mean(r), se_r = std.error(r))
#not sure what this line was for? can't find clean anywhere in this script or the figure script. 
