library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(ggplot2)
#path to data
data_location0 <- here::here("data","G7Pilot_Day0_20231104.xlsx")
data_location3 <- here::here("data","G7Pilot_Day3_20231106.xlsx")
data_location7 <- here::here("data","G7Pilot_Day7_20231110.xlsx")
data_location11<- here::here("data", "G7Pilot_Day11_20231114.xlsx")
data_location14<- here::here("data", "G7Pilot_Day14_20231117.xlsx")
data_location24<- here::here("data", "G7Pilot_Day24_20231127.xlsx")
data_location42<- here::here("data", "G7Pilot_Day42_20231215.xlsx")

#load data. 
day0 <- read_excel(data_location0)
day3 <- read_excel(data_location3)
day7 <- read_excel(data_location7)
day11<- read_excel(data_location11)
day14<- read_excel(data_location14)
day24<- read_excel(data_location24)
day42<- read_excel(data_location42)

glimpse(day0)

#clean data
day0<- day0 %>% na.omit() %>% select(!2)
day3<- day3 %>% na.omit() %>% select(!2)
day7<- day7 %>% na.omit() %>% select(!2)
day11<- day11 %>% na.omit() %>% select(!2)
day14<- day14 %>% na.omit() %>% select(!2)
day24<- day24 %>% na.omit() %>% select(!2)
day42<- day42 %>% na.omit() %>% select(!2)
#rename
colnames(day0)[1] = "well"
colnames(day3)[1] = "well"
colnames(day7)[1] = "well"
colnames(day11)[1] = "well"
colnames(day14)[1] = "well"
colnames(day24)[1] = "well"
colnames(day42)[1] = "well"

#add time point to data frame
day0$day <- 0
day3$day<- 3
day7$day<- 7
day11$day<- 11
day14$day<- 14
day24$day<- 24
day42$day<- 42

#load plate layouts
data_locationtop <- here::here("data", "plate_G7Pilot_top.xlsx")
data_locationbottom <- here::here("data", "plate_G7Pilot_bottom.xlsx")

plate_top <- read_excel(data_locationtop)
plate_bottom <- read_excel(data_locationbottom)
#clean and wrangle plate data
#for top
plate_top[] <- lapply(plate_top, as.character)
plate_top <- plate_top %>% pivot_longer(cols = !row, 
                                names_to = "column_num", 
                                values_to = "sample")
plate_top$well <- paste(plate_top$row, plate_top$column_num, sep="") 
plate_top <- plate_top %>% select(3:4)
#for bottom
glimpse(plate_bottom)
plate_bottom[] <- lapply(plate_bottom, as.character)
plate_bottom <- plate_bottom %>% pivot_longer(cols = !row, 
                                names_to = "column_num", 
                                values_to = "sample")
plate_bottom$well <- paste(plate_bottom$row, plate_bottom$column_num, sep="") 
plate_bottom <- plate_bottom %>% select(3:4)


#join names to sample data file
#day0
join0 <- left_join(day0, plate_top, by = "well")

long0 <- join0 %>% gather(meas, value, mScarlet:mVenus, factor_key=TRUE)
glimpse(long0)

#day3
join3 <- left_join(day3, plate_bottom, by = "well")

long3 <- join3 %>% gather(meas, value, mScarlet:mVenus, factor_key=TRUE)
glimpse(long3)

#day7
join7 <- left_join(day7, plate_top, by = "well")

long7 <- join7 %>% gather(meas, value, mScarlet:mVenus, factor_key=TRUE)
glimpse(long7)

#day11 bottom
join11 <- left_join(day11, plate_bottom, by = "well")

long11 <- join11 %>% gather(meas, value, mScarlet:mVenus, factor_key=TRUE)
glimpse(long11)

#day14 top
join14 <- left_join(day14, plate_top, by = "well")

long14 <- join14 %>% gather(meas, value, mScarlet:mVenus, factor_key=TRUE)
glimpse(long14)

#day24 bottom
join24 <- left_join(day24, plate_bottom, by = "well")

long24 <- join24 %>% gather(meas, value, mScarlet:mVenus, factor_key=TRUE)
glimpse(long24)

#day42 top
join42 <- left_join(day42, plate_top, by = "well")

long42 <- join42 %>% gather(meas, value, mScarlet:mVenus, factor_key=TRUE)
glimpse(long42)

#combine data from multiple days
all<- bind_rows(list(long0, long3, long7, long11, long14, long24, long42))

#Plotting! 

#plot results
ggplot(data = all, aes(x = day, y = value, color = sample)) +
  geom_point(stat='identity') +
  geom_smooth(se = FALSE)+
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("G7 Pilot Fluor Over Time")+
  scale_color_manual(values = c("mScarlet" = "indianred3", 
                                "mVenus" = "yellow1"))
# this is too messy. we need to clean this data some more and use the sample names to create more identifying variables to subset by

all <- all %>% mutate(type = case_when(startsWith(all$sample, "s") ~ "supernatant",
                                      startsWith(all$sample, "b") ~ "microcapsule"))
                                  
scar<- all %>% filter(meas == "mScarlet") %>% na.omit() 

ggplot(data = scar, aes(x = day, y = value, color = sample)) +
  geom_point(stat='identity') +
  geom_smooth(se = FALSE)+
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("G7 Pilot Fluor Over Time")+
  facet_wrap(~type, nrow = 2)

# this format is better, but we still need to add more metadata. I want to add a replicate column so that I can average the data
# I also need to add this info: coating, biological replicate, planktonic or microcapsule

sel <- all %>% na.omit()
sel<- sel %>% filter(sample != "srb15")
sel<- sel %>% extract(sample, into = c("reactor", "replicate"), "(.*)_([^_]+)$") #split by underscore

#coating
test <- sel %>% mutate(coating = case_when(endsWith(sel$reactor, "s1") ~ "chitosan",
                                           endsWith(sel$reactor, "b1") ~ "chitosan",
                                           endsWith(sel$reactor, "b2") ~ "chitosan",
                                           endsWith(sel$reactor, "s2") ~ "chitosan",
                                           endsWith(sel$reactor, "b2") ~ "chitosan",
                                           endsWith(sel$reactor, "3") ~ "no coating", 
                                           endsWith(sel$reactor, "4") ~ "no coating", 
                                           endsWith(sel$reactor, "7") ~ "chitosan", 
                                           endsWith(sel$reactor, "8") ~ "chitosan", 
                                           endsWith(sel$reactor, "9") ~ "no coating", 
                                           endsWith(sel$reactor, "10") ~ "no coating", 
                                           endsWith(sel$reactor, "5") ~ "free", 
                                           endsWith(sel$reactor, "6") ~ "free", 
                                           endsWith(sel$reactor, "11") ~ "free", 
                                           endsWith(sel$reactor, "12") ~ "free"))
#add strain/control
test <- test %>% mutate(strain = case_when(endsWith(sel$reactor, "s1") ~ "G7",
                                           endsWith(sel$reactor, "b1") ~ "G7",
                                           endsWith(sel$reactor, "s2") ~ "G7",
                                           endsWith(sel$reactor, "b2") ~ "G7",
                                           endsWith(sel$reactor, "3") ~ "G7", 
                                           endsWith(sel$reactor, "4") ~ "G7", 
                                           endsWith(sel$reactor, "7") ~ "blank", 
                                           endsWith(sel$reactor, "8") ~ "blank", 
                                           endsWith(sel$reactor, "9") ~ "blank", 
                                           endsWith(sel$reactor, "10") ~ "blank", 
                                           endsWith(sel$reactor, "5") ~ "G7", 
                                           endsWith(sel$reactor, "6") ~ "G7", 
                                           endsWith(sel$reactor, "11") ~ "blank", 
                                           endsWith(sel$reactor, "12") ~ "blank"))
#treatment (planktonic or microcapsule)
test <- test %>% mutate(treatment = case_when(endsWith(sel$reactor, "s1") ~ "capsule",
                                              endsWith(sel$reactor, "b1") ~ "capsule",
                                           endsWith(sel$reactor, "s2") ~ "capsule",
                                           endsWith(sel$reactor, "b2") ~ "capsule",
                                           endsWith(sel$reactor, "3") ~ "capsule", 
                                           endsWith(sel$reactor, "4") ~ "capsule", 
                                           endsWith(sel$reactor, "7") ~ "capsule", 
                                           endsWith(sel$reactor, "8") ~ "capsule", 
                                           endsWith(sel$reactor, "9") ~ "capsule", 
                                           endsWith(sel$reactor, "10") ~ "capsule", 
                                           endsWith(sel$reactor, "5") ~ "free", 
                                           endsWith(sel$reactor, "6") ~ "free", 
                                           endsWith(sel$reactor, "11") ~ "free", 
                                           endsWith(sel$reactor, "12") ~ "free"))
#biological replicate 
test <- test %>% mutate(biorep = case_when(endsWith(sel$reactor, "s1") ~ "A",
                                           endsWith(sel$reactor, "b1") ~ "A",
                                           endsWith(sel$reactor, "s2") ~ "B",
                                              endsWith(sel$reactor, "b2") ~ "B", 
                                              endsWith(sel$reactor, "3") ~ "A", 
                                              endsWith(sel$reactor, "4") ~ "B", 
                                              endsWith(sel$reactor, "7") ~ "na", 
                                              endsWith(sel$reactor, "8") ~ "na", 
                                              endsWith(sel$reactor, "9") ~ "na", 
                                              endsWith(sel$reactor, "10") ~ "na", 
                                              endsWith(sel$reactor, "5") ~ "A", 
                                              endsWith(sel$reactor, "6") ~ "B", 
                                              endsWith(sel$reactor, "11") ~ "na", 
                                              endsWith(sel$reactor, "12") ~ "na"))
library("writexl", )
data_location2 <- here::here("data","pilotdata.xlsx")
write_xlsx(test, data_location2)
#split into measures
microcapsule<- test %>% filter(type == "microcapsule") %>% filter(meas == "mScarlet")
supernatant<- test %>% filter(type == "supernatant") %>% filter(meas == "mScarlet")
ggplot(data = supernatant, aes(x = jitter(day, 0.3), y = value, color = coating, shape = biorep)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("G7 In Supernatant Over Time")+
  facet_wrap(~strain + treatment, ncol = 2)+
  scale_y_continuous(trans="log10")

ggplot(data = microcapsule, aes(x = jitter(day, 0.25), y = value, color = coating, shape = biorep)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Bacterial Levels Within Microcapsules")+facet_wrap(~strain, nrow = 1)+
  scale_y_continuous(trans='log10')

#comparing specific factors
sample<- microcapsule %>% filter(strain == "G7")
ggplot(data = sample, aes(x = jitter(day, 0.25), y = value, color = coating, shape = biorep)) +
  geom_point(stat='identity') +
  geom_smooth(se = FALSE, span = 1) +
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Bacterial Levels Within Microcapsules")+facet_wrap(~coating, nrow = 1)+
  scale_y_continuous(trans='log10')

#take means
summ <- test %>% group_by(day, meas, type, coating, strain, treatment) %>% summarize(avg_rfu = mean(value), 
                                                           sd = sd(value))
data_location2 <- here::here("data","pilotsum.xlsx")
write_xlsx(summ, data_location2)
microcapsule<- summ %>% filter(type == "microcapsule") %>% filter(meas == "mScarlet")
supernatant<- summ %>% filter(type == "supernatant") %>% filter(meas == "mScarlet")
sample<- microcapsule %>% filter(strain == "G7")
# microcapsule sums
ggplot(data = sample, aes(x = day, y = avg_rfu, color = coating)) +
  geom_point(stat='identity') +
  geom_errorbar(aes(ymin=avg_rfu-sd, ymax=avg_rfu+sd), width=.2,
                position=position_dodge(.9))+
  geom_smooth(se = FALSE, span = 1) +
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(axis.text.x=element_text(angle = 0, hjust = .5, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.position = "bottom", 
        panel.grid.minor.y = element_line(color = "grey", 
                                          linetype = "dashed")) +
  ggtitle("Bacterial Levels Within Microcapsules")+
  facet_wrap(~factor(coating, c("no coating", "chitosan", "free")), nrow = 1)+
  scale_y_continuous(trans='log10')+
  theme(strip.text = element_text(
    size = 14))+
  scale_color_manual(values = c("no coating" = "aquamarine4", 
                                "chitosan" = "olivedrab3", 
                                "free" = "coral1"))+
  theme(panel.spacing.x = unit(5, "mm"))

#supernatant sums
super<- supernatant %>% filter(strain == "G7")
ggplot(data = super, aes(x = day, y = avg_rfu, color = coating)) +
  geom_point(stat='identity') +
  geom_errorbar(aes(ymin=avg_rfu-sd, ymax=avg_rfu+sd), width=.2,
                position=position_dodge(.9))+
  geom_smooth(se = FALSE, span = 1) +
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(axis.text.x=element_text(angle = 0, hjust = .5, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.position = "bottom", 
        panel.grid.minor.y = element_line(color = "grey", 
                                          linetype = "dashed")) +
  ggtitle("Bacterial Levels Released in Supernatant")+
  facet_wrap(~factor(coating, c("no coating", "chitosan", "free")), nrow = 1)+
  theme(strip.text = element_text(
    size = 14)) +
  scale_color_manual(values = c("no coating" = "aquamarine4", 
                                "chitosan" = "olivedrab3", 
                                "free" = "coral1"))+
  theme(panel.spacing.x = unit(5, "mm")) #add spacing to facet panels

#add grouped bar chart for supernatant
ggplot(super, aes(fill=coating, y=avg_rfu, x = as.factor(day))) + 
  geom_bar(position="dodge", stat="identity")+
geom_errorbar(aes(ymin=avg_rfu-sd, ymax=avg_rfu+sd), width=.2,
              position=position_dodge(.9))+
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(axis.text.x=element_text(angle = 0, hjust = .5, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.position = "bottom", 
        panel.grid.minor.y = element_line(color = "grey", 
                                          linetype = "dashed")) +
  ggtitle("Bacterial Levels Released in Supernatant")+
  theme(strip.text = element_text(
    size = 14)) +
  scale_fill_manual(values = c("no coating" = "aquamarine4", 
                                "chitosan" = "olivedrab3", 
                                "free" = "coral1"))+
  scale_x_discrete(breaks=c(0,3,7,11,14,24,42))


#add grouped bar chart for within microcapsules
ggplot(sample, aes(fill=coating, y=avg_rfu, x = as.factor(day))) + 
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=avg_rfu-sd, ymax=avg_rfu+sd), width=.2,
                position=position_dodge(.9))+
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(axis.text.x=element_text(angle = 0, hjust = .5, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.position = "bottom", 
        panel.grid.minor.y = element_line(color = "grey", 
                                          linetype = "dashed")) +
  ggtitle("Bacterial Levels Within Microcapsules")+
  theme(strip.text = element_text(
    size = 14)) +
  scale_fill_manual(values = c("no coating" = "aquamarine4", 
                               "chitosan" = "olivedrab3", 
                               "free" = "coral1"))+
  scale_x_discrete(breaks=c(0,3,7,11,14,24,42))
