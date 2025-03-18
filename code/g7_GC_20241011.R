library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(writexl)
#path to data
gc <- here::here("data","g7_GC_20241011.xlsx")

#load data. 
gc <- read_excel(gc)

#take a look at the data
glimpse(gc)

data<- gc %>% slice(8:43)
#rename
colnames(data)[1] = "well"


#pivot
long <- data %>% pivot_longer(cols = !well, 
                              names_to = "time_s", 
                              values_to = "OD600")

#remove s
long$time_s <- gsub('s', '', long$time_s)
plate2 <- long
plate2$time_s <- as.numeric(long$time_s)
plate2$OD600 <- as.numeric(plate2$OD600)
glimpse(plate2)
#convert time from seconds to hours
plate2 <- plate2 %>% mutate(time_h = time_s/60/60) %>% select(!time_s)
glimpse(plate2)



#use plate layout to insert treatment names
#path to data
data_location4 <- here::here("data","g7_GC_PL_20241011.xlsx")

#load data. 
plate <- read_excel(data_location4)
plate$well <- paste(plate$row, plate$column, sep="") 
plate <- plate %>% select(3:6)

#join names to sample data file
join2 <- left_join(plate2, plate, by = "well")
df<-join2


control<- subset(df, strain == "blank")  
test<- subset(df, strain != "blank")


#plot first without blank correction
ggplot(data = test, aes(x = time_h, y = OD600, color = treatment, shape = media)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("G7 in 48-Well Plate")+
  facet_wrap(~strain+media)

#take averages of blanks
#LB cap .6
#LB free .5
#srb cap .6
#srb free .5
control <- here::here("data","blank_values.xlsx")
control<- read_excel(control)
#control<- control %>% group_by(media, treatment) %>% summarize(OD600 = mean(OD600))
initial <- here::here("data", "g7_GC_initialvalues.xlsx")
initial<- read_excel(initial)

colnames(control)<- c("media","treatment", "OD600_b")
merge<- merge(test, initial, by = "well")

merge$corrected<- merge$OD600 - merge$initial #initial subtraction
merge[merge < 0 ] <- 0 #convert negative values to zero

ggplot(data = merge, aes(x = time_h, y = corrected, color = treatment)) +
  geom_point() + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("G7 Growth Curves")+
  facet_wrap(~strain + media, ncol = 2)

ggplot(data = merge, aes(x = time_h, y = corrected, color = treatment)) +
  stat_summary(geom="point", fun = mean) +
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("F199 Growth Curves")+
  facet_wrap(~strain + media, ncol = 2)


data_location2 <- here::here("data", "g7chit_20241011.xlsx")
write_xlsx(merge, data_location2)



