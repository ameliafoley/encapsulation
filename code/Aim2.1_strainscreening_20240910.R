library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
#path to data
strains <- here::here("data","aim2_strains_GC_09102024.xlsx")

#load data. 
strains <- read_excel(strains)

#take a look at the data
glimpse(strains)

data<- strains %>% slice(2:97)
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
data_location4 <- here::here("data","aim2_strainscreen_platelayout.xlsx")

#load data. 
plate <- read_excel(data_location4)
plate$well <- paste(plate$row, plate$column, sep="") 
plate <- plate %>% select(3:5)

#join names to sample data file
join2 <- left_join(plate2, plate, by = "well")
df<-join2


control<- subset(df, strain == "blank")  
test<- subset(df, strain != "blank")


#plot first without blank correction
ggplot(data = test, aes(x = time_h, y = OD600, color = strain)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Rep2, F199, and G7 in 48-Well Plate")+
  facet_wrap(~media)

#cut to 90 hours
test<- test %>% filter(time_h<90)
ggplot(data = test, aes(x = time_h, y = OD600, color = strain)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Rep2, F199, and G7 in 48-Well Plate")+
  facet_wrap(~media)



#take averages of blanks
control<- control %>% group_by(media) %>% summarize(OD600 = mean(OD600))
colnames(control)<- c("media", "OD600_b")
merge<- merge(test, control, by = c("media"))

merge$corrected<- merge$OD600 - merge$OD600_b #blank subtraction
merge[merge < 0 ] <- 0 #convert negative values to zero

ggplot(data = merge, aes(x = time_h, y = corrected, color = strain)) +
  geom_point() + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("PAH-degrading Strain Screening")+
  facet_wrap(~media, ncol = 3)

incomplete <- merge %>% filter(strain!= "p.putida") %>% filter(strain!= "n.aroma") %>% filter(strain!= "n.penta")
ggplot(data = incomplete, aes(x = time_h, y = corrected, color = strain)) +
  geom_point() + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("PAH-degrading Strain Screening")+
  facet_wrap(~media, ncol = 3)
