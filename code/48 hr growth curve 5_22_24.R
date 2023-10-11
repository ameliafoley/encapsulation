library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)

#path to data
data_location1 <- here::here("data","growthcurve5_22_23_table.xlsx")
data_location2 <- here::here("data","growthcurve5_23_24_table.xlsx")

#load data. 
night1 <- read_excel(data_location1)
night2<- read_excel(data_location2)

#take a look at the data
glimpse(night1)
glimpse(night2)

night1<- night1 %>% slice(2:37)
night2<- night2 %>% slice(2:37)

#rename
colnames(night1)[1] = "well"
colnames(night2)[1] = "well"

#pivot
night1 <- night1 %>% pivot_longer(cols = !well, 
                              names_to = "time_s", 
                              values_to = "OD600")
night2 <- night2 %>% pivot_longer(cols = !well, 
                                  names_to = "time_s", 
                                  values_to = "OD600")

#remove s
night1$time_s <- gsub('s', '', night1$time_s)

night1$time_s <- as.numeric(night1$time_s)
night1$OD600 <- as.numeric(night1$OD600)
glimpse(night1)

#remove s
night2$time_s <- gsub('s', '', night2$time_s)
night2$time_s <- as.numeric(night2$time_s)
night2$OD600 <- as.numeric(night2$OD600)
glimpse(night2)


#convert time from seconds to hours
night1 <- night1 %>% mutate(time_h = time_s/60/60) %>% select(!time_s)
glimpse(night1)
complete.cases(night1)
night1<- na.omit(night1)

#convert time from seconds to hours
night2 <- night2 %>% mutate(time_h = time_s/60/60) %>% select(!time_s)
glimpse(night2)
complete.cases(night2)
night2<- na.omit(night2)

#adjust night 2 times
night2 <- night2 %>% mutate(time_h = time_h + 20)

join<- full_join(night1, night2)

#use plate layout to insert treatment names
#path to data
data_location4 <- here::here("data","plate_5_22_24.xlsx")

#load data. 
plate <- read_excel(data_location4)

plate <- plate %>% pivot_longer(cols = !row, 
                                names_to = "column_num", 
                                values_to = "sample")
plate$well <- paste(plate$row, plate$column_num, sep="") 
plate <- plate %>% select(3:4)

#join names to sample data file
join2 <- left_join(join, plate, by = "well") %>% filter(sample != "well_blank")

#ID based on type
df <- join2 %>% mutate(type = case_when(startsWith(join2$sample, "b") ~ "bead", 
                                        startsWith(join2$sample, "f") ~ "free", 
                                        startsWith(join2$sample, "lb") ~ "free")) 

#ID sample based on strain
df <- df %>% mutate(strain = case_when(startsWith(df$sample, "b_rep") ~ "rep2", 
                                       startsWith(df$sample, "f_rep") ~ "rep2", 
                                       startsWith(df$sample, "lb") ~ "control", 
                                       startsWith(df$sample, "bb") ~ "control", 
                                       startsWith(df$sample, "b_f199") ~ "f199", 
                                       startsWith(df$sample, "f_f199") ~ "f199")) 
                

control<- subset(df, strain == "control")  
test<- subset(df, strain != "control")


#take averages of blanks
control<- control %>% group_by(time_h, sample, type, strain) %>% summarize(OD600 = mean(OD600))
colnames(control)<- c("time_h", "control_sample", "type", "strain", "OD600_b")
control$strain<- NULL
merge<- merge(test, control, by = c("time_h", "type"))

merge$corrected<- merge$OD600 - merge$OD600_b #blank subtraction
merge[merge < 0 ] <- 0 #convert negative values to zero


ggplot(data = merge, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("48 hr Growth of F199 and Rep2 - Beads and Free")

ggplot(data = merge, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("48 hr Growth of F199 and Rep2 - Beads and Free") + 
  facet_wrap(~ type+strain)

rep2<- subset(merge, strain == "rep2")
ggplot(data = rep2, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("48 hr Growth of F199 and Rep2 - Beads and Free")

#Out of each test group and six replicates each, only one well exhibited growth as expected. Potential issues here: 
#F199 beads were two days older than the Rep2 beads. They appear to being growing towards the 40 hour mark, but do not grow as much as Rep2
#I used old overnight cultures (from last week) to inoculate the "free" samples - I think this was my big mistake. All of the cells in those cultures would have died off by that point. IDIOT. 

#going back to look at control blank beads
ggplot(data = control, aes(x = time_h, y = OD600_b, color = control_sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Control OD600")

