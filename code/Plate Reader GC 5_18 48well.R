library(tidyverse)
library(dplyr)

#path to data
data_location3 <- here::here("data","growthcurve5_18_19_table.xlsx")

#load data. 
rawdata <- read_excel(data_location3)

#take a look at the data
glimpse(rawdata)

data<- rawdata %>% slice(2:31)
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
complete.cases(plate2)
plate2<- na.omit(plate2)


#use plate layout to insert treatment names
#path to data
data_location4 <- here::here("data","plate_5_18_19.xlsx")

#load data. 
plate <- read_excel(data_location4)

plate <- plate %>% pivot_longer(cols = !row, 
                                names_to = "column_num", 
                                values_to = "sample")
plate$well <- paste(plate$row, plate$column_num, sep="") 
plate <- plate %>% select(3:4)

#join names to sample data file
join2 <- left_join(plate2, plate, by = "well") %>% filter(sample != "well_blank")

#ID based on type
df <- join2 %>% mutate(type = case_when(startsWith(join2$sample, "b") ~ "bead", 
                                            startsWith(join2$sample, "f") ~ "free", 
                                            startsWith(join2$sample, "lb") ~ "free")) 

#ID sample based on strain
df <- df %>% mutate(strain = case_when(startsWith(df$sample, "brep") ~ "rep2", 
                                                 startsWith(df$sample, "frep") ~ "rep2", 
                                                 startsWith(df$sample, "lb") ~ "control", 
                                       startsWith(df$sample, "bb") ~ "control"))
                              
#ID sample based on volume
df <- df %>% mutate(volume = case_when(grepl("400", df$sample) ~ "400", 
                                                 grepl("600", df$sample) ~ "600")) 
control<- subset(df, strain == "control")  
test<- subset(df, strain != "control")

#take averages of blanks
control<- control %>% group_by(time_h, sample, type, strain, volume) %>% summarize(OD600 = mean(OD600))
colnames(control)<- c("time_h", "control_sample", "type", "strain", "volume", "OD600_b")
control$strain<- NULL
merge<- merge(test, control, by = c("time_h", "type", "volume"))

merge$corrected<- merge$OD600 - merge$OD600_b #blank subtraction
merge[merge < 0 ] <- 0 #convert negative values to zero



ggplot(data = merge, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Rep2 in 48-Well Plate")

four<- subset(merge, volume == "400")
ggplot(data = four, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Rep2 in 48-Well Plate (400uL)")

six<- subset(merge, volume == "600")
ggplot(data = six, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Rep2 in 48-Well Plate (600uL)")

#control
ggplot(data = control, aes(x = time_h, y = OD600_b, color = control_sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Control OD600")
