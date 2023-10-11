library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)

#path to data
data_location <- here::here("data","decaptwo_6_2.xlsx")

#load data. 
rawdata <- read_excel(data_location)
data<- na.omit(rawdata)

#rename
colnames(data)[1] = "well"
colnames(data)[2] = "OD600"

#insert plate layout data
#use plate layout to insert treatment names
#path to data
data_location4 <- here::here("data","plate_decaptwo_6_2.xlsx")

#load data. 
plate <- read_excel(data_location4)

plate <- plate %>% pivot_longer(cols = !row, 
                                names_to = "column_num", 
                                values_to = "sample")
plate$well <- paste(plate$row, plate$column_num, sep="") 
plate <- plate %>% select(3:4)

#join names to sample data file
join2 <- left_join(data, plate, by = "well") %>% filter(sample != "well_blank")


#ID sample based on strain
df <- join2 %>% mutate(strain = case_when(startsWith(df$sample, "b") ~ "control", 
                                       startsWith(df$sample, "f") ~ "f199", 
                                       startsWith(df$sample, "g") ~ "g7"))

df <- df %>% mutate(time = case_when(grepl("_0", df$sample) ~ "0", 
                                       grepl("5", df$sample) ~ "5", 
                                     grepl("10", df$sample) ~ "10", 
                                     grepl("14", df$sample) ~ "14", 
                                     grepl("18", df$sample) ~ "18", 
                                     grepl("24", df$sample) ~ "24")) 

glimpse(df)
df$OD600<- as.numeric(df$OD600)
df$time<- as.numeric(df$time)
glimpse(df)

#graphing with no correction, to visualize
ggplot(data = df, aes(x = time, y = OD600, color = strain)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("F199, G7, and Blank Beads w/ Decapsulation")


glimpse(df)
df$OD600<- as.numeric(df$OD600)
df$time<- as.numeric(df$time)
glimpse(df)

control<- subset(df, strain == "control")  
control <- control %>% slice(1:4) #removing outliers
test<- subset(df, strain != "control")

#take averages of blanks
control<- control %>% summarize(OD600 = mean(OD600))
colnames(control)<- c("control_sample", "type", "OD600_b")

merge<- merge(test, control)

merge$corrected<- merge$OD600 - merge$control_sample #blank subtraction
#merge[merge < 0 ] <- 0 #convert negative values to zero #not needed here

glimpse(merge)
ggplot(data = merge, aes(x = time, y = corrected, color = strain)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("F199 and G7 w/ Decapsulation")





