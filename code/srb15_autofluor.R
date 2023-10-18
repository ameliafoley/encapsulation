library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(ggplot2)
#path to data
data_location1 <- here::here("data","srb15CheckG7_20231013.xlsx")
data_location2 <- here::here("data","srb15CheckF199_20231013.xlsx")
#load data. 
g7 <- read_excel(data_location1)
f199 <- read_excel(data_location2)
#take a look at the data
glimpse(g7)

#remove extra measurement data
g7<- na.omit(g7)
glimpse(g7)
f199<- na.omit(f199) %>% select(!2)
glimpse(f199)

#rename
colnames(g7)[1] = "well"
colnames(f199)[1] = "well"

#use plate layout to insert treatment names
#path to data
data_location3 <- here::here("data","plate_sRB15_20231013.xlsx")

#load data. 
plate <- read_excel(data_location3)

plate <- plate %>% pivot_longer(cols = !row, 
                                names_to = "column_num", 
                                values_to = "sample")
plate$well <- paste(plate$row, plate$column_num, sep="") 
plate <- plate %>% select(3:4)

#join names to sample data file
g7_join <- left_join(g7, plate, by = "well")
f199_join <- left_join(f199, plate, by = "well") 

g7_long <- g7_join %>% gather(meas, value, mScarlet:mVenus, factor_key=TRUE)
f199_long <- f199_join %>% gather(meas, value, GFP:mScarlet, factor_key=TRUE)

#plot results
ggplot(data = g7_long, aes(x = sample, y = value, fill = meas)) +
  geom_bar(stat='identity') + 
  theme_classic() + 
  xlab("Sample") + 
  ylab("RFU Value")+ 
  facet_wrap(~meas)+
 theme(axis.text.x=element_text(angle = -45, hjust = 0))+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Autofluorescence of sRB15 Pyruvate (G7 Settings)")+
  scale_fill_manual(values = c("mScarlet" = "indianred3", 
                               "mVenus" = "gold1"))+
  geom_text(aes(label = after_stat(sprintf("%.1f", y))), size = 3, stat = "summary", fun = "mean", vjust = -.1)

  
#plot same for f199 settings
ggplot(data = f199_long, aes(x = sample, y = value, fill = meas)) +
  geom_bar(stat='identity') + 
  theme_classic() + 
  xlab("Sample") + 
  ylab("RFU Value")+ 
  facet_wrap(~meas)+
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Autofluorescence of sRB15 Pyruvate (F199 Settings)")  +
  scale_fill_manual(values = c("mScarlet" = "indianred3", 
                               "GFP" = "palegreen"))+
  geom_text(aes(label = after_stat(sprintf("%.1f", y))), size = 3, stat = "summary", fun = "mean", vjust = -.1)

