library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)

#path to data
data_location3 <- here::here("data","envcond_8_16.xlsx")

#load data. 
rawdata <- read_excel(data_location3)

#take a look at the data
glimpse(rawdata)
rawdata$value <- as.numeric(rawdata$value)

sample <- rawdata %>% filter(sample == "1")
control <- rawdata %>% filter(sample == "0")

fluor <- sample %>% filter(fluor == "1")
abs <- rawdata %>% filter(meas == "absorbance")

fluor_raw <- rawdata %>% filter(fluor == "1")
fluor_samp <- fluor_raw %>% filter(sample == "1")
                            

ggplot(data = rawdata, aes(x = day, y = value, color = meas, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Day") + ylab("Value") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Env Cond Exp")

ggplot(data = fluor, aes(x = day, y = value, color = meas, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Day") + ylab("Value") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Env Cond Exp") + facet_wrap(~meas) + facet_wrap(~type)


  #+facet_wrap(~)

plate <- sample %>% filter(grepl("dil", meas)) %>% select(!c("fluor", "sample"))
plate2 <- plate %>% remove_missing()

#G7 leaked from capsules measured by CFU/mL
ggplot(data = plate2, aes(x = day, y = cfu_ml)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Day") + ylab("CFU/mL") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("CFU/mL Leaked from Capsules")

#G7 leaked from capsules measured by fluorescence
leak <- fluor %>% filter(type=="water")
ggplot(data = leak, aes(x = day, y = value, color = meas)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Day") + ylab("RFU") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Fluorescence of Water from Test Tubes Containing Beads")

#Not sure what I'm seeing here: based on CFU counts, there is increased leakage over time, but this is not neccesarily reflected in the fluorescence
#that I can tell right now. 

decap <- fluor %>% filter(type=="decap")
ggplot(data = decap, aes(x = day, y = value, color = meas)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Day") + ylab("RFU") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Fluorescence of Decapsulated Solution")

ggplot(data = abs, aes(x = day, y = value, color = meas)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Day") + ylab("OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Absorbance of Solutions") + facet_wrap(~type)

con_fluor <- control %>% filter(fluor == "1")
con_abs <- control %>% filter(fluor == "0")

ggplot(data = con_fluor, aes(x = day, y = value, color = meas)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Day") + ylab("OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Fluorescence Controls") + facet_wrap(~type)

ggplot(data = con_abs, aes(x = day, y = value, color = meas)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Day") + ylab("OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Absorbance Controls") + facet_wrap(~type)

ggplot(data = fluor_raw, aes(x = day, y = value, color = meas)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Day") + ylab("OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Fluorescence - All") + facet_wrap(~type)

fluor_decap <- fluor_raw %>% filter(type == c("pbs", "decap"))
fluor_water <- fluor_raw %>% filter(type == c("io", "water"))

ggplot(data = fluor_decap, aes(x = day, y = value, color = meas)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Day") + ylab("RFU") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Decapsulation") + facet_wrap(~type)

ggplot(data = fluor_water, aes(x = day, y = value, color = meas)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Day") + ylab("RFU") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Exterior Solution") + facet_wrap(~type)

#As of Day 21, looks like bacteria levels are dropping (fluorescence readings are decreasing and so are CFU/mL counts)

