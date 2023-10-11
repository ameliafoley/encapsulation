library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(ggpubr) #for p-value stats
#path to data
data_location <- here::here("data","cell_viability.xlsx")

#load data. 
rawdata <- read_excel(data_location)

#take a look at the data
glimpse(rawdata)

#name new dataset we will transform
data <- rawdata

#calculate colony forming units
data$cfu <- (data$platecount_a + data$platecount_b)/2 * data$best_dilution/.1

#PLOT
ggplot(data = data, aes(x = group, y = cfu, fill = group)) +
  geom_boxplot() + theme_classic() + xlab("Treatment Group (n=2)") + ylab("CFU/mL") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Cell Viability in Sodium Citrate via Plate Count")  + 
  scale_fill_manual(values=c("darkseagreen4", "lightsalmon")) +
  stat_compare_means()

#my sample size is super small, of course we are seeing high p-value. let's look at the RFU measurements next

#FLUORESCENCE MEASUREMENTS

data_location2 <- here::here("data","cell_viability10_4_fluor.xlsx")
data_location3 <- here::here("data", "cell_viability_plate.xlsx")

#load data. 
raw_fluor <- read_excel(data_location2)
plate <- read_excel(data_location3)

#take a look at the data
glimpse(raw_fluor)
glimpse(plate)

#clean fluor
fluor <- raw_fluor %>% select(!2)
colnames(fluor)[1] = "well"
fluor <- na.omit(fluor)

#add plate data
plate <- plate %>% pivot_longer(cols = !row, 
                                names_to = "column_num", 
                                values_to = "sample")
plate$well <- paste(plate$row, plate$column_num, sep="") 
plate <- plate %>% select(3:4)
plate <- na.omit(plate)



#join names to sample data file
join <- left_join(fluor, plate, by = "well") 

#ID based on rep
df <- join %>% mutate(rep = case_when(endsWith(join$sample, "a") ~ "a",
                                        endsWith(join$sample, "b") ~ "b"))

#ID sample based on exposure group
df <- df %>% mutate(group = case_when(startsWith(df$sample, "exposed") ~ "exposed", 
                                       startsWith(df$sample, "control") ~ "control"))
#lengthen
long <- df %>% gather(key="fluor", value="rfu", 2:3)

exp <- na.omit(long)
mVenus <- exp %>% filter(fluor=="mVenus")
mScarlet <- exp %>% filter(fluor=="mScarlet")

#plot mVenus
ggplot(data = mVenus, aes(x = group, y = rfu, fill = group)) +
  geom_boxplot() + theme_classic() + xlab("Treatment Group (n=4)") + ylab("RFU") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Cell Viability in Sodium Citrate via mVenus") + 
  scale_fill_manual(values=c("darkseagreen4", "lightsalmon"))+
  stat_compare_means()

#plot mScarlet
ggplot(data = mScarlet, aes(x = group, y = rfu, fill = group)) +
  geom_boxplot() + theme_classic() + xlab("Treatment Group (n=4)") + ylab("RFU") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Cell Viability in Sodium Citrate via mScarlet") + 
  scale_fill_manual(values=c("darkseagreen4", "lightsalmon"))+
  stat_compare_means()

  
