library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(ggplot2)
#path to data
data_location1 <- here::here("data","LowFluoCurveF199_20231018.xlsx")
#data_location2 <- here::here("data","LowFluoCurveF199_PlateCounts_20231018.xlsx")
data_location3 <- here::here("data", "plate_LowFluoCurveF199_20231018.xlsx")
#load data. 
rfu <- read_excel(data_location1)
#plate_count <- read_excel(data_location2)
plate<- read_excel(data_location3)
#take a look at the data
glimpse(rfu)

#remove extra measurement data
rfu<- na.omit(rfu) %>% select(!2)
glimpse(rfu)
#rename
colnames(rfu)[1] = "well"

glimpse(plate)
plate[] <- lapply(plate, as.character)
plate <- plate %>% pivot_longer(cols = !row, 
                                names_to = "column_num", 
                                values_to = "sample")
plate$well <- paste(plate$row, plate$column_num, sep="") 
plate <- plate %>% select(3:4)

glimpse(plate)

#join names to sample data file
join <- left_join(rfu, plate, by = "well")

long <- join %>% gather(meas, value, GFP:mScarlet, factor_key=TRUE)
glimpse(long)

#remove positive control to get better visualization
select<- long%>% filter(sample != "10")

#plot results
ggplot(data = select, aes(x = sample, y = value, color = meas)) +
  geom_point(stat='identity') +
  geom_smooth(se = FALSE)+
  theme_classic() + 
  xlab("Dilution (fraction of OD 0.1)") + 
  ylab("RFU Value")+ 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("F199 Standard Curve: OD Dilutions vs RFU Value")+
  scale_color_manual(values = c("mScarlet" = "indianred3", 
                                  "GFP" = "palegreen"))

#better calculations for OD
calc <- long %>% filter(sample != "pbs") %>% filter(sample != "srb15")
glimpse(calc)
calc$sample <- as.numeric(calc$sample)
calc <- calc %>% mutate(OD = 0.1*sample)  
glimpse(calc)

#remove positive control to get better visualization
calc<- calc%>% filter(sample != "10")

#NEED TO FIGURE OUT HOW TO ADD LINE OF BEST FIT EQUATION AND R SQUARED VALUE FOR IT

#plot results
ggplot(data = calc, aes(x = OD, y = value, color = meas)) +
  geom_point(stat='identity') +
  geom_smooth(method='lm', se=FALSE)+
  theme_classic() + 
  xlab("OD Value (Achieved by Dilution)") + 
  ylab("RFU Value")+ 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("F199 Standard Curve: OD Dilutions vs RFU Value")+
  scale_color_manual(values = c("mScarlet" = "indianred3", 
                                "GFP" = "palegreen"))
