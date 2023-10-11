library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)

#path to data
data_location3 <- here::here("data","BeadSpec_5_11_23.xlsx")

#load data. 
rawdata <- read_excel(data_location3)

#take a look at the data
glimpse(rawdata)
data<- rawdata
#rename
colnames(data)[1] = "well"
long <- data %>% pivot_longer(cols = !c(well, sample, time), 
                              names_to = "wavelength_nm", 
                              values_to = "absorbance")
#add IDs
long <- long %>% mutate(strain = case_when(startsWith(long$sample, "B") ~ "f199", 
                                       startsWith(long$sample, "C") ~ "control", 
                                       startsWith(long$sample, "F") ~ "f199"))

long <- long %>% mutate(type = case_when(grepl("BA", long$sample) ~ "bead", 
                                          grepl("BB", long$sample) ~ "bead", 
                                          grepl("CA", long$sample) ~ "free", 
                                          grepl("CB", long$sample) ~ "free", 
                                          grepl("CBB", long$sample) ~ "bead", 
                                          grepl("CBA", long$sample) ~ "bead", 
                                          grepl("F", long$sample) ~ "free"))

#remove nm
long$wavelength_nm <- gsub('nm', '', long$wavelength_nm)
plate2 <- long
plate2$wavelength_nm <- as.numeric(long$wavelength_nm)
plate2$absorbance <- as.numeric(plate2$absorbance)
glimpse(plate2)
#change time to numeric instead of character
plate2$time <- as.numeric(plate2$time)

#capitalize
plate2<- plate2 %>% mutate(Type = fct_recode(type, 
                                                    "Bead" = "bead", 
                                                    "Free" = "free")) %>%
  mutate(Sample = fct_recode(sample, 
                             "Bead" = "BA", 
                             "Bead" = "BB",
                             "Free" = "FA", 
                             "Free" = "FB",
                             "Bead Control" = "CBA", 
                             "Bead Control" = "CBB", 
                             "Free Control" = "CA", 
                             "Free Control" = "CB")) %>%
  mutate(Sample = fct_relevel(Sample, 
                               "Bead", "Free", "Bead Control", "Free Control"))


ggplot(data = plate2, aes(x = wavelength_nm, y = absorbance, color = Sample)) +
  geom_point() + theme_classic() + xlab("Wavelength (nm)") + ylab("Absorbance") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Absorbance Spectra") +
  facet_wrap(~Type)

beadfree<- subset(plate2, type != "NA")
ggplot(data = beadfree, aes(x = wavelength_nm, y = absorbance, color = Sample)) +
  geom_point() + theme_classic() + xlab("Wavelength (nm)") + ylab("Absorbance") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Absorbance Spectra") +
  facet_wrap(~Type)
ggsave("absorbance_spectra2.png", width = 8, height = 5)

bead<- subset(plate2, type == "bead")
ggplot(data = bead, aes(x = wavelength_nm, y = absorbance, color = sample)) +
  geom_point() + theme_classic() + xlab("Wavelength (nm)") + ylab("Absorbance") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Absorbance Spectra - Beads")


#blank subtractions
#take average of blanks at each time point

plate600<- subset(plate2, wavelength_nm == "600")
blank<- subset(plate2, strain == "control")
blank600<- subset(plate600, strain == "control")
test600<- subset(plate600, strain != "control")

#group by time, type, and average the absorbance
blank600<- blank600 %>% group_by(time, type, strain) %>% summarize(OD600 = mean(absorbance))

#remerge test and control
merge<- merge(test600, blank600, by = c("time", "type"))

#blank subtract
merge$corrected<- merge$absorbance - merge$OD600 #blank subtraction
merge[merge < 0 ] <- 0 #convert negative values to zero


glimpse(merge)
ggplot(data = merge, aes(x = time, y = corrected, color = sample)) +
  geom_point() + geom_smooth() + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Decapsulated Beads - Absorbance Over Time")


