library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)

#path to data
data_location <- here::here("data","decap6_8.xlsx")

#load data. 
rawdata <- read_excel(data_location)

data<- rawdata %>% slice(2:33)
#rename
colnames(data)[1] = "well"
colnames(data)[2] = "OD600"

data<- na.omit(data)

#insert plate layout data
#use plate layout to insert treatment names
#path to data
data_location4 <- here::here("data","plate_decap_6_7.xlsx")

#load data. 
plate <- read_excel(data_location4)

plate <- plate %>% pivot_longer(cols = !row, 
                                names_to = "column_num", 
                                values_to = "sample")
plate$well <- paste(plate$row, plate$column_num, sep="") 
plate <- plate %>% select(3:4)

#join names to sample data file
join2 <- left_join(data, plate, by = "well") %>% filter(sample != "well_blank")

df<- join2
#ID sample based on strain
df <- df %>% mutate(strain = case_when(startsWith(df$sample, "r") ~ "rep2", 
                                       startsWith(df$sample, "f") ~ "f199",
                                       startsWith(df$sample, "g") ~ "g7",
                                       startsWith(df$sample, "b") ~ "control"))

#ID sample based on time
df <- df %>% mutate(time = case_when(startsWith("b_0", df$sample) ~ "0", 
                                     startsWith("b_4", df$sample) ~ "4", 
                                     startsWith("b_6", df$sample) ~ "6", 
                                     startsWith("b_10", df$sample) ~ "10", 
                                     startsWith("b_14", df$sample) ~ "14", 
                                     startsWith("b_18", df$sample) ~ "18", 
                                     startsWith("b_21", df$sample) ~ "21" ,
                                     startsWith("b_24", df$sample) ~ "24", 
                                     startsWith("f_0", df$sample) ~ "0", 
                                     startsWith("f_4", df$sample) ~ "4", 
                                     startsWith("f_6", df$sample) ~ "6", 
                                     startsWith("f_10", df$sample) ~ "10", 
                                     startsWith("f_14", df$sample) ~ "14", 
                                     startsWith("f_18", df$sample) ~ "18", 
                                     startsWith("f_21", df$sample) ~ "21" ,
                                     startsWith("f_24", df$sample) ~ "24", 
                                     startsWith("r_0", df$sample) ~ "0", 
                                     startsWith("r_4", df$sample) ~ "4", 
                                     startsWith("r_6", df$sample) ~ "6", 
                                     startsWith("r_10", df$sample) ~ "10", 
                                     startsWith("r_14", df$sample) ~ "14", 
                                     startsWith("r_18", df$sample) ~ "18", 
                                     startsWith("r_21", df$sample) ~ "21" ,
                                     startsWith("r_24", df$sample) ~ "24", 
                                     startsWith("g_0", df$sample) ~ "0",
                                     startsWith("g_4", df$sample) ~ "4", 
                                     startsWith("g_6", df$sample) ~ "6", 
                                     startsWith("g_10", df$sample) ~ "10", 
                                     startsWith("g_14", df$sample) ~ "14", 
                                     startsWith("g_18", df$sample) ~ "18", 
                                     startsWith("g_21", df$sample) ~ "21" ,
                                     startsWith("g_24", df$sample) ~ "24")) 
glimpse(df)
df$OD600<- as.numeric(df$OD600)
df$time<- as.numeric(df$time)
glimpse(df)

control<- subset(df, strain == "control")  
test<- subset(df, strain != "control")

#take averages of blanks
control<- control %>% summarize(OD600 = mean(OD600))
#colnames(control)<- c("wellcontrol_sample", "type", "OD600_b")

corrected<- test
corrected$corrected <- corrected$OD600 - 0.1760888 #blank subtraction
#merge<- merge(test, control, by = c("type"))

corrected[corrected < 0 ] <- 0 #convert negative values to zero #not needed here

#capitalize
corrected<- corrected %>% mutate(Strain = fct_recode(strain, 
                                                     "F199" = "f199", 
                                                     "G7" = "g7", 
                                                     "Rep2" = "rep2"))
glimpse(corrected)

ggplot(data = corrected, aes(x = time, y = corrected, color = Strain)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("OD600 of Decapsulated Solutions")
ggsave("od600_decapsulatedsol.png", width = 5, height = 4)







