library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)

#path to data
data_location <- here::here("data","Ecoli_leak_7_12.xlsx")

#load data. 
rawdata <- read_excel(data_location)

data<- rawdata %>% slice(11:17) %>% select(1:10)


#rename
colnames(data)[1] = "row"

long <- data %>% pivot_longer(cols = !row, 
                              names_to = "column", 
                              values_to = "OD600") %>% slice(10:63)
long$well <- paste(long$row, long$column, sep="") 
long <- long %>% select(3:4)


#insert plate layout data
#use plate layout to insert treatment names
#path to data
data_location4 <- here::here("data","Ecoli_plate.xlsx")

#load data. 
plate <- read_excel(data_location4)

plate <- plate %>% pivot_longer(cols = !row, 
                                names_to = "column_num", 
                                values_to = "sample")
plate$well <- paste(plate$row, plate$column_num, sep="") 
plate <- plate %>% select(3:4)

#join names to sample data file
join <- left_join(long, plate, by = "well") 


#ID based on type
df <- join %>% mutate(cell_load = case_when(startsWith(join$sample, "blank") ~ "blank", 
                                            startsWith(join$sample, "low") ~ "low", 
                                            startsWith(join$sample, "med") ~ "med", 
                                            startsWith(join$sample, "high") ~ "high")) 
#ID based on alginate
df <- df %>% mutate(alginate = case_when(grepl("two", df$sample) ~ "two", 
                                         grepl("one", df$sample) ~ "one"))


#ID sample based on time
df <- df %>% mutate(time = case_when(grepl("_0", df$sample) ~ "0", 
                                     grepl("_2_", df$sample) ~ "2", 
                                     grepl("_4", df$sample) ~ "4", 
                                     grepl("_6", df$sample) ~ "6", 
                                     grepl("_10", df$sample) ~ "10", 
                                     grepl("_14", df$sample) ~ "14", 
                                     grepl("_18", df$sample) ~ "18", 
                                     grepl("_21", df$sample) ~ "21", 
                                     grepl("_24", df$sample) ~ "24"))


glimpse(df)
df$OD600<- as.numeric(df$OD600)
df$time<- as.numeric(df$time)
glimpse(df)



ggplot(data = df, aes(x = time, y = OD600, color = cell_load, shape = cell_load)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("E. coli Leakage")+ 
  facet_wrap(~alginate)
ggsave("ecoli_leakage.png", width = 5, height = 4)








