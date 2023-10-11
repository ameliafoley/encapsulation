library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)

#path to data
data_location3 <- here::here("data","Ecoli_wholesystem_7_17_table.xlsx")

#load data. 
rawdata <- read_excel(data_location3)

#take a look at the data
glimpse(rawdata)

data<- rawdata %>% slice(2:19)
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
data_location4 <- here::here("data","Ecoli_system_plate.xlsx")

#load data. 
plate <- read_excel(data_location4)

plate <- plate %>% pivot_longer(cols = !row, 
                                names_to = "column_num", 
                                values_to = "sample")
plate$well <- paste(plate$row, plate$column_num, sep="") 
plate <- plate %>% select(3:4)

#join names to sample data file
join2 <- left_join(plate2, plate, by = "well") 

#ID based on cell loading
df <- join2 %>% mutate(cell_load = case_when(startsWith(join2$sample, "blank") ~ "blank", 
                                            startsWith(join2$sample, "low") ~ "low", 
                                            startsWith(join2$sample, "med") ~ "med", 
                                            startsWith(join2$sample, "high") ~ "high")) 
#ID based on alginate
df <- df %>% mutate(alginate = case_when(grepl("2", df$sample) ~ "two", 
                                         grepl("1", df$sample) ~ "one", 
                                         grepl("free_k", df$sample) ~ "free", 
                                         grepl("lb", df$sample) ~ "free"))


#ID based on type
df <- df %>% mutate(type = case_when(startsWith(join2$sample, "free_k") ~ "free", 
                                        startsWith(join2$sample, "blank") ~ "bead",
                                        startsWith(join2$sample, "low") ~ "bead",
                                        startsWith(join2$sample, "med") ~ "bead",
                                        startsWith(join2$sample, "high") ~ "bead",
                                        startsWith(join2$sample, "lb") ~ "free")) 



ggplot(data = df, aes(x = time_h, y = OD600, color = cell_load, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("E. Coli Whole System")+
  facet_wrap(~alginate)
ggsave("ecoli_wholesystem.png", width = 5, height = 4)



