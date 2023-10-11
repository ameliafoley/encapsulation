---
  title: "growth curves may 2023"
output: html_document
---
  #Loading data
  ```{r}
#load  packages
library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)

#path to data
data_location1 <- here::here("data","growthcurve5_19.xlsx")

#load data. 
rawdata <- read_excel(data_location1)

#take a look at the data
glimpse(rawdata)

```

#Blank subtractions
```{r}
#all values are adjusted for LB broth. So, free cell absorbance stays the same. I need to subtract the absorbance of the blank beads from the absorbance of the encapsulated microbes to account for just the bacterial cells

#Let's take an average of all the blank beads values, and subtract this from our enc. bead values. 
mean(rawdata$blank_beads)

#mean value of blank beads is 0.114

data <- rawdata %>% mutate(f199_beads = f199_beads - 0.114, 
                           rep2_beads = rep2_beads - 0.114) #blank subtractions
data[data < 0 ] <- 0 #convert negative values to zero

long_data <- data %>% pivot_longer(cols=c("blank", 
                                          "blank_beads", 
                                          "f199_beads", 
                                          "f199_free", 
                                          "rep2_beads", 
                                          "rep2_free"), 
                                   names_to = "group", 
                                   values_to = "OD600")
```

#Plot growth curve
```{r}

#F199 growth curve
long_data %>% filter(group == c("blank", "blank_beads", "f199_beads", "f199_free")) %>% 
  ggplot(aes(time_hr, OD600, color = group)) + geom_point() + geom_smooth() + theme_classic() + labs(title = "F199 Growth Curve") + xlab("Time (hr)") + ylab("OD600 (Corrected)") + theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))

#Rep2 growth curve
long_data %>% filter(group == c("blank", "blank_beads", "rep2_beads", "rep2_free")) %>% 
  ggplot(aes(time_hr, OD600, color = group)) + geom_point() + geom_smooth() + theme_classic() + labs(title = "Rep2 Growth Curve") + xlab("Time (hr)") + ylab("OD600 (Corrected)") + theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))
```

It looks like a growth curve! Looks like Rep2 (and F199) need long incubation times, which makes sense since they are "slow-growers"

#Look at plate reader data - WRANGLE
```{r}
#path to data
data_location2 <- here::here("data","growthcurve5_17_18_table.xlsx")

#load data. 
rawdata_plate1 <- read_excel(data_location2)

#take a look at the data
glimpse(rawdata_plate1)

#clean - remove columns 2 and 3, rename
data_plate1 <- rawdata_plate1 %>% select(!2) %>% select(!2) %>% select(!2) %>% slice(2:49)


#rename
colnames(data_plate1)[1] = "well"
colnames(data_plate1)[2] = "0s"

#pivot
long_plate1 <- data_plate1 %>% pivot_longer(cols = !well, 
                                            names_to = "time_s", 
                                            values_to = "OD600")

#remove s
long_plate1$time_s <- gsub('s', '', long_plate1$time_s)
plate1 <- long_plate1
plate1$time_s <- as.numeric(long_plate1$time_s)
plate1$OD600 <- as.numeric(plate1$OD600)
glimpse(plate1)
#convert time from seconds to hours
plate1 <- plate1 %>% mutate(time_h = time_s/60/60) %>% select(!time_s)
glimpse(plate1)

#use plate layout to insert treatment names
#path to data
data_location3 <- here::here("data","plate_5_17_18.xlsx")

#load data. 
plate <- read_excel(data_location3)

plate <- plate %>% pivot_longer(cols = !row, 
                                names_to = "column_num", 
                                values_to = "sample")
plate$well <- paste(plate$row, plate$column_num, sep="") 
plate <- plate %>% select(3:4)

#join names to sample data file
join <- left_join(plate1, plate, by = "well")

df_type <- join %>% mutate(type = case_when(startsWith(join$sample, "f") ~ "free", 
                                            startsWith(join$sample, "b") ~ "bead", 
                                            startsWith(join$sample, "LB") ~ "free")) 

write_csv(df_type, "df_type.csv")                                         
```


#Plate reader data - VISUALIZE
```{r}
join %>% filter(sample == c("bb_f_22","LB_2","ff_22_a","ff_22_b","ff_22_c","bf_22_a","bf_22_b","bf_22_c")) %>% 
  ggplot(aes(time_h, OD600, color = sample)) + geom_point() + labs(title = "F199 Growth Curve 0.2g")
```
#BLANK SUBTRACTIONS   
```{r}
OD_sub <- df_type %>% group_by(time_h) %>% mutate(OD600 = case_when(type == "bead" ~ OD600 - OD600[sample = "bb_f_22"], 
                                                                    type == "free" ~ OD600 - OD600[sample = "LB_2"])) 

%>%
  filter(sample != "bb_f_22")
```

