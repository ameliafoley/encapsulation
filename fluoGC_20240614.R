library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)

#load data. 
rawdata <- read_excel(here::here("data","fluoGC_20240614.xlsx"))

#take a look at the data
glimpse(rawdata)
#remove empty rows
data<- rawdata[1:56,]
colnames(data) <- data[1,]
clean_data <- data[-c(1:2),]
#rename
colnames(clean_data)[1] = "well"
mScarlet<- clean_data %>% select(1:81)%>% pivot_longer(cols = !well, 
                                                       names_to = "time_s", 
                                                       values_to = "mScarlet")
mVenus<- clean_data %>% select(1, 82:161)%>% pivot_longer(cols = !well, 
                                                          names_to = "time_s", 
                                                          values_to = "mVenus")
OD600<- clean_data %>% select(1, 162: 241)%>% pivot_longer(cols = !well, 
                                                           names_to = "time_s", 
                                                           values_to = "OD600")
GFP<- clean_data %>% select(1, 242:321)%>% pivot_longer(cols = !well, 
                                                        names_to = "time_s", 
                                                        values_to = "GFP")
glimpse(mScarlet)
#remove s
mScarlet$time_s <- gsub('s', '', mScarlet$time_s) %>% as.numeric()
mVenus$time_s <- gsub('s', '', mVenus$time_s) %>% as.numeric()
OD600$time_s <- gsub('s', '', OD600$time_s) %>% as.numeric()
GFP$time_s <- gsub('s', '', GFP$time_s) %>% as.numeric()

mScarlet$mScarlet <- as.numeric(mScarlet$mScarlet)
mVenus$mVenus <- as.numeric(mVenus$mVenus)
OD600$OD600 <- as.numeric(OD600$OD600)
GFP$GFP <- as.numeric(GFP$GFP)

#convert time from seconds to hours
mScarlet <- mScarlet %>% mutate(time_h = time_s/60/60) %>% select(!time_s)
mVenus <- mVenus  %>% mutate(time_h = time_s/60/60) %>% select(!time_s)
OD600 <- OD600 %>% mutate(time_h = time_s/60/60) %>% select(!time_s)
GFP <- GFP  %>% mutate(time_h = time_s/60/60) %>% select(!time_s)

#plate layout and code for sample info
plate_layout <- read_excel(here::here("data", "platelayout_fluoGC_20240614.xlsx"))
#clean and wrangle plate data

plate_layout[] <- lapply(plate_layout, as.character)
plate_layout <- plate_layout %>% pivot_longer(cols = !row, 
                                      names_to = "column_num", 
                                      values_to = "sample")
plate_layout$well <- paste(plate_layout$row, plate_layout$column_num, sep="") 
plate_layout <- plate_layout %>% select(3:4)

#join sample names
join_mScarlet <- left_join(mScarlet, plate_layout, by = "well")
join_mVenus <- left_join(mVenus, plate_layout, by = "well")
join_OD600 <- left_join(OD600, plate_layout, by = "well")
join_GFP <- left_join(GFP, plate_layout, by = "well")

plate_code<- read_excel(here::here("data", "platecode_fluoGC_20240614.xlsx"))
join_mScarlet<- join_mScarlet %>% left_join(plate_code, by = "sample")
join_mVenus <- join_mVenus %>% left_join(plate_code, by = "sample")
join_OD600 <- join_OD600 %>% left_join(plate_code, by = "sample")
join_GFP <- join_GFP %>% left_join(plate_code, by = "sample")

mScarlet_control<- join_mScarlet %>% subset(strain == "blank")
mScarlet_control_code<- mScarlet_control %>% group_by(type, media) %>% summarize(control = mean(mScarlet))
mVenus_control<- join_mVenus %>% subset(strain == "blank") 
mVenus_control_code<- mVenus_control %>% group_by(type, media) %>% summarize(control = mean(mVenus))
GFP_control<- join_GFP %>% subset(strain == "blank") 
GFP_control_code<- GFP_control %>% group_by(type, media) %>% summarize(control = mean(GFP))
OD600_control<- join_OD600 %>% subset(strain == "blank") 
OD600_control_code<- OD600_control %>% group_by(type, media) %>% summarize(control = mean(OD600))

join_mScarlet<- join_mScarlet %>% left_join(mScarlet_control_code)
join_mVenus<- join_mVenus %>% left_join(mVenus_control_code)
join_GFP<- join_GFP %>% left_join(GFP_control_code)
join_OD600<- join_OD600 %>% left_join(OD600_control_code)

join_mScarlet<- join_mScarlet %>%
  mutate(mScarlet_cor = mScarlet-control)
join_mVenus<- join_mVenus %>%
  mutate(mVenus_cor = mVenus-control)
join_GFP<- join_GFP %>%
  mutate(GFP_cor = GFP-control)
join_OD600<- join_OD600 %>%
  mutate(OD600_cor = OD600-control)

write_xlsx(join_mScarlet, here::here("data", "fluoGC_mScarlet.xlsx"))
write_xlsx(join_mVenus, here::here("data","fluoGC_mVenus.xlsx"))
write_xlsx(join_GFP, here::here("data","fluoGC_GFP.xlsx"))
write_xlsx(join_OD600, here::here("data","fluoGC_OD600.xlsx"))

#now each dataset has sample info, we can begin trying to graph

ggplot(data = join_OD600, aes(x = time_h, y = OD600_cor, color = strain, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + facet_wrap(~strain + media)

ggplot(data = join_mScarlet, aes(x = time_h, y = mScarlet, color = strain, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + facet_wrap(~strain + media)
ggplot(data = join_mVenus, aes(x = time_h, y = mVenus, color = strain, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + facet_wrap(~strain + media)
ggplot(data = join_GFP, aes(x = time_h, y = GFP, color = strain, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + facet_wrap(~strain + media)
