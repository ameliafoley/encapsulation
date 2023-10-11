
library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)

df_type<- read_csv("df_type.csv")


#ID sample based on strain
df_type <- df_type %>% mutate(strain = case_when(startsWith(df_type$sample, "ff") ~ "f199", 
                                            startsWith(df_type$sample, "fr") ~ "rep2", 
                                            startsWith(df_type$sample, "bf") ~ "f199", 
                                            startsWith(df_type$sample, "br") ~ "rep2", 
                                            startsWith(df_type$sample, "bb") ~ "control", 
                                            startsWith(df_type$sample, "LB") ~ "control")) 
#ID sample based on volume
df_type <- df_type %>% mutate(volume = case_when(grepl("22", df_type$sample) ~ "200", 
                                              grepl("21", df_type$sample) ~ "100", 
                                              grepl("11", df_type$sample) ~ "100", 
                                              grepl("LB_1", df_type$sample) ~ "100", 
                                              grepl("LB_2", df_type$sample) ~ "200"))

df_type$control<- ifelse(df_type$strain == "control", 1, 0) #dummy variable
#df_type$control<- ifelse(df_type$sample == "LB_2", 1, df_type$control) #dummy variable

control<- subset(df_type, control == 1) #subset controls
control$control<- NULL #remove column
test<- filter(df_type, control == 0) #subset tests
test$control<- NULL #remove column
control$strain<- NULL #remove column

#average the two controls to get one value
avgcon <- subset(df_type, strain == "control")
avgcon$control<- NULL

avgcon <- avgcon %>% mutate(beadcon = case_when(grepl("f", avgcon$sample) ~ "a", 
                                                 grepl("r", avgcon$sample) ~ "b"))

#cona <- subset(avgcon, sample == c("bb_f_22", "bb_f_21", "bb_f_11"))
cona <- subset(avgcon, beadcon == "a")
cona$beadcon<- NULL
conb <- subset(avgcon, beadcon == "b")
conb$beadcon<- NULL
#conb <- subset(avgcon, sample == c("bb_r_22", "bb_r_21", "bb_r_11"))
colnames(conb)<- c("well_b", "OD600_b", "time_h", "sample_b", "type", "strain", "volume")
con<- merge(cona, conb, by = c("time_h", "type", "volume", "strain"))
con <- con %>% mutate(mean = rowMeans(select(con, c(OD600, OD600_b)), na.rm = TRUE))
af<- group_by(con, time_h, well) %>% summarize(dupmean = mean(mean)) #take mean of duplicate wells
af<- left_join(af, con, by = c("well", "time_h")) #add means for duplicate wells back to df
af<- af %>% distinct(sample, time_h, .keep_all = TRUE) #remove duplicates, keep mean
af<- af %>% select(time_h, well, dupmean, type, volume)
colnames(af)<- c("time_h", "well", "mean", "type", "volume")
af<- af%>% distinct(time_h, type, volume, .keep_all = TRUE) #remove duplicate values

#aggregate free data
free_controls<- subset(avgcon, type == "free")
free_controls$beadcon<- NULL
free_mean<- group_by(free_controls, sample, time_h) %>% summarize(mean = mean(OD600)) #calculate mean of duplicates
free_mean<- left_join(free_mean, free_controls, by = c("sample", "time_h")) #add mean to OG data set
free_mean<- free_mean %>% distinct(sample, time_h, .keep_all = TRUE) #remove duplicates, keep mean

#merge bead controls back with all controls

allcontrols <- full_join(af, free_mean)
allcontrols$strain<- NULL
allcontrols$OD600<- NULL
allcontrols$OD600_b<- NULL
allcontrols$well<- NULL
allcontrols$well_b<- NULL
allcontrols$sample<- NULL
allcontrols$sample_b<- NULL
#colnames(allcontrols)<- c("time_h", "type", "volume", )
tmp<- merge(test, allcontrols, by  = c("time_h", "type", "volume")) #merge datasets by time_h and type
tmp$corrected<- tmp$OD600 - tmp$mean #blank subtraction
tmp[tmp < 0 ] <- 0 #convert negative values to zero

#colnames(control)<- c("well_control", "OD600_control", "time_h", "sample_control", "type", "volume")
#tmp<- merge(control, test, by = c("time_h", "type", "volume")) #merge datasets by time_h and type
#tmp$corrected<- tmp$OD600 - tmp$OD600_control #blank subtraction
#tmp[tmp < 0 ] <- 0 #convert negative values to zero


write_csv(tmp, "Blank Subtraction 17_18.csv") #save data as csv


graph_data <- tmp
beads<- subset(graph_data, type == "bead")
free<- subset(graph_data, type == "free")

library(ggplot2)

#graph_data<- subset(tmp, corrected >=0)
ggplot(data = beads, aes(x = time_h, y = corrected, color = sample)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))
ggplot(data = free, aes(x = time_h, y = corrected, color = sample)) +
  geom_point() + geom_smooth() + theme_classic() + xlab("Time") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))

#divide up by volume

two <- subset(tmp, volume == "200")
ggplot(data = two, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))

one <- subset(tmp, volume == "100")
ggplot(data = one, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))+ggtitle("All Samples in 100uL")
beadsone<- subset(one, type == "bead")
ggplot(data = beadsone, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))+ggtitle("All Beads in 100uL")
beadsonef<- subset(beadsone, strain == "f199")
ggplot(data = beadsonef, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))+ggtitle("F199 Beads in 100uL")
beadsoner<- subset(beadsone, strain == "rep2")
ggplot(data = beadsoner, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))+ggtitle("Rep2 Beads in 100uL")

two <- subset(tmp, volume == "200")
ggplot(data = two, aes(x = time_h, y = corrected, color = sample, shape = type, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))+ggtitle("All Samples in 200uL")
beadstwo<- subset(two, type == "bead")
ggplot(data = beadstwo, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))+ggtitle("All Beads in 200uL")
beadstwof<- subset(beadstwo, strain == "f199")
ggplot(data = beadstwof, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))+ggtitle("F199 Beads in 200uL")
beadstwor<- subset(beadstwo, strain == "rep2")
ggplot(data = beadstwor, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))+ggtitle("Rep2 Beads in 200uL")

onef199<- subset(one, strain == "f199")
ggplot(data = onef199, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))+ggtitle("F199 in 100 uL")


twof199<- subset(two, strain == "f199")
ggplot(data = twof199, aes(x = time_h, y = corrected, color = sample, shape = type)) + 
  geom_point() + geom_smooth(se = FALSE) + 
  theme_classic() + 
  xlab("Time") + 
  ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))+
  ggtitle("F199 in 200 uL")




