library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
#path to data
data_location3 <- here::here("data","GC_6_6 48 hr.xlsx")

#load data. 
rawdata <- read_excel(data_location3)

#take a look at the data
glimpse(rawdata)

data<- rawdata %>% slice(2:49)
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
data_location4 <- here::here("data","plate_6_05.xlsx")

#load data. 
plate <- read_excel(data_location4)

plate <- plate %>% pivot_longer(cols = !row, 
                                names_to = "column_num", 
                                values_to = "sample")
plate$well <- paste(plate$row, plate$column_num, sep="") 
plate <- plate %>% select(3:4)

#join names to sample data file
join2 <- left_join(plate2, plate, by = "well") %>% filter(sample != "well_blank")

#ID based on type
df <- join2 %>% mutate(type = case_when(endsWith(join2$sample, "c") ~ "chitosan_bead",
                                        endsWith(join2$sample, "sa") ~ "alginate_bead",
                                        startsWith(join2$sample, "f") ~ "free", 
                                        startsWith(join2$sample, "lb") ~ "free")) 

endsWith("what", "t")

#ID sample based on strain
df <- df %>% mutate(strain = case_when(startsWith(df$sample, "b_rep") ~ "rep2", 
                                       startsWith(df$sample, "f_rep") ~ "rep2",
                                       startsWith(df$sample, "b_f199") ~ "f199",
                                       startsWith(df$sample, "f_f199") ~ "f199",
                                       startsWith(df$sample, "b_g7") ~ "g7",
                                       startsWith(df$sample, "f_g7") ~ "g7",
                                       startsWith(df$sample, "lb") ~ "control", 
                                       startsWith(df$sample, "bb") ~ "control"))


control<- subset(df, strain == "control")  
test<- subset(df, strain != "control")

#take averages of blanks
control<- control %>% group_by(time_h, sample, type, strain) %>% summarize(OD600 = mean(OD600))
colnames(control)<- c("time_h", "control_sample", "type", "strain", "OD600_b")
control$strain<- NULL
merge<- merge(test, control, by = c("time_h", "type"))

merge$corrected<- merge$OD600 - merge$OD600_b #blank subtraction
merge[merge < 0 ] <- 0 #convert negative values to zero



ggplot(data = merge, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Rep2, F199, and G7 in 48-Well Plate")

f199<- subset(merge, strain == "f199")
ggplot(data = f199, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("F199 in 48-Well Plate")

rep2<- subset(merge, strain == "rep2")
ggplot(data = rep2, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Rep2 in 48-Well Plate")

g7<- subset(merge, strain == "g7")
ggplot(data = g7, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("G7 in 48-Well Plate")

# out of curiosity - plot non-corrected
ggplot(data = control, aes(x = time_h, y = OD600_b, color = control_sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Control OD600")

test_f199<- subset(test, strain == "f199")
ggplot(data = test_f199, aes(x = time_h, y = OD600, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Test_F199 in 48-Well Plate")

test_rep2<- subset(test, strain == "rep2")
ggplot(data = test_rep2, aes(x = time_h, y = OD600, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Test_Rep2 in 48-Well Plate")

test_g7<- subset(test, strain == "g7")
ggplot(data = test_g7, aes(x = time_h, y = OD600, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Test_G7 in 48-Well Plate")


#plots are messy - attempt summarizing/aggregating 6 replicates into one line

sum<- merge %>% group_by(time_h, type, sample, strain) %>% summarize(OD600 = mean(OD600))
cosum<- merge %>% group_by(time_h, type, sample, strain) %>% summarize(corrected = mean(corrected))

ggplot(data = sum, aes(x = time_h, y = OD600, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("Avg OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Average OD600 for Rep2, F199, G7 in 48-Well Plate")

ggplot(data = cosum, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("Avg OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Average Corrected OD600 for Rep2, F199, G7 in 48-Well Plate")

#seeing some weird stuff with my control - it's also exhibiting a "growth curve" - could it be contaminated, or does this behavior happen every time?


#what if I just subtract the blank value of the blank beads at time zero in order to correct? Plate seems to show that my aseptic technique was sufficient, 
#because LB (free control) is not contaminated. This suggests that perhaps the blank beads themselves are contaminated, but maybe this doesn't mean I have to throw out the data - since there are differences, 
#I don't think contamination interfered at all levels of the experiment. Lets see how this looks

#what is the average value of the blank beads at time zero?

zero<- control %>% filter(time_h == 0.0000000)

cor<- merge(test, zero, by = c("type"))

cor$blank_cor<- cor$OD600 - cor$OD600_b #blank subtraction
cor[cor < 0 ] <- 0 #convert negative values to zero

#PLOTS
ggplot(data = cor, aes(x = time_h.x, y = blank_cor, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Rep2, F199, and G7 in 48-Well Plate")

f199<- subset(cor, strain == "f199")
ggplot(data = f199, aes(x = time_h.x, y = blank_cor, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("F199 in 48-Well Plate")

rep2<- subset(cor, strain == "rep2")
ggplot(data = rep2, aes(x = time_h.x, y = blank_cor, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Rep2 in 48-Well Plate")

g7<- subset(cor, strain == "g7")
ggplot(data = g7, aes(x = time_h.x, y = blank_cor, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("G7 in 48-Well Plate")



zsum <- cor %>% group_by(time_h.x, type, sample, strain) %>% summarize(blank_cor = mean(blank_cor))


ggplot(data = zsum2, aes(x = time_h.x, y = blank_cor, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("Avg OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"),
        axis.text.x=element_text(angle = 0, hjust = 0, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 14)) +ggtitle("Average OD600 for Rep2, F199, & G7 Systems")+
  facet_wrap(~ type2)

#capitalize
zsum<- zsum %>% mutate(Strain = fct_recode(strain, 
                                                     "F199" = "f199", 
                                                     "G7" = "g7", 
                                                     "Rep2" = "rep2")) %>%
  mutate(Type = fct_recode(type, 
                           "Alginate Bead" = "alginate_bead", 
                           "Chitosan Bead" = "chitosan_bead", 
                           "Free" = "free"))
glimpse(zsum)
#no geom smooth
ggplot(data = zsum, aes(x = time_h.x, y = blank_cor, color = Strain, shape = Type)) +
  geom_point() + theme_classic() + xlab("Time (hr)") + ylab("Avg OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"),
        axis.text.x=element_text(angle = 0, hjust = 0, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 14)) +ggtitle("Average OD600 for Rep2, F199, & G7 Systems")+
  facet_wrap(~ Type)+
  scale_color_brewer(palette="Set2")
ggsave("avgODrep2_f199_g7.png", width = 8, height = 5)

#ggplot(data = cosum, aes(x = time_h, y = corrected, color = sample, shape = type)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("Avg OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +ggtitle("Average Corrected OD600 for Rep2, F199, G7 in 48-Well Plate")

