library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(ggplot2)
#path to data
data_location1 <- here::here("data","LowFluoCurveF199_20231018.xlsx")

data_location3 <- here::here("data", "plate_LowFluoCurveF199_20231018.xlsx")
#load data. 
rfu <- read_excel(data_location1)

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
select.<- long%>% filter(sample != "10")

#plot results
ggplot(data = select., aes(x = sample, y = value, color = meas)) +
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
#removing negative controls
calc <- long %>% filter(sample != "pbs") %>% filter(sample != "srb15")
glimpse(calc)
calc$sample <- as.numeric(calc$sample)
calc <- calc %>% mutate(OD = 0.1*sample)  
glimpse(calc)

#remove positive control to get better visualization
calc<- calc%>% filter(sample != "10")


#plot results

#get numbers for equation
#subset to split by meas
mScar.df<- subset(calc, meas == "mScarlet")

mScar.lm<- lm(value~OD, data = mScar.df) #estimate coefficients + r2
summary(mScar.lm) #get values
GFP.df<- subset(calc, meas == "GFP")
GFP.lm<- lm(value~OD, data = GFP.df)
summary(GFP.lm)



ggplot(data = calc, aes(x = OD, y = value, color = meas)) +
  geom_point(stat='identity') +
  geom_smooth(method='lm', se=FALSE)+
  theme_classic() + 
  xlab("OD Value (Achieved by Dilution)") + 
  ylab("RFU Value")+ 
  theme(axis.text.x=element_text(angle = 0, hjust = 0))+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("F199 Standard Curve: OD Dilutions vs RFU Value")+
  scale_color_manual(values = c("mScarlet" = "indianred3", 
                                "GFP" = "palegreen")) +
  annotate("text", x = c(.05,.045), y = c(1000,4700),  
           label = c("y = 5E04x + 213 | R^2 = 0.9969","y = 5E04x + 350 | R^2 = 0.9952"), 
           color= c("palegreen", "indianred3"),  size=3.5 , fontface="bold")
  

#ADDING PLATE COUNTS 10/20/2023
data_location2 <- here::here("data","LowFluoCurveF199_PlateCounts_20231018.xlsx")
plate_count <- read_excel(data_location2)

#calculate CFU/mL
plate_count <- plate_count %>% mutate(cfu = plate_count*best_dilution/0.1)

#plot diluted OD vs plate count
ggplot(data = plate_count, aes(x = sample, y = cfu)) +
  geom_point(stat='identity') +
  geom_smooth(method = "lm", se = FALSE)+
  theme_classic() + 
  xlab("Dilution (fraction of OD 0.1)") + 
  ylab("CFU/mL")+ 
  theme(axis.text.x=element_text(angle = -45, hjust = 0))+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("F199 Standard Curve: OD Dilutions vs CFU/mL")

#get averages
plate.avg<- plate_count %>% group_by(sample) %>% summarize(avg.cfu = mean(cfu))

#join cfu counts to calc data
join_all <- left_join(calc, plate.avg, by = "sample")

cfu.lm<- lm(avg.cfu~OD, data = join_all) #estimate coefficients + r2
summary(cfu.lm) #get values

#subset to look at fluor data
mScar.cfu<- subset(join_all, meas == "mScarlet")
GFP.cfu<- subset(join_all, meas == "GFP")

#calculate values for OD
cfu.lm.Scar<- lm(avg.cfu~OD, data = mScar.cfu) #estimate coefficients + r2
summary(cfu.lm.Scar) #get values

cfu.lm.GFP<- lm(avg.cfu~OD, data = GFP.cfu) #estimate coefficients + r2
summary(cfu.lm.GFP) #get values

#calculate values for RFU
cfu.lm.Scar.rfu<- lm(avg.cfu~value, data = mScar.cfu) #estimate coefficients + r2
summary(cfu.lm.Scar.rfu) #get values

cfu.lm.GFP.rfu<- lm(avg.cfu~value, data = GFP.cfu) #estimate coefficients + r2
summary(cfu.lm.GFP.rfu) #get values

#VISUALIZE IN PLOTS
ggplot(data = join_all, aes(x = value, y = avg.cfu, color = meas)) +
  geom_point(stat='identity') +
  geom_smooth(method='lm', se=FALSE)+
  theme_classic() + 
  xlab("RFU Value") + 
  ylab("CFU/mL")+ 
  theme(axis.text.x=element_text(angle = 0, hjust = 0))+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("F199 Standard Curve: CFU/mL vs RFU Value")+
  scale_color_manual(values = c("mScarlet" = "indianred3", 
                                "GFP" = "palegreen")) +
  annotate("text", x = c(2000,4000), y = c(6e+08,3E+08),  
           label = c("y = 1.8e05x - 7.9e07 | R^2 = 0.8161","y = 1.7e05x - 1.0e08 | R^2 = 0.8047"), 
           color= c("forestgreen", "indianred3"),  size=3.5 , fontface="bold")

#attempt log scale to see values better. same plot. different scale - changing ylim instead of using y scale function
ggplot(data = join_all, aes(x = value, y = avg.cfu, color = meas)) +
  geom_point(stat='identity') +
  geom_smooth(method='lm', se=FALSE)+
  theme_classic() + 
  xlab("RFU Value") + 
  ylab("CFU/mL")+ 
  theme(axis.text.x=element_text(angle = 0, hjust = 0, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.position = "bottom")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("F199 Standard Curve: CFU/mL vs RFU Value")+
  scale_color_manual(values = c("mScarlet" = "indianred3", 
                                "GFP" = "palegreen")) +
  annotate("text", x = c(2000,4000), y = c(6e+08,1E+07),  
           label = c("y = 1.8e05x - 7.9e07 | R^2 = 0.8161","y = 1.7e05x - 1.0e08 | R^2 = 0.8047"), 
           color= c("forestgreen", "indianred3"),  size=5 , fontface="bold") +
  scale_y_continuous(trans="log10")

#this does look better. adjusting annotate so equations don't block line
#ylim(2.700e+06, max(join_all$avg.cfu)) why is this not working??

#how much would the fit be improved without the lowest value? - want to test this (note 10/27/23)
#removing lowest value, which is below limit of detection. rerunning models
remove<- join_all %>% filter(value>300)
#remodel
#subset to look at fluor data
mScar.cfu.r<- subset(remove, meas == "mScarlet")
GFP.cfu.r<- subset(remove, meas == "GFP")
#calculate values for RFU
cfu.lm.Scar.r<- lm(avg.cfu~value, data = mScar.cfu.r) #estimate coefficients + r2
summary(cfu.lm.Scar.r) #get values

cfu.lm.GFP.r<- lm(avg.cfu~value, data = GFP.cfu.r) #estimate coefficients + r2
summary(cfu.lm.GFP.r) #get values

#this does not appear to improve R2 values at all. why? 

#just kidding. need to log actual value. 
join_all$log<- log10(join_all$avg.cfu)
#plot again
ggplot(data = join_all, aes(x = value, y = log, color = meas)) +
  geom_point(stat='identity') +
  geom_smooth(method='lm', se=FALSE)+
  theme_classic() + 
  xlab("RFU Value") + 
  ylab("log(CFU/mL)")+ 
  theme(axis.text.x=element_text(angle = 0, hjust = 0))+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("F199 Standard Curve: CFU/mL vs RFU Value")+
  scale_color_manual(values = c("mScarlet" = "indianred3", 
                                "GFP" = "palegreen")) +ylim(6, 9)+
  annotate("text", x = c(2000,4000), y = c(8.7,6.6),  
           label = c("y = 1.8e05x - 7.9e07 | R^2 = 0.8161","y = 1.7e05x - 1.0e9 | R^2 = 0.8047"), 
           color= c("forestgreen", "indianred3"),  size=3.5 , fontface="bold")

#fit the model
test <- lm(join_all$log ~ join_all$value)
summary(test)
#fit is not better for exponential model than for linear model

#view the output of the model
summary(model)

#test switching axes
ggplot(data = join_all, aes(x = log, y = value, color = meas)) +
  geom_point(stat='identity') +
  geom_smooth(method='lm', se=FALSE)+
  theme_classic() + 
  xlab("log(CFU/mL)") + 
  ylab("RFU Value")+ 
  theme(axis.text.x=element_text(angle = 0, hjust = 0))+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("F199 Standard Curve: CFU/mL vs RFU Value")+
  scale_color_manual(values = c("mScarlet" = "indianred3", 
                                "GFP" = "palegreen"))
  
#"long" has negative controls. let's look there to estimate LOD

control<- long %>% subset(sample == c("srb15"))
control.Scar<- control %>% subset(meas == "mScarlet")
control.GFP<- control %>% subset(meas =="GFP")

mean(control.Scar$value)
mean(control.GFP$value)

#LOB (limit of blank)
(mean(control.Scar$value) + 1.645*sd(control.Scar$value))
#LOB is 379.0042 for mScarlet

#low concentration sample
low <- long %>% subset(sample == .01)

#LOD = LoB + 1.645(SD low concentration sample)
379.0042+1.645*sd(low$value)
#LOD is 478.7478 RFU for mScarlet

#LOB (limit of blank)
(mean(control.GFP$value) + 1.645*sd(control.GFP$value))
#LOB is 242.3142 for GFP

#LOD = LoB + 1.645(SD low concentration sample)
242.3142+1.645*sd(low$value)
#LOD is 342.0578 RFU for GFP

#Detection limit equations from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2556583/