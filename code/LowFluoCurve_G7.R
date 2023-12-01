library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(ggplot2)
#path to data
data_location1 <- here::here("data","LowFluoCurveG7_20231025.xlsx")

data_location3 <- here::here("data", "plate_LowFluoCurveG7_20231026.xlsx")
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

long <- join %>% gather(meas, value, mScarlet:mVenus, factor_key=TRUE)
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
  ggtitle("G7 Standard Curve: OD Dilutions vs RFU Value")+
  scale_color_manual(values = c("mScarlet" = "indianred3", 
                                "mVenus" = "yellow1"))

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
mVen.df<- subset(calc, meas == "mVenus")
mVen.lm<- lm(value~OD, data = mVen.df)
summary(mVen.lm)



ggplot(data = calc, aes(x = OD, y = value, color = meas)) +
  geom_point(stat='identity') +
  geom_smooth(method='lm', se=FALSE)+
  theme_classic() + 
  xlab("OD Value (Achieved by Dilution)") + 
  ylab("RFU Value")+ 
  theme(axis.text.x=element_text(angle = 0, hjust = 0))+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("G7 Standard Curve: OD Dilutions vs RFU Value")+
  scale_color_manual(values = c("mScarlet" = "indianred3", 
                                "mVenus" = "gold")) +
  annotate("text", x = c(.05,.045), y = c(1200,4700),  
           label = c("y = 5.4E04x + 1100 | R^2 = 0.9976","y = 6.6E04x + 35 | R^2 = 0.9946"), 
           color= c("gold", "indianred3"),  size=3.5 , fontface="bold")


#ADDING PLATE COUNTS 10/20/2023
data_location2 <- here::here("data","LowFluoCurveG7_PlateCounts_20231026.xlsx")
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
  ggtitle("G7 Standard Curve: OD Dilutions vs CFU/mL")

#get averages
plate.avg<- plate_count %>% group_by(sample) %>% summarize(avg.cfu = mean(cfu))

#join cfu counts to calc data
join_all <- left_join(calc, plate.avg, by = "sample")

cfu.lm<- lm(avg.cfu~OD, data = join_all) #estimate coefficients + r2
summary(cfu.lm) #get values

#subset to look at fluor data
mScar.cfu<- subset(join_all, meas == "mScarlet")
mVen.cfu<- subset(join_all, meas == "mVenus")

#calculate values for OD
cfu.lm.Scar<- lm(avg.cfu~OD, data = mScar.cfu) #estimate coefficients + r2
summary(cfu.lm.Scar) #get values

cfu.lm.mVen<- lm(avg.cfu~OD, data = mVen.cfu) #estimate coefficients + r2
summary(cfu.lm.mVen) #get values

#calculate values for RFU
cfu.lm.Scar.rfu<- lm(avg.cfu~value, data = mScar.cfu) #estimate coefficients + r2
summary(cfu.lm.Scar.rfu) #get values

cfu.lm.mVen.rfu<- lm(avg.cfu~value, data = mVen.cfu) #estimate coefficients + r2
summary(cfu.lm.mVen.rfu) #get values
#these R squared values seem really good! looks like G7 is better with fluorescence than F199. Think we may also have lower LOD, etc

#VISUALIZE IN PLOTS
ggplot(data = join_all, aes(x = value, y = avg.cfu, color = meas)) +
  geom_point(stat='identity') +
  geom_smooth(method='lm', se=FALSE)+
  theme_classic() + 
  xlab("RFU Value") + 
  ylab("CFU/mL")+ 
  theme(axis.text.x=element_text(angle = 0, hjust = 0))+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("G7 Standard Curve: CFU/mL vs RFU Value")+
  scale_color_manual(values = c("mScarlet" = "indianred3", 
                                "mVenus" = "gold"))+
  annotate("text", x = c(5000,2000), y = c(3e+07,1.1E+08),  
           label = c("y = 2.2e04x - 2.9e07 | R^2 = 0.9656","y = 1.8e04x - 4.4e06 | R^2 = 0.9692"), 
           color= c("gold", "indianred3"),  size=3.5 , fontface="bold")

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
        legend.position = "bottom", panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("G7 Standard Curve: CFU/mL vs RFU Value")+
  scale_color_manual(values = c("mScarlet" = "indianred3", 
                                "mVenus" = "gold"))+
  annotate("text", x = c(5000,2000), y = c(1e+07,1.1E+08),  
           label = c("y = 2.2e04x - 2.9e07 | R^2 = 0.9656","y = 1.8e04x - 4.4e06 | R^2 = 0.9692"), 
           color= c("gold", "indianred3"),  size=5 , fontface="bold") +
  scale_y_continuous(trans="log10")

#this does look better. adjusting annotate so equations don't block line

#fixing figure for poster
fig <- join_all %>% filter(meas == "mScarlet")
ggplot(data = fig, aes(x = value, y = avg.cfu, color = meas)) +
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
        panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("G7: CFU/mL vs RFU")+
  scale_color_manual(values = c("mScarlet" = "indianred3"))+
  annotate("text", x = c(2000), y = c(1.1E+08),  
           label = c("y = 1.8e04x - 4.4e06 | R^2 = 0.9692"), 
           color= c("indianred3"),  size=5 , fontface="bold") +
  scale_y_continuous(trans="log10")






##OLD CODE - pausing here 10/30/23 2pm

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
  ggtitle("G7 Standard Curve: CFU/mL vs RFU Value")+
  scale_color_manual(values = c("mScarlet" = "indianred3", 
                                "mVenus" = "gold"))
#need to change equation if displaying log values
+
  annotate("text", x = c(5000,2000), y = c(7,8),  
           label = c("y = 2.2e04x - 2.9e07 | R^2 = 0.9656","y = 1.8e04x - 4.4e06 | R^2 = 0.9692"), 
           color= c("gold", "indianred3"),  size=3.5 , fontface="bold")

#fit the model
test <- lm(join_all$log ~ join_all$value)
summary(test)
#fit is not better for exponential model than for linear model. fit is worse

#view the output of the model
summary(model)


#"long" has negative controls. let's look there to estimate LOD

control<- long %>% subset(sample == c("srb15"))
control.Scar<- control %>% subset(meas == "mScarlet")
control.mVenus<- control %>% subset(meas =="mVenus")

mean(control.Scar$value)
mean(control.mVenus$value)

#LOB (limit of blank)
(mean(control.Scar$value) + 1.645*sd(control.Scar$value))
#LOB is 22.55 for mScarlet

#low concentration sample
low <- long %>% subset(sample == .01)

#LOD = LoB + 1.645(SD low concentration sample)
22.55+1.645*sd(low$value)
#LOD is XXX RFU for mScarlet
#this number is getting conflated due to mVenus. need to rerun several blanks for each fluorophore to improve these calculations

#LOB (limit of blank)
(mean(control.GFP$value) + 1.645*sd(control.GFP$value))
#LOB is XXX for mVenus

#LOD = LoB + 1.645(SD low concentration sample)
LOB+1.645*sd(low$value)
#LOD is XXX RFU for mVenus

#Detection limit equations from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2556583/