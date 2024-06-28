library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(ggplot2)
library("writexl", )

#load data. 
expA_Day0 <- read_excel(here::here("data", "expA_Day0_gainrerun_20240507.xlsx"))
expA_Day7 <- read_excel(here::here("data", "expA_Day7_rerun_20240507.xlsx"))
expA_Day14 <- read_excel(here::here("data", "expA_Day14_20240514.xlsx"))
expA_Day21 <- read_excel(here::here("data", "expA_Day21_20240521.xlsx"))
expA_Day28 <- read_excel(here::here("data", "expA_Day28_20240529.xlsx"))
expA_Day35 <- read_excel(here::here("data", "expA_Day35_20240604.xlsx"))
expA_Day42 <- read_excel(here::here("data", "expA_Day42_20240611.xlsx"))
expA_Day49 <- read_excel(here::here("data", "expA_Day49_20240618.xlsx"))
expA_Day56 <- read_excel(here::here("data", "expA_Day56_20240625.xlsx"))
expB_Day0 <- read_excel(here::here("data", "expB_Day0_gain_20240422.xlsx"))
expB_Day7 <- read_excel(here::here("data", "expB_Day7_20240426.xlsx"))
expB_Day14 <- read_excel(here::here("data", "expB_Day14_20240502.xlsx"))
expB_Day21 <- read_excel(here::here("data", "expB_Day21_20240509.xlsx"))
expB_Day28 <- read_excel(here::here("data", "expB_Day28_20240516.xlsx"))
expB_Day35 <- read_excel(here::here("data", "expB_Day35_20240523.xlsx"))
expB_Day42 <- read_excel(here::here("data", "expB_Day42_20240529.xlsx"))
expB_Day49 <- read_excel(here::here("data", "expB_Day49_20240606.xlsx"))
expB_Day56 <- read_excel(here::here("data", "expB_Day56_20240613.xlsx"))
expC_Day0 <- read_excel(here::here("data", "expC_Day0_20240424_rerunmanual.xlsx"))
expC_Day7 <- read_excel(here::here("data", "expC_Day7_20240501.xlsx"))
expC_Day14 <- read_excel(here::here("data", "expC_day14_20240507.xlsx"))
expC_Day21 <- read_excel(here::here("data", "expC_Day21_20240514.xlsx"))
expC_Day28 <- read_excel(here::here("data", "expC_Day28_20240521.xlsx"))
expC_Day35 <- read_excel(here::here("data", "expC_Day35_20240529.xlsx"))
expC_Day42 <- read_excel(here::here("data", "expC_Day42_20240604.xlsx"))
expC_Day49 <- read_excel(here::here("data", "expC_Day49_20240611.xlsx"))
expC_Day56 <- read_excel(here::here("data", "expC_Day56_20240618.xlsx"))
expD_Day0 <- read_excel(here::here("data", "expD_Day0_20240426.xlsx"))
expD_Day7 <- read_excel(here::here("data", "expD_Day7_20240502.xlsx"))
expD_Day14 <- read_excel(here::here("data", "expD_Day14_20240509.xlsx"))
expD_Day21 <- read_excel(here::here("data", "expD_Day21_20240516.xlsx"))
expD_Day28 <- read_excel(here::here("data", "expD_Day28_20240523.xlsx"))
expD_Day35 <- read_excel(here::here("data", "expD_Day35_20240529.xlsx"))
expD_Day42 <- read_excel(here::here("data", "expD_Day42_20240606.xlsx"))
expD_Day49 <- read_excel(here::here("data", "expD_Day49_20240613.xlsx"))
expD_Day56 <- read_excel(here::here("data", "expD_Day56_20240620.xlsx"))

glimpse(expA_Day0) #GFP is character
glimpse(expA_Day7) #GFP is double
#need to make the same factor type

#clean data
#Exp A
expA_Day0<- expA_Day0[1:70,] %>% select(!2)
expA_Day0$GFP<- as.numeric(expA_Day0$GFP) #making GFP numeric since it was imported as character
expA_Day7<- expA_Day7[1:36,] %>% select(!2)
expA_Day14<- expA_Day14[1:86,] %>% select(!2)
expA_Day14$GFP<- as.numeric(expA_Day14$GFP)
expA_Day21<- expA_Day21[1:67,] %>% select(!2)
expA_Day28<- expA_Day28[1:86,] %>% select(!2)
expA_Day35<- expA_Day35[1:67,] %>% select(!2)
expA_Day42<- expA_Day42[1:86,] %>% select(!2)
expA_Day49<- expA_Day49[1:67,] %>% select(!2)
expA_Day56<- expA_Day56[1:86,] %>% select(!2)
#Exp B
expB_Day0<- expB_Day0 %>% na.omit() %>% select(!2)
expB_Day7<- expB_Day7 %>% na.omit() %>% select(!2)
expB_Day14<- expB_Day14 %>% na.omit() %>% select(!2)
expB_Day21<- expB_Day21 %>% na.omit() %>% select(!2)
expB_Day28<- expB_Day28 %>% na.omit() %>% select(!2)
expB_Day28$mScarlet_g7<- as.numeric(expB_Day28$mScarlet_g7)
expB_Day35<- expB_Day35 %>% na.omit() %>% select(!2)
expB_Day42<- expB_Day42 %>% na.omit() %>% select(!2)
expB_Day49<- expB_Day49 %>% na.omit() %>% select(!2)
expB_Day56<- expB_Day56 %>% na.omit() %>% select(!2)
#Exp C
expC_Day0<- expC_Day0 %>% na.omit() %>% select(!2)
expC_Day7<- expC_Day7 %>% na.omit() %>% select(!2)
expC_Day14<- expC_Day14 %>% na.omit() %>% select(!2)
expC_Day21<- expC_Day21 %>% na.omit() %>% select(!2)
expC_Day28<- expC_Day28 %>% na.omit() %>% select(!2)
expC_Day35<- expC_Day35 %>% na.omit() %>% select(!2)
expC_Day42<- expC_Day42 %>% na.omit() %>% select(!2)
expC_Day49<- expC_Day49 %>% na.omit() %>% select(!2)
expC_Day56<- expC_Day56 %>% na.omit() %>% select(!2)
#Exp D
expD_Day0<- expD_Day0[1:70,] %>% select(!2)
expD_Day7<- expD_Day7 %>% na.omit() %>% select(!2)
expD_Day14<- expD_Day14 %>% na.omit() %>% select(!2)
expD_Day21<- expD_Day21 %>% na.omit() %>% select(!2)
expD_Day28<- expD_Day28 %>% na.omit() %>% select(!2)
expD_Day35<- expD_Day35 %>% na.omit() %>% select(!2)
expD_Day42<- expD_Day42 %>% na.omit() %>% select(!2)
expD_Day49<- expD_Day49 %>% na.omit() %>% select(!2)
expD_Day56<- expD_Day56 %>% na.omit() %>% select(!2)

glimpse(expB_Day28)

#rename

#ExpA
colnames(expA_Day0)[1] = "well"
colnames(expA_Day7)[1] = "well"
colnames(expA_Day14)[1] = "well"
colnames(expA_Day21)[1] = "well"
colnames(expA_Day28)[1] = "well"
colnames(expA_Day35)[1] = "well"
colnames(expA_Day42)[1] = "well"
colnames(expA_Day49)[1] = "well"
colnames(expA_Day56)[1] = "well"
#ExpB
colnames(expB_Day0)[1] = "well"
colnames(expB_Day7)[1] = "well"
colnames(expB_Day14)[1] = "well"
colnames(expB_Day21)[1] = "well"
colnames(expB_Day28)[1] = "well"
colnames(expB_Day35)[1] = "well"
colnames(expB_Day42)[1] = "well"
colnames(expB_Day49)[1] = "well"
colnames(expB_Day56)[1] = "well"
#ExpC
colnames(expC_Day0)[1] = "well"
colnames(expC_Day7)[1] = "well"
colnames(expC_Day14)[1] = "well"
colnames(expC_Day21)[1] = "well"
colnames(expC_Day28)[1] = "well"
colnames(expC_Day35)[1] = "well"
colnames(expC_Day42)[1] = "well"
colnames(expC_Day49)[1] = "well"
colnames(expC_Day56)[1] = "well"
#ExpD
colnames(expD_Day0)[1] = "well"
colnames(expD_Day7)[1] = "well"
colnames(expD_Day14)[1] = "well"
colnames(expD_Day21)[1] = "well"
colnames(expD_Day28)[1] = "well"
colnames(expD_Day35)[1] = "well"
colnames(expD_Day42)[1] = "well"
colnames(expD_Day49)[1] = "well"
colnames(expD_Day56)[1] = "well"

#add time point to data frame
#ExpA
expA_Day0$day <- 0
expA_Day7$day <- 7
expA_Day14$day <- 14
expA_Day21$day <- 21
expA_Day28$day <- 28
expA_Day35$day <- 35
expA_Day42$day <- 42
expA_Day49$day <- 49
expA_Day56$day <- 56
#ExpB
expB_Day0$day<- 0
expB_Day7$day<- 7
expB_Day14$day<- 14
expB_Day21$day <- 21
expB_Day28$day <- 28
expB_Day35$day <- 35
expB_Day42$day <- 42
expB_Day49$day <- 49
expB_Day56$day <- 56
#ExpC
expC_Day0$day<- 0
expC_Day7$day<- 7
expC_Day14$day <- 14
expC_Day21$day <- 21
expC_Day28$day <- 28
expC_Day35$day <- 35
expC_Day42$day <- 42
expC_Day49$day <- 49
expC_Day56$day <- 56
#ExpD
expD_Day0$day<- 0
expD_Day7$day<- 7
expD_Day14$day <- 14
expD_Day21$day <- 21
expD_Day28$day <- 28
expD_Day35$day <- 35
expD_Day42$day <- 42
expD_Day49$day <- 49
expD_Day56$day <- 56

#combine days within the same experiment with same original plate layout
expA<- expA_Day0 %>% full_join(expA_Day14)
expB <- expB_Day0 %>% full_join(expB_Day14) %>% full_join(expB_Day28)
expC <- expC_Day0 %>% full_join(expC_Day14)
expD <- expD_Day0 %>% full_join(expD_Day14)
#super
expA_super<- expA_Day7
expB_super<- expB_Day7 %>% full_join(expB_Day21)
expC_super<- expC_Day7 %>% full_join(expC_Day21)
expD_super<- expD_Day7 %>% full_join(expD_Day21)

#combine days within same experiment with new plate layout
expD_new <- expD_Day28 %>% full_join(expD_Day42) %>% full_join(expD_Day56) #new plate layout because I switched well-mixed and regular supernatant on plate
expC_new <- expC_Day28 %>% full_join(expC_Day42) %>% full_join(expC_Day56)
expA_new <- expA_Day28 %>% full_join(expA_Day42) %>% full_join(expA_Day56)
expB_new <- expB_Day42 %>% full_join(expB_Day56)

#super new - new layouts because I started adding well-mixed supernatant
expA_super_new<- expA_Day21 %>% full_join(expA_Day35) %>% full_join(expA_Day49)
expD_super_new<- expD_Day21 %>% full_join(expD_Day35) %>% full_join(expD_Day49)
expC_super_new<- expC_Day21 %>% full_join(expC_Day35) %>% full_join(expC_Day49)
expB_super_new<- expB_Day35 %>% full_join(expB_Day35) %>% full_join(expB_Day49)

#load plate layouts
plate_ad <- read_excel(here::here("data", "ExperimentA_platelayout.xlsx"))
plate_bc <- read_excel(here::here("data", "ExperimentB_platelayout.xlsx"))
plate_bzero <- read_excel(here::here("data", "ExperimentB_day0_platelayout.xlsx"))
plate_superA<- read_excel(here::here("data", "ExperimentA_superplatelayout.xlsx"))
plate_superB<- read_excel(here::here("data", "ExperimentB_superplatelayout.xlsx"))
plate_superC<- read_excel(here::here("data", "ExperimentC_superplatelayout.xlsx"))
plate_superD<- read_excel(here::here("data", "ExperimentD_superplatelayout.xlsx"))
#new
plate_ad_new<- read_excel(here::here("data", "ExperimentA_platelayout_new.xlsx"))
plate_bc_new<- read_excel(here::here("data", "ExperimentB_platelayout_new.xlsx"))
plate_superA_new<- read_excel(here::here("data", "ExperimentA_superplatelayout_new.xlsx")) #this also applies for exp D new
plate_superC_new<- read_excel(here::here("data", "ExperimentC_superplatelayout_new.xlsx")) #this also applies for exp B new

#clean and wrangle plate data
#for plate_ad
plate_ad[] <- lapply(plate_ad, as.character)
plate_ad <- plate_ad %>% pivot_longer(cols = !row, 
                                        names_to = "column_num", 
                                        values_to = "sample")
plate_ad$well <- paste(plate_ad$row, plate_ad$column_num, sep="") 
plate_ad <- plate_ad %>% select(3:4)
#for plate_bc
glimpse(plate_bc)
plate_bc[] <- lapply(plate_bc, as.character)
plate_bc <- plate_bc %>% pivot_longer(cols = !row, 
                                              names_to = "column_num", 
                                              values_to = "sample")
plate_bc$well <- paste(plate_bc$row, plate_bc$column_num, sep="") 
plate_bc <- plate_bc %>% select(3:4)

#for bzero
glimpse(plate_bzero)
plate_bzero[] <- lapply(plate_bzero, as.character)
plate_bzero <- plate_bzero %>% pivot_longer(cols = !row, 
                                      names_to = "column_num", 
                                      values_to = "sample")
plate_bzero$well <- paste(plate_bzero$row, plate_bzero$column_num, sep="") 
plate_bzero <- plate_bzero %>% select(3:4)

#for plate_superA
plate_superA[] <- lapply(plate_superA, as.character)
plate_superA<- plate_superA %>% pivot_longer(cols = !row, 
                                            names_to = "column_num", 
                                            values_to = "sample")
plate_superA$well <- paste(plate_superA$row, plate_superA$column_num, sep="") 
plate_superA <- plate_superA %>% select(3:4)
#for plate_superB
plate_superB[] <- lapply(plate_superB, as.character)
plate_superB<- plate_superB %>% pivot_longer(cols = !row, 
                                             names_to = "column_num", 
                                             values_to = "sample")
plate_superB$well <- paste(plate_superB$row, plate_superB$column_num, sep="") 
plate_superB <- plate_superB %>% select(3:4)
#for plate_superC
plate_superC[] <- lapply(plate_superC, as.character)
plate_superC<- plate_superC %>% pivot_longer(cols = !row, 
                                             names_to = "column_num", 
                                             values_to = "sample")
plate_superC$well <- paste(plate_superC$row, plate_superC$column_num, sep="") 
plate_superC <- plate_superC %>% select(3:4)
#for plate_superD
plate_superD[] <- lapply(plate_superD, as.character)
plate_superD<- plate_superD %>% pivot_longer(cols = !row, 
                                             names_to = "column_num", 
                                             values_to = "sample")
plate_superD$well <- paste(plate_superD$row, plate_superD$column_num, sep="") 
plate_superD <- plate_superD %>% select(3:4)

#for plate_ad_new
plate_ad_new[] <- lapply(plate_ad_new, as.character)
plate_ad_new <- plate_ad_new %>% pivot_longer(cols = !row, 
                                      names_to = "column_num", 
                                      values_to = "sample")
plate_ad_new$well <- paste(plate_ad_new$row, plate_ad_new$column_num, sep="") 
plate_ad_new <- plate_ad_new %>% select(3:4)

#for plate_bc_new
plate_bc_new[] <- lapply(plate_bc_new, as.character)
plate_bc_new <- plate_bc_new %>% pivot_longer(cols = !row, 
                                              names_to = "column_num", 
                                              values_to = "sample")
plate_bc_new$well <- paste(plate_bc_new$row, plate_bc_new$column_num, sep="") 
plate_bc_new <- plate_bc_new %>% select(3:4)

#for superA_new
glimpse(plate_superA_new)
plate_superA_new[] <- lapply(plate_superA_new, as.character)
plate_superA_new <- plate_superA_new %>% pivot_longer(cols = !row, 
                                      names_to = "column_num", 
                                      values_to = "sample")
plate_superA_new$well <- paste(plate_superA_new$row, plate_superA_new$column_num, sep="") 
plate_superA_new <- plate_superA_new %>% select(3:4)

#for superC_new
glimpse(plate_superC_new)
plate_superC_new[] <- lapply(plate_superC_new, as.character)
plate_superC_new <- plate_superC_new %>% pivot_longer(cols = !row, 
                                                      names_to = "column_num", 
                                                      values_to = "sample")
plate_superC_new$well <- paste(plate_superC_new$row, plate_superC_new$column_num, sep="") 
plate_superC_new <- plate_superC_new %>% select(3:4)

#join names to sample data file
#Exp A
joinA <- left_join(expA, plate_ad, by = "well")
longA <- joinA %>% gather(meas, value, GFP:mScarlet_f199, factor_key=TRUE) %>% na.omit()

#Exp A_new
joinA_new <- left_join(expA_new, plate_ad_new, by = "well")
longA_new <- joinA_new %>% gather(meas, value, GFP:mScarlet_f199, factor_key=TRUE) %>% na.omit()

#Exp B
joinB <- left_join(expB, plate_bc, by = "well")
longB <- joinB %>% gather(meas, value, mVenus:mScarlet_g7, factor_key=TRUE)

#Exp B new
joinB_new <- left_join(expB_new, plate_bc_new, by = "well")
longB_new <- joinB_new %>% gather(meas, value, mVenus:mScarlet_g7, factor_key=TRUE)
#Exp C
joinC <- left_join(expC, plate_bc, by = "well")
longC <- joinC %>% gather(meas, value, mVenus:mScarlet_g7, factor_key=TRUE)
#Exp C_new
joinC_new <- left_join(expC_new, plate_bc_new, by = "well")
longC_new <- joinC_new %>% gather(meas, value, mVenus:mScarlet_g7, factor_key=TRUE)

#Exp D
joinD <- left_join(expD, plate_ad, by = "well")
longD <- joinD %>% gather(meas, value, mVenus:mScarlet_g7, factor_key=TRUE)
#new
joinD_new <- left_join(expD_new, plate_ad_new, by = "well")
longD_new <- joinD_new %>% gather(meas, value, mVenus:mScarlet_g7, factor_key=TRUE)

#ExpA_super
joinA_super <- left_join(expA_super, plate_superA, by = "well")
longA_super <- joinA_super %>% gather(meas, value, GFP:mScarlet_f199, factor_key=TRUE) %>% na.omit()
#ExpA_super_new
joinA_super_new <- left_join(expA_super_new, plate_superA_new, by = "well")
longA_super_new <- joinA_super_new %>% gather(meas, value, GFP:mScarlet_f199, factor_key=TRUE) %>% na.omit()
#ExpB_super
joinB_super <- left_join(expB_super, plate_superB, by = "well")
longB_super <- joinB_super %>% gather(meas, value, mVenus:mScarlet_g7, factor_key=TRUE) %>% na.omit()
#ExpB_super_new
joinB_super_new <- left_join(expB_super_new, plate_superC_new, by = "well")
longB_super_new <- joinB_super_new %>% gather(meas, value, mVenus:mScarlet_g7, factor_key=TRUE) %>% na.omit()
#ExpC_super
joinC_super <- left_join(expC_super, plate_superC, by = "well")
longC_super <- joinC_super %>% gather(meas, value, mVenus:mScarlet_g7, factor_key=TRUE) %>% na.omit()
#ExpC_super_new
joinC_super_new <- left_join(expC_super_new, plate_superC_new, by = "well")
longC_super_new <- joinC_super_new %>% gather(meas, value, mVenus:mScarlet_g7, factor_key=TRUE) %>% na.omit()
#ExpD_super
joinD_super <- left_join(expD_super, plate_superD, by = "well")
longD_super <- joinD_super %>% gather(meas, value, mVenus:mScarlet_g7, factor_key=TRUE) %>% na.omit()
#ExpD_super_new
joinD_super_new <- left_join(expD_super_new, plate_superA_new, by = "well")
longD_super_new <- joinD_super_new %>% gather(meas, value, mVenus:mScarlet_g7, factor_key=TRUE) %>% na.omit()
########

#Now, need to add sample information by joining with a table decoding reactor contents. also need to average technical replicates (a and b)
plate_code<- read_excel(here::here("data", "plate_code_20240506.xlsx"))
plate_code_bc<- read_excel(here::here("data", "plate_code_bc_20240506.xlsx"))
#individual experiments
expA_code<- read_excel(here::here("data", "expA_code.xlsx"))
expB_code<- read_excel(here::here("data", "expB_code.xlsx"))
expC_code<- read_excel(here::here("data", "expC_code.xlsx"))
expD_code<- read_excel(here::here("data", "expD_code.xlsx"))
  
longA<- longA %>% left_join(plate_code, by = "sample") %>% left_join(expA_code, by = "reactor")
longA_new<- longA_new %>% left_join(plate_code, by = "sample") %>% left_join(expA_code, by = "reactor")
longB<- longB %>% left_join(plate_code_bc, by = "sample") %>% left_join(expB_code, by = "reactor")
longB_new<- longB_new %>% left_join(plate_code_bc, by = "sample") %>% left_join(expB_code, by = "reactor")
longC<- longC %>% left_join(plate_code_bc, by = "sample") %>% left_join(expC_code, by = "reactor")
longC_new<- longC_new %>% left_join(plate_code_bc, by = "sample") %>% left_join(expC_code, by = "reactor")
longD<- longD %>% left_join(plate_code, by = "sample") %>% left_join(expD_code, by = "reactor")
longD_new<- longD_new %>% left_join(plate_code, by = "sample") %>% left_join(expD_code, by = "reactor")
#for supers
longA_super<- longA_super %>% left_join(plate_code, by = "sample") %>% left_join(expA_code, by = "reactor")
longA_super_new<- longA_super_new %>% left_join(plate_code, by = "sample") %>% left_join(expA_code, by = "reactor")
longB_super<- longB_super %>% left_join(plate_code_bc, by = "sample") %>% left_join(expB_code, by = "reactor")
longB_super_new<- longB_super_new %>% left_join(plate_code_bc, by = "sample") %>% left_join(expB_code, by = "reactor")
longC_super<- longC_super %>% left_join(plate_code_bc, by = "sample") %>% left_join(expC_code, by = "reactor")
longC_super_new<- longC_super_new %>% left_join(plate_code_bc, by = "sample") %>% left_join(expC_code, by = "reactor")
longD_super<- longD_super %>% left_join(plate_code, by = "sample") %>% left_join(expD_code, by = "reactor")
longD_super_new<- longD_super_new %>% left_join(plate_code, by = "sample") %>% left_join(expD_code, by = "reactor")

##Now, want to combine data between experiments
A_all<- longA %>% full_join(longA_super) %>% full_join(longA_super_new) %>% full_join(longA_new)
A_all$exp <- "a"
B_all<- longB %>% full_join(longB_super) %>% full_join(longB_super_new) %>% full_join(longB_new)
B_all$exp <- "b"
C_all<- longC %>% full_join(longC_super) %>% full_join(longC_super_new) %>% full_join(longC_new)
C_all$exp <- "c"
D_all<- longD %>% full_join(longD_super) %>% full_join(longD_super_new) %>% full_join(longD_new)
D_all$exp <- "d"

all_exp<- A_all %>% full_join(B_all) %>% full_join(C_all) %>% full_join(D_all)
all_sample<- all_exp %>% drop_na(reactor)
write_xlsx(all_sample, here::here("data", "all_sample.xlsx"))

is.na(B_all$reactor) %>% sum() #only 60 are NAs
A_sample<- A_all %>% drop_na(reactor)
B_sample<- B_all %>% drop_na(reactor)
C_sample<- C_all %>% drop_na(reactor)
D_sample<- D_all %>% drop_na(reactor)

data_location2 <- here::here("data","pilotdata.xlsx")
write_xlsx(A_sample, here::here("data", "A_text.xlsx"))
write_xlsx(B_sample, here::here("data","B_test.xlsx"))
write_xlsx(C_sample, here::here("data","C_test.xlsx"))
write_xlsx(D_sample, here::here("data","D_test.xlsx"))
#try to plot longB
ggplot(data = B_sample, aes(x = jitter(day, 0.3), y = value, color = strain, shape = meas)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("G7 In Supernatant Over Time") + facet_wrap(~sample_type + encapsulation_treat)

#this data layout allows me to evaluate data roughly in JMP. May need to do more subsetting later and definitely need to figure out best visualization

#now, need code to combine plate count data with RFUs for comparison
#load data
long_plate <- read_excel(here::here("data", "longterm_platecounts.xlsx"))
mix <- all_exp %>% filter(sample_type == "mixsup")
plate_join<- long_plate %>% 
  left_join(mix, by = c("exp", "day", "reactor")) 
big_join <- long_plate %>% 
  left_join(all_exp, by = c("exp", "day", "reactor")) 
write_xlsx(big_join, here::here("data", "big_join.xlsx"))
ggplot(data = plate_join, aes(x = value, y = cfu, color = meas)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("RFU") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type + meas) + geom_abline(aes(intercept = 2e+6, slope = 60016)) + 
  geom_abline(aes(intercept = -2e+7, slope = 33585)) +
  geom_smooth(method='lm')
#+ xlim (0, 12500) + ylim(2e7, 8e8)
#this plot looks less crazy when axis is expanding..really these are all similar samples at the same time point, so it's
#like looking at a really zoomed in plot. hopefully linear standard curve will appear when more diverse data points are added. 
#really, it's good that all of these samples from the same day (14) are in a similar range of cfu/mL??

#with the lines from the standard curve, these data points actually look a little better. wonder if I could get an r squared value to see how
#well these points fit those lines. when I put in excel, line of best fit is actually negative...that's not good. 
#looking at this plot again on 6/5/24 - points and line of best fit for mScarlet are actually not too far off from the 
#standard curve! this could actually be working??
ggplot(data = big_join, aes(x = value, y = cfu, color = meas)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("RFU") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type + meas) + 
  geom_smooth(method='lm')
#here, see best relationship between dissolved RFU and CFU 
cap<- big_join %>% filter(sample_type == c("capsule", "dissolved"))
ggplot(data = cap, aes(x = value, y = cfu, color = meas)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("RFU") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type + meas) + geom_smooth(method='lm')

ggplot(data = big_join, aes(x = day, y = cfu, color = meas)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("Day") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("CFU Values Over Time") + geom_smooth() + facet_wrap(~encapsulation_treat)

ggplot(data = all_sample, aes(x = day, y = value, color = meas)) +
  geom_boxplot()+
  theme_classic() + 
  xlab("RFU") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type)

samples<- all_sample %>% filter(strain != "blank") %>% filter(sample_type != "super")
super<- samples %>% filter(sample_type=="mixsup")
ggplot(data = super, aes(x = factor(day), y = value, color = meas)) +
  geom_boxplot()+
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type +encapsulation_treat)

btest<- B_all %>% filter(strain != "blank") %>% filter(sample_type != "super") %>% filter(meas == "mVenus")
ggplot(data = btest, aes(x = factor(day), y = value, color = encapsulation_treat)) +
  geom_point()+
  stat_summary(fun = mean, 
               fun.min = function(x) mean(x) - sd(x)/sqrt(length(x)), 
               fun.max = function(x) mean(x) + sd(x)/sqrt(length(x)),
               geom = 'errorbar',  width = 0.25) +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = 'errorbar',  width = 0.5) +
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type)

#widen data
test <- spread(all_sample, sample_type, value)
test <- all_sample %>% select(!well) %>% select(!OD600) %>% select(!sample) %>% pivot_wider(names_from = sample_type, values_from = value)

#maybe I can try to separate and then rejoin the data 
#separate into sample types
cap<- all_sample %>% filter(sample_type == "capsule") %>% spread(sample_type, value)
dis<- all_sample %>% filter(sample_type == "dissolved") %>% spread(sample_type, value)
mixsup<- all_sample %>% filter(sample_type == "mixsup") %>% spread(sample_type, value)

jointest<- cap %>% left_join(dis, join_by(day, meas, reactor, tech_rep, strain, 
                                          encapsulation_treat, coating, bio_rep, exp, cell_loading))
jointest<- jointest %>% left_join(mixsup, join_by(day, meas, reactor, tech_rep, strain, 
                                                  encapsulation_treat, coating, bio_rep, exp, cell_loading))
#this worked!
ggplot(data = jointest, aes(x = capsule, y = dissolved, color = encapsulation_treat)) +
  geom_point() +
  theme_classic() + 
  xlab("Capsule RFU") + 
  ylab("Dissolved RFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Capsule vs Dissolved RFU")

#does relationship depend on day? 
ggplot(data = jointest, aes(x = capsule, y = dissolved, color = as.factor(day))) +
  geom_point() +
  theme_classic() + 
  xlab("Capsule RFU") + 
  ylab("Dissolved RFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Capsule vs Dissolved RFU Colored by Day")

ggplot(data = jointest, aes(x = dissolved, y = capsule, color = encapsulation_treat)) +
  geom_point() +
  theme_classic() + 
  xlab("Dissolved RFU") + 
  ylab("Capsule RFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Capsule vs Dissolved RFU")

jointest_sample<- jointest %>% filter(strain != "blank")
ggplot(data = jointest_sample, aes(x = capsule, y = mixsup, color = coating)) +
  geom_point() +
  theme_classic() + 
  xlab("Caspsule RFU") + 
  ylab("Supernatant RFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Capsule vs Supernatant RFU") + facet_wrap(~encapsulation_treat+strain) +
  geom_smooth(method='lm', )
#could be interesting result here: while RFU in F199 capsules increased, this did not result in an increase of RFU in the supernatant (as opposed to G7, which saw greater growth in supernatant)

g7 <- big_join %>% filter(strain == "G7") %>% filter(sample_type != "super")
#comparing exp. data points to standard curve lines of best fit
ggplot(data = g7, aes(x = value, y = cfu, color = as.factor(day))) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("RFU") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type + meas, ncol = 2) + geom_abline(aes(intercept = -1e+8, slope = 42602)) + 
  geom_abline(aes(intercept = -1e+8, slope = 39569)) + xlim (0, 40000) + ylim(2e7, 1e9)
#putting these figures on the same scale as the standard curves shows that the points cluster along the std curve
#while the small # of data points may create a different trend line, when you zoom out these points make sense (we are just looking at very similar values and therefore seeing artifacts of that)

a_sup<- A_sample %>% filter(sample_type == "mixsup") %>% filter(strain != "blank")
f199<- a_sup %>% filter(strain == "F199")
#trying to look at ExpA supernatant
ggplot(data = a_sup, aes(x = day, y = value, color = encapsulation_treat)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("Day") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time in Exp A") +facet_wrap(~sample_type + strain + meas)
#F199 CFU counts seem to be higher in encapsulated sysmems compared to planktonic, however RFUs are not reflecting that
#perhaps there is fluorescence quenching? however, this did not show up in standard curve. need to look back at that and compare. 

#looking just at f199 on smaller scale
ggplot(data = f199, aes(x = day, y = value, color = encapsulation_treat)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("Day") + 
  ylab("RFU Value")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time for F199 in MixSup") +facet_wrap(~sample_type + meas)

#import final plate counts
final_plate <- read_excel(here::here("data", "finalplatecounts.xlsx"))
all_mixsup <- all_exp %>% filter(sample_type == "mixsup")
final_join <- final_plate %>% 
  left_join(all_mixsup, by = c("exp", "day", "reactor")) 
ggplot(data = final_join, aes(x = value, y = avg_cfu, color = meas)) +
  geom_point(stat='identity') +
  theme_classic() + 
  xlab("RFU") + 
  ylab("CFU")+ 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("RFU Values Over Time") +facet_wrap(~sample_type + meas) + geom_abline(aes(intercept = 2e+6, slope = 60016)) + 
  geom_abline(aes(intercept = -2e+7, slope = 33585)) +
  geom_smooth(method='lm')
write_xlsx(final_join, here::here("data","final_join.xlsx"))

all_plate<- 

#need to create new variable for supernatant
#starting with B_sample
B_14<- subset(B_sample, day <=14 & sample_type=="super") 
B_rest<- subset(B_sample, day >14 & sample_type=="mixsup")
B_14<- B_14 %>% select(!sample_type) %>% mutate(sample_type = "supernatant")
B_rest<- B_rest %>% select(!sample_type) %>% mutate(sample_type = "supernatant")
B_super<- B_14 %>% full_join(B_rest) 
B_other <- B_sample %>% filter(sample_type == c("capsule", "dissolved"))
B_sample_clean<- B_other %>% full_join(B_super)
write_xlsx(B_sample_clean, here::here("data","B_test_clean.xlsx"))
