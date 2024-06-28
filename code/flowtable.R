library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
#rearranging data frame for biostat class

#path to data
data_location <- here::here("data","pilotdata.xlsx")
data <- read_excel(data_location)

#keep mScarlet only
data <- data %>% filter(meas == "mScarlet")

summ <- data %>% group_by(day, reactor, type, coating, strain, treatment, biorep) %>% summarize(avg_rfu = mean(value), 
                                                                                     sd = sd(value))
summ <- summ %>% dplyr::select(-biorep)
summ<- ungroup(summ)
#add reactor numbers
test <- summ %>% mutate(reactor_no = case_when(endsWith(summ$reactor, "s1") ~ "1",
                                           endsWith(summ$reactor, "b1") ~ "1",
                                           endsWith(summ$reactor, "b2") ~ "2", 
                                           endsWith(summ$reactor, "s2") ~ "2", 
                                           endsWith(summ$reactor, "3") ~ "3", 
                                           endsWith(summ$reactor, "4") ~ "4", 
                                           endsWith(summ$reactor, "5") ~ "5", 
                                           endsWith(summ$reactor, "6") ~ "6", 
                                           endsWith(summ$reactor, "7") ~ "7", 
                                           endsWith(summ$reactor, "8") ~ "8", 
                                           endsWith(summ$reactor, "9") ~ "9", 
                                           endsWith(summ$reactor, "10") ~ "10", 
                                           endsWith(summ$reactor, "11") ~ "11", 
                                           endsWith(summ$reactor, "12") ~ "12"))
test$reactor_no <- as.numeric(test$reactor_no)
test <- test %>% dplyr::select(-sd)
test <- test %>% dplyr::select(-reactor)
data_wide <- spread(test, day, avg_rfu)

#need to make microcapsule and supernatant values wide and not long, but keep days long
wide <- spread(test, type, avg_rfu)
#this is what I'll work with, so let's export it. 
library("writexl")
data_location4 <- here::here("data","pilotwide.xlsx")
write_xlsx(wide, data_location4)

ggplot(wide, aes(factor(day), supernatant)) + 
  geom_boxplot() + facet_wrap(~reactor_no, scale = "free")

#attempting to use boxplot to look at data quality and outliers. However, difficult to ID
#because n is so slow. For n<4, we don't get whiskers so we can't ID asymmetrical outliers
# definitely something to keep in mind for future experiments; to look at something with higher N, could look at experiments from last summer (thought probably not *real* biological replicates)

if(!require(psych)){install.packages("psych")}
if(!require(nlme)){install.packages("nlme")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(rcompanion)){install.packages("rcompanion")}

#blanks vs. treatment
ggplot(wide, aes(factor(day), supernatant)) + 
  geom_boxplot() + facet_wrap(~strain)

#separate treatment groups from blanks
g7 <- wide %>% filter(strain == "G7")

#boxplots over time for supernatant
ggplot(g7, aes(factor(day), supernatant)) + 
  geom_boxplot() + facet_wrap(~coating)

#boxplots over time for microcapsule
capsule <- g7 %>% filter(treatment == "capsule")
ggplot(capsule, aes(factor(day), microcapsule)) + 
  geom_boxplot() + facet_wrap(~coating)

#supernatant stats
group_by(g7, coating) %>% 
  summarise(mean = mean(supernatant, na.rm = TRUE),
                                    sd = sd(supernatant, na.rm = TRUE))
#microcapsule stats
group_by(capsule, coating) %>% 
  summarise(mean = mean(microcapsule, na.rm = TRUE),
            sd = sd(microcapsule, na.rm = TRUE))

library(tidyverse)
library(ggpubr)
library(rstatix)

g7 %>% group_by(day, coating) %>% 
  summarise(mean = mean(supernatant, na.rm = TRUE),
            sd = sd(supernatant, na.rm = TRUE))
#plot leevl colored by treatment group (supernatant)
ggplot(g7, aes(factor(day), supernatant, color = coating)) + 
  geom_boxplot()
#plot level colored by coating (microcapsule)
ggplot(capsule, aes(factor(day), microcapsule, color = coating)) + 
  geom_boxplot()

#check assumptions
g7 %>% group_by(coating, day) %>% identify_outliers(supernatant) #no outliers
g7 %>% group_by(coating, day) %>% identify_outliers(microcapsule) #no outliers

#normality test
g7 %>% shapiro_test(supernatant)
g7 %>% group_by(day) %>% shapiro_test(supernatant) #grouping by day and coating means sample size is less than 3 and test can't be run
#data is normally distributed p>0.05 at each time point except day 0
g7 %>% group_by(coating) %>% shapiro_test(supernatant) #normal for each group

capsule %>% group_by(coating) %>% shapiro_test(microcapsule) #no coating group is not normal
capsule %>% group_by(day) %>% shapiro_test(microcapsule) #all groups are normal except day 14

#QQ plot
ggqqplot(g7, "supernatant", ggtheme = theme_bw())+ facet_grid(day ~ coating, labeller = "label_both")

all(is.na(capsule$coating)) #checks for NAs...there are none
#using example from online book
#res.aov <- anova_test(
  #data = capsule, dv = microcapsule, wid = reactor_no, 
  #within = c(coating, day))
#get_anova_table(res.aov) #gives 0 non-NA error

#two-way anova?
anova_test(data = capsule, dv=microcapsule, between=c(coating, day))
anova_test(data = g7, dv=supernatant, between=c(treatment, day))
#trying repeated measures
anova_test(data=g7, dv=supernatant, wid=reactor_no, between=c("coating", "day"))
anova_test(data=capsule, dv=microcapsule, wid=reactor_no, between=c("coating", "day"))
#renaming to see if variable name is causing issue
g7 <- g7 %>% rename(reactor = reactor_no)
#anova_test(data=g7, dv=supernatant, wid=reactor, within=c("coating", "day"))
glimpse(g7)

#separate within and between
#anova_test(data=g7, dv=supernatant, wid=reactor, within="day", between="coating") #getting contrast error

#drop variables not included in model
g7_mod <- g7 %>% dplyr::select(day, coating, reactor, supernatant)
anova_test(data=g7_mod, dv=supernatant, wid=reactor, within="day", between="coating") 
#sig effect of coating, day, and coating*day
#anova_test(data=g7_mod, dv=supernatant, wid=reactor, within=c("day","coating") ) #this still doesn't work

#THIS WORKS!
capsule<- capsule %>% rename(reactor=reactor_no)
cap_sup<- capsule %>% dplyr::select(day, coating, reactor, microcapsule, supernatant) %>% na.omit()
capsule_mod <- capsule %>% dplyr::select(day, coating, reactor, microcapsule)
anova_test(data=capsule_mod, dv=microcapsule, wid=reactor, within="day", between="coating") 
#sig effect of day on bacteria level in capsule (no effect of coating)

#does microcapsule concentration impact supernatant?
anova_test(data=cap_sup, dv=supernatant, wid=reactor, within=c("day","microcapsule"), between="coating") #think this is not working because microcapsule is numerical and R can't "divide" into groups based on those values
#need a different statistical test to answer this question

ggplot(data = cap_sup, aes(x = microcapsule, y = supernatant, color = coating)) +
  geom_point() + theme_classic() + xlab("Microcapsule Concentration") + ylab("Supernatant Concentration") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))

hist(cap_sup$supernatant) #does not look normal. use spearman rank correlation? 
shapiro_test(cap_sup$supernatant) #not normal
shapiro_test(cap_sup$microcapsule) #not normal

hist(log(cap_sup$supernatant))
shapiro_test(log10(cap_sup$supernatant)) #not normal

result<- cor(cap_sup$microcapsule, cap_sup$supernatant, method = "spearman")
result

result2<- cor.test(cap_sup$microcapsule, cap_sup$supernatant, method = "spearman")
result2 #get same answer but this gives you the p-value! 
#strong relationship between concentration of microcapsule and concentration of supernatant

#day vs concentration, show capsule and supernatant
cap_sup_long<- gather(cap_sup, meas, value, microcapsule:supernatant, factor_key=TRUE)
ggplot(data = cap_sup_long) +
  geom_boxplot(aes(x=day, y=value, group=interaction(day, meas), fill=factor(cap_sup_long$meas))) + theme_classic() + xlab("Day") + ylab("Concentration") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + facet_wrap(~coating)












#trying ezANOVA
library(ez)
newModel<- ezANOVA(data = capsule, dv =microcapsule, wid=reactor_no, within=c("coating", "day")) #still getting error

#got results here but don't really understand what they mean
library(nlme)
model = gls(supernatant ~ coating + day + coating*day, 
            correlation = corAR1(form = ~ day | reactor_no, 
                                 value = 0.4287), 
            data=g7, 
            method="REML")
library(car)
Anova(model)

model2 = gls(microcapsule ~ coating + day + coating*day, 
            correlation = corAR1(form = ~ day | reactor_no, 
                                 value = 0.4287), 
            data=capsule, 
            method="REML")
