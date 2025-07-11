library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(ggplot2)
library("writexl", )
library(ggbreak)
library(ggforce)


#load data. 
filter <- read_excel(here::here("data", "Exp2_Filter copy.xlsx"))


f_label<- c("abiotic" = "Abiotic", 
            "bacteria" = "N. pentaromativorans", 
            "fungi" = "Trichoderma.508")
ggplot(data = filter, aes(x = day, y = ng.l, fill = filtered)) +
  stat_summary(geom="bar", fun = mean, size =1, position = position_dodge(20, preserve = "single")) +
  stat_summary(geom = "errorbar", width = 3, position = position_dodge(20, preserve = "single"), size = .5)+
  theme_pubr() + 
  theme(strip.text = element_text(face = "italic", size = 12))+ #italicize
  #theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  theme(plot.title = element_markdown())+
  #ggtitle("*P. putida* G7")+
  xlab("Treatment + Day") + 
  ylab("Fluoranthene (ng/mL)")+
  facet_wrap(~rx, strip.position = "bottom", labeller = as_labeller(f_label))+
  labs(fill = "Filter")+
  scale_x_continuous(limits=c(-10, 21))
glimpse(filter)
  

sum_fil<- filter %>% group_by(rx, day, filtered) %>% summarise(fluoranthene_mean = mean(ng.l), 
                                                                   fluoranthene_se = sd(ng.l) / sqrt(n()),)

ggplot(data = sum_fil, aes(x = day, y = fluoranthene_mean, fill = filtered)) +
  geom_bar(stat='identity', position=position_dodge(20, preserve = "single")) +
  geom_errorbar(aes(ymin=fluoranthene_mean-fluoranthene_se, ymax=fluoranthene_mean+fluoranthene_se, 
                    group = interaction(sum_fil$rx,sum_fil$filtered)), 
                position=position_dodge(20, preserve = "single"), width = 3, alpha = 1)+
  theme_pubr() + xlab("Day") + ylab("Fluoranthene (ng/mL)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) + 
  facet_wrap(~rx, labeller = as_labeller(f_label), strip.position = "bottom")+
  theme(strip.text = element_text(face = "italic", size = 12), axis.title.y = element_markdown())+
  #guides(fill = "none")+
  scale_x_continuous(breaks=c(0, 21))+
  scale_fill_manual(values = c( "no" = "darkseagreen3",
                                 "yes" = "palevioletred"))


#load data from Experiment 1
exp1 <- read_excel(here::here("data", "Exp1 copy.xlsx"))

sum_exp1<- exp1 %>% group_by(treatment, day) %>% summarise(fluoranthene_mean = mean(ng.ml), 
                                                               fluoranthene_se = sd(ng.ml) / sqrt(n()),)
sum_exp1$day<- sum_exp1$day %>% as.character()
ggplot(data = sum_exp1, aes(x = day, y = fluoranthene_mean, fill = treatment)) +
  geom_bar(stat='identity', position=position_dodge(preserve = "single")) +
  geom_errorbar(aes(ymin=fluoranthene_mean-fluoranthene_se, ymax=fluoranthene_mean+fluoranthene_se, 
                    group = interaction(sum_fil$rx,sum_fil$filtered)), 
                position=position_dodge(.9, preserve = "single"), width = .1, alpha = 1)+
  theme_pubr() + xlab("Day") + ylab("Fluoranthene (ng/mL)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))+
  scale_fill_manual(values = c("abiotic" = "#F8766D",
                                "bacteria" = "mediumturquoise", 
                                "fungi" = "darkseagreen3"), 
                    labels = c("abiotic" = "Abiotic", 
                               "bacteria" = "*N. pentaromativorans*", 
                               "fungi" = "*Trichoderma.508*"))+
  theme(legend.text = ggtext::element_markdown())+
  labs(fill = "Treatment")
                                                   
                                                     
                                                                   
