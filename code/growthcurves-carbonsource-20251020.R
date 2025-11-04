library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(writexl)
library(ggpubr)
#path to data
gc <- here::here("data","AMF-alg-6-7days-20251010.xlsx")

#load data. 
gc <- read_excel(gc)

#take a look at the data
glimpse(gc)

data<- gc %>% slice(2:49) #removes temperature data and sentence at bottom of Excel file
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



#use plate layout to insert treatment names
#path to data
data_location4 <- here::here("data","alg-6-7days-20251010-PL.xlsx")

#load data. 
plate <- read_excel(data_location4)
plate$well <- paste(plate$row, plate$column, sep="") 
plate <- plate %>% select(3:5)

#join names to sample data file
join2 <- left_join(plate2, plate, by = "well")
df<-join2


control<- subset(df, strain == "blank")  
test<- subset(df, strain != "blank")


#plot first 
ggplot(data = df, aes(x = time_h, y = OD600, color = well)) +
  geom_point() + 
  #geom_smooth(se = FALSE) + 
  theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Carbon Source Growth Curves")+
  facet_wrap(~strain+media)

#sRB glucose is contaminated; but no other media type is. And we really don't need sRB15 glucose, since pyr worked in this assay. 
df_clean <- df %>% filter(media != "sRB15.glucose")
ggplot(data = df_clean, aes(x = time_h, y = OD600, color = strain)) +
  geom_point() + 
  #geom_smooth(se = FALSE) + 
  theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Carbon Source Growth Curves")+
  facet_wrap(~media)

#averages
ggplot(data = df_clean, aes(x = time_h, y = OD600, color = strain)) +
  stat_summary(geom="line", fun = mean) +
  stat_summary(geom = "errorbar", width = .1, position = position_dodge(0.8))+
  theme_pubr() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Carbon Source Growth Curves")+
  facet_wrap(~media)

ggplot(data = df_clean, aes(x = time_h, y = OD600, color = strain, linetype = strain)) +
  # Mean line
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  
  # Shaded error band (mean ± sd or se; here I used mean_se)
  stat_summary(fun.data = mean_se, geom = "ribbon", 
               alpha = 0.2, color = NA, fill = "grey70") +
  theme_pubr() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Carbon Source Growth Curves")+
  facet_wrap(~media)

#blank correction
# Calculate mean OD600 at time zero for each media
time0_means <- df_clean %>%
  filter(time_h == 0) %>%  # assuming your time variable is called "Time"
  group_by(media) %>%
  summarise(initial.all = mean(OD600, na.rm = TRUE))

# Join these initial values back to dataset
df_clean <- df_clean %>%
  left_join(time0_means, by = "media") %>%
  mutate(OD600_corrected = OD600 - initial.all)
df_clean[df_clean < 0 ] <- 0 #convert negative values to zero

ggplot(data = df_clean, aes(x = time_h, y = OD600_corrected, color = strain, linetype = strain)) +
  # Mean line
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  
  # Shaded error band (mean ± sd or se; here I used mean_se)
  stat_summary(fun.data = mean_se, geom = "ribbon", 
               alpha = 0.2, color = NA, fill = "grey70") +
  theme_pubr() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Carbon Source Growth Curves")+
  facet_wrap(~media)

# custom facet labels
facet_labels <- c(
  "sRB15.alg"      = "sRB15 + alginate",
  "sRB15.chit"     = "sRB15 + chitosan",
  "sRB15.noC"      = "sRB15 + no carbon",
  "sRB15.pyruvate" = "sRB15 + pyruvate",
  "LB"             = "LB"            # include LB if present
)

# colors & linetypes (edit colors/linetypes to your preference)
my_colors <- c("blank" = "#e41a1c", "n.aroma" = "#4daf4a", "p.putida" = "#377eb8")
my_linetypes <- c("blank" = "solid", "n.aroma" = "dashed", "p.putida" = "dotted")

#levels
df_clean <- df_clean %>%
  mutate(media = factor(media, levels = c("sRB15.noC", "sRB15.alg", "sRB15.chit", "sRB15.pyruvate", "LB")))

# plot — 
carbonsource.gc<- ggplot(data = df_clean, aes(x = time_h, y = OD600_corrected, color = strain, linetype = strain)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA, fill = "grey70") +
  theme_pubr() +
  xlab("Time (hr)") + ylab("OD600") +
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold"),
        #plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "bottom") +
  #ggtitle("Carbon Source Growth Curves") +
  facet_wrap(~media, labeller = labeller(media = facet_labels)) +
  scale_color_manual(
    name = "Bacteria",
    breaks = c("blank", "n.aroma", "p.putida"),
    values = my_colors,
    labels = list(
      "blank"    = "Blank",
      "n.aroma"  = expression(italic("N. aromaticivorans")),
      "p.putida" = expression(italic("P. putida"))
    )
  ) +
  scale_linetype_manual(
    breaks = c("blank", "n.aroma", "p.putida"),
    values = my_linetypes,
    labels = c("Blank", expression(italic("N. aromaticivorans")), expression(italic("P. putida")))
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linetype = my_linetypes,
                          color = my_colors,
                          size = 2.5), 
      keywidth = 3, 
      keyheight = 1.2
    ),
    linetype = "none"   # hide the separate linetype legend
  ) +
  theme(legend.position = "top")
carbonsource.gc
ggsave(here("results", "carbon-source-gc.png"), carbonsource.gc, width = 7.5, height = 7, units = "in")

