library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
#path to data
strains <- here::here("data","aim2_strains_GC_09102024.xlsx")

#load data. 
strains <- read_excel(strains)

#take a look at the data
glimpse(strains)

data<- strains %>% slice(2:97)
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
data_location4 <- here::here("data","aim2_strainscreen_platelayout.xlsx")

#load data. 
plate <- read_excel(data_location4)
plate$well <- paste(plate$row, plate$column, sep="") 
plate <- plate %>% select(3:5)

#join names to sample data file
join2 <- left_join(plate2, plate, by = "well")
df<-join2


control<- subset(df, strain == "blank")  
test<- subset(df, strain != "blank")


#plot first without blank correction
ggplot(data = test, aes(x = time_h, y = OD600, color = strain)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Rep2, F199, and G7 in 48-Well Plate")+
  facet_wrap(~media)

#cut to 90 hours
test<- test %>% filter(time_h<90)
ggplot(data = test, aes(x = time_h, y = OD600, color = strain)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("Rep2, F199, and G7 in 48-Well Plate")+
  facet_wrap(~media)



#take averages of blanks
control<- control %>% group_by(media) %>% summarize(OD600 = mean(OD600))
colnames(control)<- c("media", "OD600_b")
merge<- merge(test, control, by = c("media"))

merge$corrected<- merge$OD600 - merge$OD600_b #blank subtraction
merge[merge < 0 ] <- 0 #convert negative values to zero

ggplot(data = merge, aes(x = time_h, y = corrected, color = strain)) +
  geom_point() + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("PAH-degrading Strain Screening")+
  facet_wrap(~media, ncol = 3)

incomplete <- merge %>% filter(strain!= "p.putida") %>% filter(strain!= "n.aroma") %>% filter(strain!= "n.penta")
ggplot(data = incomplete, aes(x = time_h, y = corrected, color = strain)) +
  geom_point() + theme_classic() + xlab("Time (hr)") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed")) +
  ggtitle("PAH-degrading Strain Screening")+
  facet_wrap(~media, ncol = 3)


##gcplyr
library(gcplyr)
merge <- merge %>% mutate(copy.no = case_when(
  strain == "p.putida" ~ 7,
  strain == "a.venet"  ~ 6,
  strain == "p.resin"  ~ 5,
  strain == "n.penta"  ~ 3,
  strain == "a.faec"    ~ 3,
  strain == "sphingo"  ~ 2,
  TRUE ~ NA_real_
))
growth_dat <- merge %>%
  group_by(well) %>%
  mutate(
    growth_rate = calc_deriv(
      x = time_h,
      y = corrected,
      percapita = TRUE,
      blank = 0,
      window_width_n = 11,
      trans_y = "log"
    )
  ) %>%
  ungroup()
ggplot(growth_dat,
       aes(x = time_h, y = growth_rate, color = strain)) +
  geom_line() +
  labs(
    x = "Time (h)",
    y = "Per-capita growth rate (1/h)"
  ) +
  theme_classic()
growth_summary <- growth_dat %>%
  group_by(strain, media, copy.no, well) %>%
  summarize(
    max_growth_rate = max_gc(growth_rate, na.rm = TRUE),
    max_growth_time = extr_val(
      time_h,
      which_max_gc(growth_rate)
    ),
    doubling_time = doubling_time(max_growth_rate)
  )
ggplot(growth_summary, 
       aes(x = copy.no, y = max_growth_rate, color = strain)) +
  geom_point() + 
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = TRUE,
    color = "black"
  ) +
  labs(
    x = "16S copy number", 
    y = "Max Growth Rate" ) + 
  theme_classic() +
  facet_wrap(~media)

library(dplyr)
library(broom)

regression_results <- growth_summary %>%
  group_by(media) %>%
  group_modify(~ {
    
    model <- lm(max_growth_rate ~ copy.no, data = .x)
    
    coef <- tidy(model) %>%
      filter(term == "copy.no")
    
    fit <- glance(model)
    
    tibble(
      slope = coef$estimate,
      std_error = coef$std.error,
      p_value = coef$p.value,
      r_squared = fit$r.squared,
      adj_r_squared = fit$adj.r.squared
    )
  }) %>%
  ungroup()

regression_results
