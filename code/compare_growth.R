library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)
library(rstatix)
#path to data
data_location <- here::here("data","odwells.xlsx")
data_location2 <- here::here("data","odwells_six.xlsx")
#load data. 
data <- read_excel(data_location)
six <- read_excel(data_location2)

#library(MASS)
#b <- boxcox(lm(test_blank ~ 1, data = six))
# Exact lambda
#lambda <- b$x[which.max(b$y)]
#lambda
ggplot(data = six, aes(x = time_h.x, y = blank_cor, color = strain, shape = type)) +
  geom_point() + theme_classic() + xlab("Time (hr)") + ylab("Avg OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"),
        axis.text.x=element_text(angle = 0, hjust = 0, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 14)) +ggtitle("OD600 for Rep2, F199, & G7 Systems")+
  facet_wrap(~ type)+
  scale_color_brewer(palette="Set2")

six %>% filter(strain == "rep2", type == "alginate_bead") %>% dplyr::select(well) %>% unique()

glimpse(data)
data_minus<- data %>% dplyr::select(time_h.x, strain, type, sample, blank_cor)
#anova_test(data=data_minus, dv=blank_cor, wid=sample, within="time_h.x", between=c("strain", "type")) #gives NaN

glimpse(six)
six_minus<- six %>% dplyr::select(time_h.x, strain, type, sample, blank_cor, well)
anova_test(data=six_minus, dv=blank_cor, wid=well, within="time_h.x", between=c("strain", "type"))
#this works! 3 factor repeated measures anova 

#THURSDAY: drop Rep2, truncate by time, make sure times are the same, try again
#think not running because there aren't "complete cases" for all time points due to truncation

#since this data(six_minus) works, let's subdivide it and try again
div<- six_minus %>% dplyr::filter(blank_cor>0.1)
anova_test(data=div, dv=blank_cor, wid=well, within="time_h.x", between=c("strain", "type")) #still not working
#only subdivided data, but this anova test does not work. why????? "Error in contrasts"
#let's try removing unused variable, sample
div<- div %>% select(!sample)
anova_test(data=div, dv=blank_cor, wid=well, within="time_h.x", between=c("strain", "type")) #still not working

#data for 3 wells of each type
ggplot(data = six_minus, aes(x = time_h.x, y = blank_cor, color = strain, shape = type)) +
  geom_point() + theme_classic() + xlab("Time (hr)") + ylab("Avg OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"),
        axis.text.x=element_text(angle = 0, hjust = 0, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 14)) +ggtitle("Average OD600 for Rep2, F199, & G7 Systems")+
  facet_wrap(~ type)+
  scale_color_brewer(palette="Set2")

#data for average of 3 groups
ggplot(data = data_minus, aes(x = time_h.x, y = blank_cor, color = strain, shape = type)) +
  geom_point() + theme_classic() + xlab("Time (hr)") + ylab("Avg OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"),
        axis.text.x=element_text(angle = 0, hjust = 0, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 14)) +ggtitle("Average OD600 for Rep2, F199, & G7 Systems")+
  facet_wrap(~ type)+
  scale_color_brewer(palette="Set2")

#trying log
data_minus$plus <- data_minus$blank_cor+1
data_minus$log <- log10(data_minus$plus)
data_minus$sqrt <- data_minus$blank_cor**(1/2)
hist(data_minus$sqrt)
hist(data_minus$log)
#data for average of groups
ggplot(data = data_minus, aes(x = time_h.x, y = blank_cor, color = strain, shape = type)) +
  geom_point() + theme_classic() + xlab("Time (hr)") + ylab("Avg OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"),
        axis.text.x=element_text(angle = 0, hjust = 0, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 14)) +ggtitle("Average OD600 for Rep2, F199, & G7 Systems")+
  facet_wrap(~ type)+
  scale_color_brewer(palette="Set2")
#on log scale
ggplot(data = data_minus, aes(x = time_h.x, y = blank_cor, color = strain, shape = type)) +
  geom_point() + theme_classic() + xlab("Time (hr)") + ylab("Avg OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"),
        axis.text.x=element_text(angle = 0, hjust = 0, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 14)) +ggtitle("Average OD600 for Rep2, F199, & G7 Systems")+
  facet_wrap(~ type)+
  scale_color_brewer(palette="Set2") + scale_y_continuous(trans="log10") #looks bad

#log transformed
ggplot(data = data_minus, aes(x = time_h.x, y = log, color = strain, shape = type)) +
  geom_point() + theme_classic() + xlab("Time (hr)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"),
        axis.text.x=element_text(angle = 0, hjust = 0, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 14)) +ggtitle("Average OD600 for Rep2, F199, & G7 Systems")+
  facet_wrap(~ type)+
  scale_color_brewer(palette="Set2")
#anova on log
data_minus_log <- data_minus %>% dplyr::select(time_h.x, strain, type, sample, log)
anova_test(data=data_minus_log, dv=log, wid=sample, within="time_h.x", between=c("strain", "type"))
#I think we can't do this because there aren't replicates here, and that's why NaN shows up

#adding log to dataframe with 3 wells per group
six_minus$plus <- six_minus$blank_cor+1
six_minus$log <- log10(six_minus$plus)
six_minus$sqrt <- six_minus$blank_cor**(1/2)
hist(six_minus$sqrt)
hist(six_minus$log)

six_minus_log <- six_minus %>% dplyr::select(time_h.x, strain, type, sample, log, well)
#need to keep well for id for this to work
anova_test(data=six_minus_log, dv=log, wid=well, within="time_h.x", between=c("strain", "type"))

glimpse(six_minus_log)

#removing rep2 to reduce zeroes
two<- six_minus %>% filter(strain!= "rep2")
hist(two$blank_cor)
#removing all zeros
nozero<- six_minus %>% filter(blank_cor>0)
hist(nozero$blank_cor)
shapiro_test(nozero$blank_cor) #still not normal
nozero$log10 <- log10(nozero$blank_cor)
hist(nozero$log10)
ggplot(data = nozero, aes(x = time_h.x, y = log10, color = strain, shape = type)) +
  geom_point() + theme_classic() + xlab("Time (hr)") + ylab("Avg OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"),
        axis.text.x=element_text(angle = 0, hjust = 0, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 14)) +ggtitle("Average OD600 for Rep2, F199, & G7 Systems")+
  facet_wrap(~ type)+
  scale_color_brewer(palette="Set2")

#trying truncating at 30 hours
thirty<- six_minus %>% filter(time_h.x<30)
hist(thirty$blank_cor)
ggplot(data = thirty, aes(x = time_h.x, y = log10(blank_cor+1), color = strain, shape = type)) +
  geom_point() + theme_classic() + xlab("Time (hr)") + ylab("Avg OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"),
        axis.text.x=element_text(angle = 0, hjust = 0, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 14)) +ggtitle("Average OD600 for Rep2, F199, & G7 Systems")+
  facet_wrap(~ type)+
  scale_color_brewer(palette="Set2")

#try truncating at .1 OD
one<- six_minus %>% filter(blank_cor>0.1)
hist(one$blank_cor) #this gives us normal looking data! 
ggplot(data = one, aes(x = time_h.x, y = blank_cor, color = strain, shape = type)) +
  geom_point() + theme_classic() + xlab("Time (hr)") + ylab("Avg OD600") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"),
        axis.text.x=element_text(angle = 0, hjust = 0, size = 15),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 14)) +ggtitle("Average OD600 for Rep2, F199, & G7 Systems")+
  facet_wrap(~ type)+
  scale_color_brewer(palette="Set2")
#and we can still distinguish differences in growth curves on the truncated graphs
#what do the statistical tests look like when we perform them on truncated data?
one_clean<- one %>% dplyr::select(time_h.x, strain, type, blank_cor, well)
one_clean$od<- one_clean$blank_cor
one_clean$log<- log10(one_clean$od)
od<- one_clean %>% dplyr::select(time_h.x, strain, type, od, well)
one_log<- one_clean %>% dplyr::select(time_h.x, strain, type, log, well)

class(one_clean$time_h.x)
class(one_clean$type)
class(one_clean$strain)


anova_test(data=od, dv=od, wid=well, within="time_h.x", between=c("type"))
anova_test(data=one_log, dv=log, wid=well, within="time_h.x", between=c("strain","type"))

glimpse(one_clean)
anova_test(data=one_clean, dv=blank_cor, wid=well, within="time_h.x", between=c("strain","type"))

dplyr::ungroup(one_clean)
debug_contr_error <- function (dat, subset_vec = NULL) {
  if (!is.null(subset_vec)) {
    ## step 0
    if (mode(subset_vec) == "logical") {
      if (length(subset_vec) != nrow(dat)) {
        stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
      }
      subset_log_vec <- subset_vec
    } else if (mode(subset_vec) == "numeric") {
      ## check range
      ran <- range(subset_vec)
      if (ran[1] < 1 || ran[2] > nrow(dat)) {
        stop("'numeric' `subset_vec` provided but values are out of bound")
      } else {
        subset_log_vec <- logical(nrow(dat))
        subset_log_vec[as.integer(subset_vec)] <- TRUE
      } 
    } else {
      stop("`subset_vec` must be either 'logical' or 'numeric'")
    }
    dat <- base::subset(dat, subset = subset_log_vec)
  } else {
    ## step 1
    dat <- stats::na.omit(dat)
  }
  if (nrow(dat) == 0L) warning("no complete cases")
  ## step 2
  var_mode <- sapply(dat, mode)
  if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
  var_class <- sapply(dat, class)
  if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
    stop("matrix variables with 'AsIs' class must be 'numeric'")
  }
  ind1 <- which(var_mode %in% c("logical", "character"))
  dat[ind1] <- lapply(dat[ind1], as.factor)
  ## step 3
  fctr <- which(sapply(dat, is.factor))
  if (length(fctr) == 0L) warning("no factor variables to summary")
  ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr
  dat[ind2] <- lapply(dat[ind2], base::droplevels.factor)
  ## step 4
  lev <- lapply(dat[fctr], base::levels.default)
  nl <- lengths(lev)
  ## return
  list(nlevels = nl, levels = lev)
}
debug_contr_error(one_clean)
complete.cases(one_clean) %>% summary()
glimpse(one_clean)
one_clean$well <- as.factor(one_clean$well)
one_clean$type <- as.factor(one_clean$type)
one_clean$strain <- as.factor(one_clean$strain)
one_clean$sample <- as.factor(one_clean$sample)
glimpse(one_clean)

#display unique values for each variable
lapply(one_clean[c('blank_cor', 'time_h.x', 'strain', 'type', 'well')], unique)

str(one_clean) #variables are not factors
one_clean$strain <- factor(one_clean$strain, levels = unique(one_clean$strain))
one_clean$type <- factor(one_clean$type, levels = unique(one_clean$type))
one_clean$well <- factor(one_clean$well, levels = unique(one_clean$well))

one_clean$time_h.x_test <- factor(as.numeric(one_clean$time_h.x), levels = unique(one_clean$time_h.x))


#try again with variables as factors

class(one_clean$strain)
levels(one_clean$strain)
levels(one_clean$type)
anova_test(data=one_clean, dv=blank_cor, wid=well, within=time_h.x_test, between=c(strain, type))

df <- tibble(ID = as.factor(rep(1:6, each = 2)),
             Score = sample(1:15, 12, replace = TRUE),
             Time = as.factor(rep(1:2, times = 6)),
             Group = as.factor(rep(1:2, each = 6)))

one_clean %>% anova_test(blank_cor~time_h.x_test*type + Error())



#testing for normality

six_minus %>% shapiro_test(blank_cor)

anova_test(data=six_minus, dv=blank_cor, wid=sample, within="time_h.x", between=c("strain","type")) #works, gives small p-value

#including well to test for independent sample or technical replicate
notime<- six_minus %>% dplyr::select(!"time_h.x")
anova_test(data=notime, dv=blank_cor, wid=sample, between=c("type", "well")) #can't get this to give me p-values for everything


#trying more within
#anova_test(data=six_minus, dv=blank_cor, wid=sample, within=c("time_h.x","strain","type")) #doesn't work
#anova_test(data=data_minus, dv=blank_cor, wid=sample, within="time_h.x", between=c("strain","type")) #returns NaN

#but, we can't do ANOVA because this data is not normally distributed
#nonparametric alternative to RM ANOVA is Friedman test
library(ggpubr)
ggboxplot(six_minus, x = "time_h.x", y = "blank_cor", add = "jitter")
res.fried <- six_minus %>% friedman_test(blank_cor ~ time_h.x |well)
res.fried
#pairwise comparisons with Wilcoxon signed rank
pwc <- six_minus %>% wilcox_test(blank_cor ~ time_h.x, paired = TRUE, p.adjust.method = "bonferroni")
pwc #think this was comparing each time point - that's not what we want

ggboxplot(six_minus, x = "type", y = "blank_cor", color = "type")
kruskal.test(blank_cor ~ type, data = six_minus)

ggboxplot(six_minus, x = "strain", y = "blank_cor", color = "strain")
kruskal.test(blank_cor ~ strain, data = six_minus)

#we could use the Kolmogorov-Smirnov Test to test whether samples come from same distribution
bead<- six_minus %>% filter(type == "alginate_bead")
free<- six_minus %>% filter(type =="free")
ks.test(bead$blank_cor, free$blank_cor) #distributions are different p<2.2e-16
chitosan<- six_minus %>% filter(type == "chitosan_bead")
ks.test(bead$blank_cor, chitosan$blank_cor)
ks.test(free$blank_cor, chitosan$blank_cor)

#try linearizing with Box Cox (Power) transformation
library(caret)
library(MASS)
bc<- BoxCoxTrans(six_minus$blank_cor) #doesn't work
hist(six_minus$blank_cor)
boxcox(lm(six_minus$blank_cor ~ six_minus$time_h.x))

library(car)
m1<- lm(blank_cor ~ time_h.x, six_minus)
summary(p1 <- powerTransform(m1, family = "bcnPower"))
coef(p1, round=TRUE) #says coef is 0
summary(m2<- lm(bcnPower(blank_cor, p1$roundlam) ~ time_h.x, six_minus))

hist(data_minus$blank_cor)
hist(data_minus$log)


#RM ANOVA example from R companion
library(nlme)
model = lme(blank_cor ~ time_h.x + strain + type + time_h.x*strain + time_h.x*type + strain*type)
########################################################

library(growthcurver)
curvetest<- data_minus %>% filter(strain == "g7") %>% filter(type == "free")
#keep only time and OD
curvetest.<- curvetest %>% select(time_h.x, blank_cor) %>% filter(time_h.x < 30) %>% filter(time_h.x > 4)
glimpse(curvetest.)
ggplot(curvetest., aes(x = time_h.x, y = blank_cor)) + geom_point(alpha=0.7)
model.f199.free <- SummarizeGrowth(curvetest.$time_h.x, curvetest.$blank_cor)
gc_fit <- SummarizeGrowth(curvetest.$time_h.x, curvetest.$blank_cor)
gc_fit
model.f199.free
plot(model.f199.free)
str(model.f199.free)
#can't seem to get Growthcurver to work to give me summary stats for the growth rate

#Define Gompertz function
Gompertz <- function(x, y0, ymax, k, lag){
  result <- y0 + (ymax -y0)*exp(-exp(k*(lag-x)/(ymax-y0) + 1) )
  return(result)
}
jitter_y = jitter(curvetest.$blank_cor, amount=0.2)
Gomp1 <-  nls(jitter_y ~ Gompertz(time_h.x, y0, ymax, k, lag),
              data = curvetest.,
              start = list(y0=0.15, ymax=7, k=0.5, lag=3)) #gives singular gradient error
Gomp1

fit.gompertz <- function(data, time){
  d <- data.frame(y=data, t=time)
  
  # Must have at least 3 datapoints at different times
  if (length(unique(d$t)) < 3) stop("too few data points to fit curve")
  
  # Pick starting values ###
  i <- which.max(diff(d$y))
  starting.values <- c(a=max(d$y), 
                       mu=max(diff(d$y))/(d[i+1,"t"]-d[i, "t"]), 
                       lambda=i)
  print("Starting Values for Optimization: ")
  print(starting.values)
  ##########################
  
  formula.gompertz <- "y~a*exp(-exp(mu*exp(1)/a*(lambda-t)+1))"
  nls(formula.gompertz, d, starting.values)
}
gompertz <- function(time, a, mu, lambda){
  y <- a*exp(-exp(mu*exp(1)/a*(lambda-time)+1))
  return(data.frame(time=time, y=y))
}

d <- gompertz(1:100, 10, 2, 30)
plot(d)
(fit <- fit.gompertz(d$y, d$time))
gompertz <- function(time, a, mu, lambda){
  y <- a*exp(-exp(mu*exp(1)/a*(lambda-time)+1))
  return(data.frame(time=time, y=y))
}

d <- gompertz(1:100, 10, 2, 30)
plot(d)
fit <- fit.gompertz(curvetest$blank_cor, curvetest$time_h.x)
safe.fit.gompertz <- safely(fit.gompertz)
safe.fit.gompertz(curvetest$blank_cor, curvetest$time_h.x)
