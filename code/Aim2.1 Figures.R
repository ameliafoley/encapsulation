##run this AFTER Aim2.1_platecounts.R - depends on dataframes created there. 

clean_2<- clean1 %>% group_by(rx, sample_type, strain, day) %>% mutate(
  day = ifelse(rx== "encapsulated" & sample_type == "cap", day -1, day)
) %>% mutate(
  day = ifelse(rx== "free" , day +1, day)
)

colnames(clean_2)
test<- unique(clean_2[,-c(5, 8)])
test1<- test %>% mutate(
  day = rep(10.5, length(strain)),
  avg_cfu = rep(NA, length(strain))
)
test2<- test %>% mutate(
  day = rep(31.5, length(strain)),
  avg_cfu = rep(NA, length(strain))
)

clean_2<- rbind.data.frame(clean_2, test1, test2)
unique(clean_2$day)
clean_2$day<-  factor(as.character(clean_2$day), levels = c("-1", "0", "1", "10.5", "20", "21", "22","31.5", "41", "42", "43"))
glimpse(clean_2)
bacteria_names <- list(
  'a.venet'="A. venetianus",
  'h.taenio'="H. taeniospiralis",
  'm.fred'="M. fredericksbergense",
  'n.aroma'="N. aromaticivorans", 
  'n.penta' = 'N. pentaromativorans', 
  'p.putida' = 'P. putida'
)
bacteria_labeller<- function(variable,value){
  return(bacteria_names[value])}
nbreaks <- 10
breaks <- 10^(nbreaks:1)
breaks <- c(0,breaks)

bac_label = c('a.venet'="A. venetianus",
              'h.taenio'="H. taeniospiralis",
              'm.fred'="M. fredericksbergense",
              'n.aroma'="N. aromaticivorans", 
              'n.penta' = 'N. pentaromativorans', 
              'p.putida' = 'P. putida')

bacteria_labeller2<- bacteria_names

ggplot(clean_2, aes(day, avg_cfu, pattern = sample_type)) +
  stat_summary(aes(fill = rx), 
               geom = "bar_pattern", 
               color= "black",
               pattern_fill="black", 
               pattern_density = .03, 
               width = 1,
               pattern_spacing = .06,
               pattern_key_scale_factor = .2,
               position = position_dodge(width=1, preserve = "single"))+
  stat_summary(geom = "errorbar", width = .25, position = position_dodge(width=1, preserve = "single"), size = .8,
               color = "black")+
  theme_pubr()+
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), 
                     breaks=c(1e0, 1e2, 1e4, 1e6, 1e8, 1e10, 1e12), 
                     #breaks = trans_breaks('log10', function(x) 10^x, c(0, 1e9), n = 9), 
                     labels = trans_format('log10', math_format(10^.x)))+
  scale_x_discrete(breaks = c("0", "21", "42"))+
  facet_wrap(~strain, strip.position = "bottom", scales = "free_x", labeller = as_labeller(bac_label))+
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside")+
ggtitle("Concentrations over Time")+
  xlab("Days") + 
  ylab("CFU/mL")+
    ggeasy::easy_labs()+
  scale_pattern_manual(
    name = "Sample Type",
    values = c(
      "cap" = "stripe",
      "super" = "none",
      "encapsulated" = "none", 
      "free" = "none"), 
    guide = guide_legend(override.aes = list(fill = "whitesmoke")))+
  scale_fill_manual(
    name = "Treatment",
    values = c("encapsulated" = "salmon2", "free" = "cadetblue1"), 
    guide = guide_legend(override.aes = list(pattern_alpha = 0))) +
  theme(strip.text = element_text(face = "italic", size = 12))
  
 
##change axis scale
ggplot(clean_2, aes(day, avg_cfu, pattern = sample_type)) +
  stat_summary(aes(fill = rx), 
               geom = "bar_pattern", 
               color= "black",
               pattern_fill="black", 
               pattern_density = .03, 
               width = 1,
               pattern_spacing = .06,
               pattern_key_scale_factor = .2,
               position = position_dodge(width=1, preserve = "single"))+
  stat_summary(geom = "errorbar", width = .25, position = position_dodge(width=1, preserve = "single"), size = .8,
               color = "black")+
  theme_pubr()+
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))+
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), 
                     breaks=c(1e0, 1e2, 1e4, 1e6, 1e8, 1e10, 1e12), 
                     #breaks = trans_breaks('log10', function(x) 10^x, c(0, 1e9), n = 9), 
                     labels = trans_format('log10', math_format(10^.x)))+
  scale_x_discrete(breaks = c("0", "21", "42"))+
  facet_wrap(~strain, strip.position = "bottom", scales = "free_x", labeller = as_labeller(bac_label))+
  theme(panel.spacing = unit(.6, "lines"), #increased panel spacing for more breaks between strains
        strip.background = element_blank(),
        strip.placement = "outside")+
  ggtitle("Concentrations over Time")+
  xlab("Days") + 
  ylab("CFU/mL")+
  ggeasy::easy_labs()+
  scale_pattern_manual(
    name = "Sample Type",
    values = c(
      "cap" = "stripe",
      "super" = "none",
      "encapsulated" = "none", 
      "free" = "none"), 
    guide = guide_legend(override.aes = list(fill = "whitesmoke")))+
  scale_fill_manual(
    name = "Treatment",
    values = c("encapsulated" = "salmon2", "free" = "cadetblue1"), 
    guide = guide_legend(override.aes = list(pattern_alpha = 0))) +
  theme(strip.text = element_text(face = "italic", size = 12))+
  coord_cartesian (ylim=c(1e3, 1e10)) 


