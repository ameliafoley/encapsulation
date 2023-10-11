
library(dplyr)
df_type<- read_csv("df_type.csv")
df_type$control<- ifelse(df_type$sample == "bb_f_22", 1, 0)
df_type$control<- ifelse(df_type$sample == "LB_2", 1, df_type$control)

control<- subset(df_type, control == 1)
control$control<- NULL
test<- subset(df_type, control == 0)
test$control<- NULL


colnames(control)<- c("well_control", "OD600_control", "time_h", "sample_control", "type")
tmp<- merge(control, test, by = c("time_h", "type"))
tmp$corrected<- tmp$OD600 - tmp$OD600_control


write_csv(tmp, "Blank Subtraction 17_18.csv")


beads<- subset(graph_data, type == "bead")
free<- subset(graph_data, type == "free")
library(ggplot2)

graph_data<- subset(tmp, corrected >=0)
ggplot(data = beads, aes(x = time_h, y = corrected, color = sample)) +
  geom_point() + geom_smooth(se = FALSE) + theme_classic() + xlab("Time") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))
ggplot(data = free, aes(x = time_h, y = corrected, color = sample)) +
  geom_point() + geom_smooth() + theme_classic() + xlab("Time") + ylab("OD600 (Corrected)") + 
  theme(panel.grid.minor.y = element_line(color = "grey", linetype = "dashed"))



