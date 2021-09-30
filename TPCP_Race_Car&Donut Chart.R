library (dslabs)
library(tidyverse)
install.packages("remotes")
library(ggplot2)
library(remotes)
remotes::install_github("feddelegrand7/ddplot", build_vignettes = TRUE)
library(ddplot)
library(xlsx)

#RaceCarChart
setwd("/Users/vishalsharma/Downloads/Data Analytics/Excel Files for R")
data <- read.xlsx("Product_wise_R_v2.xlsx", sheetIndex = 1)
head(data)
graph <- data %>% mutate(NewDate = as.Date(Date), month = months(NewDate)) %>% filter(Date >= "2021-01-01")
head(graph)
data %>% ggplot(aes(Date, Total, col = Zone)) +geom_point()
data %>% ggplot(aes(Date, Total, col = Zone)) +geom_line()
graph_data <- graph %>% select(month, Material.Code, Total)
barChartRace(graph_data, x = "Total", y = "Material.Code", time = "month", title = "Sales by Zone", frameDur = 2000, colorCategory = "Dark2", panelcol = "White", bgcol = "#DCDCDC")
#DonutChartfor Sales
zone_data <- data %>% group_by(Zone) %>% summarise(sum_total = sum(Total))
zone_data$fraction = zone_data$sum_total / sum(zone_data$sum_total)
zone_data$ymax = cumsum(zone_data$fraction)
zone_data$ymin = c(0, head(zone_data$ymax, n=-1))
zone_data$labelposition <- (zone_data$ymax+zone_data$ymin)/2
zone_data$label <- paste0(zone_data$Zone, "\n %: ", zone_data$fraction*100, "%")
ggplot(zone_data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Zone)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelposition, label=label), size=4)+
  scale_fill_brewer(palette = 4)+
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4))+
  theme_void()+
  theme(legend.position = "none")
dealers <- data.frame(
  Zone = c("East", "North1","North2", "South", "West"),
  Count = c(19, 25, 11, 41, 27)
)
dealers
attach(dealers)
fraction = Count / sum(Count)
ymax = cumsum(fraction)
ymin = c(0, head(ymax, n=-1))
labelposition <- (ymax+ymin)/2
label <- paste0(Zone, "\n %: ", fraction*100, "%")
ggplot(dealers, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Zone)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelposition, label=label), size=4)+
  scale_fill_brewer(palette = 4)+
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4))+
  theme_void()+
  theme(legend.position = "none")
total <- merge(dealers, zone_data, by = "Zone")
total
avg <- total %>% select(Zone, Count, sum_total)
avg
avg_sales <- avg %>% mutate(avg_s = sum_total/Count)
avg_sales
avg_sales %>% ggplot(aes(x = Zone, y = avg_s, fill = Zone))+
  geom_bar(stat = "identity")+ 
  coord_flip()+
  geom_text(aes(label = avg_s), vjust = 0, color ="black", size =3.5)+
  scale_fill_brewer(palette= 4)+
  ggtitle("Average Sales by Dealers in Zone")

                     