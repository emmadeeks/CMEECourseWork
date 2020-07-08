rm(list=ls()) #Clear global environment 

library(glatos)
library(dplyr)
library(ggplot2)
library(raster) #Require these packages 
library(sf)     #Require 
library(viridis)
library('units')
library('rgdal')

require(ggmap)
require(rgdal)
require(sf)
library(data.table)
library(geosphere)
library(tidyverse)
library(lubridate)
library(plyr)
require(hexbin)
library(ggplot2)
library(tidyverse)
library("wesanderson")




setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
GPS_acoustic <- read.csv("New_data_no_dg_hour/acoustic_GPS_no_elas.csv")

GPS_acoustic$NewDate <- substr(GPS_acoustic$Date, 0 , 7)
GPS_acoustic <- GPS_acoustic[!duplicated(GPS_acoustic[c('Date', 'Code')]),] 
all_combined <- GPS_acoustic %>%
  nest(data= -NewDate) #

detections = as.data.frame(matrix(nrow = 1, ncol = 2))
for (i in 1:nrow(all_combined)) {
  data <- all_combined$data[[i]]
  month <- all_combined$NewDate[[i]]
  detec <- nrow(data)
  toadd <- c(month, detec)
  detections <- rbind(detections, toadd)
}

detections <- detections[-52,]
detections <- detections[-1,]
detections$V1 <- paste0(detections$V1, "-15")

detections$V1 <- as.Date(detections$V1, format="%Y-%m-%d")
detections$V1 <- as.factor(as.character(detections$V1))


#plot(detections$V1, detections$V2)
names <- c("monthyear", "count")
colnames(detections) <- names

tags_at_liberty <- read.csv("../results/acoustic_GPS/AG_standardising_tags.csv")  


detections$count_tag <- 
  sapply(detections$monthyear, function(x)
    sum(as.Date(tags_at_liberty$min, "%Y-%m-%d") <= as.Date(x, "%Y-%m-%d") &
          as.Date(tags_at_liberty$max, "%Y-%m-%d") >= as.Date(x, "%Y-%m-%d")))




summary_original <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')


detections$monthyear <- substr(detections$monthyear, 0 , 7)
new_frame <- merge(summary_original, detections, by = "monthyear")
new_frame$count_tag <- as.numeric(as.character(new_frame$count_tag.y))
new_frame$count <- as.numeric(as.character(new_frame$count.y))


summary_tags <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')



new_frame$plot1 <- (new_frame$count / new_frame$count_tag)
new_frame$plot2 <- (new_frame$count / new_frame$number_sharks)
new_frame <- new_frame[-30,]




detections$count_tag <- as.numeric(as.character(detections$count_tag))
detections$count <- as.numeric(as.character(detections$count))
detections$plot1 <- detections$count / detections$count_tag
detections$monthyear <- substr(detections$monthyear, 0 , 7)



pdf("../results/acoustic/acoustic_detections_tags_at_lib.pdf")
ggplot() + geom_bar(detections, mapping = aes(x=monthyear, y=plot1), stat="identity", colour = "black", alpha = 0.3) +
  ylab("Number of detections of acoustically tagged sharks standardised by tags at liberty") +
  #geom_line(summary_tags, mapping = aes(x=monthyear, y=standard2, group = 1)) +
  #geom_bar(alpha = 0.5) +
  #theme(axis.text.x = element_text(angle = 90)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), axis.text.x = element_text(angle = 90), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

dev.off()




#pdf("../results/Thesis_figures/figure_2_panel.pdf")
ggplot() + geom_bar(new_frame, mapping = aes(x=monthyear, y=plot2), stat="identity", colour = "black", alpha = 0.3) +
  ylab("Detection frequency (standardised by tags at liberty)") +
  #geom_line(summary_tags, mapping = aes(x=monthyear, y=standard2, group = 1)) +
  #geom_bar(alpha = 0.5) +
  #theme(axis.text.x = element_text(angle = 90)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), axis.text.x = element_text(angle = 90), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
#dev.off()






