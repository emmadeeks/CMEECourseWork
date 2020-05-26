rm(list=ls()) #Clear global environment 
setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data/shape_files/")

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



chagos_v6 <- read_sf(dsn = ".", layer = "Chagos_v6") #read in the shapefiles
#chagosEEZ <- read_sf(dsn = ".", layer = "ChagosEEZ") # read in the shapefiles

#plot(st_geometry(chagos_v6), asp=1, axes=TRUE)
#plot(st_geometry(chagosEEZ), asp=1, axes=TRUE, main='Chagos outline') #plot the shapefiles

tracts <- readOGR(dsn = ".", layer = "ChagosEEZ") %>%
  spTransform("+proj=longlat +ellps=WGS84")
Chagos_try <- fortify(tracts)

chagos_v6 <- readOGR(dsn = ".", layer = "Chagos_v6") %>%
  spTransform("+proj=longlat +ellps=WGS84")
Chagos_island <- fortify(chagos_v6)


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
new_frame$count_tag <- as.numeric(as.character(new_frame$count_tag))
new_frame$count <- as.numeric(as.character(new_frame$count))
new_frame <- merge(summary_original, detections, by = "monthyear")

summary_tags <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')

new_frame$plot1 <- (new_frame$count / new_frame$count_tag)
new_frame$plot2 <- (new_frame$count / new_frame$number_sharks)


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
