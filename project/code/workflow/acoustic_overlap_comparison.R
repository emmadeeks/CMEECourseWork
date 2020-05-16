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
GPS_acoustic <- read.csv("acoustic_no_elas.csv")

GPS_acoustic$NewDate <- substr(GPS_acoustic$Date, 0 , 7)
GPS_acoustic <- GPS_acoustic[!duplicated(GPS_acoustic[c('Date', 'Code')]),] 


GPS_acoustic$year <- substr(GPS_acoustic$NewDate, 0, 4)

ac_16 <- GPS_acoustic[GPS_acoustic$year == 2016,]
ac_17 <- GPS_acoustic[GPS_acoustic$year == 2017,]
ac_18 <- GPS_acoustic[GPS_acoustic$year == 2018,]

overlap <- read.csv("../results/acoustic_GPS/NO_REPEAT_Ac_GPS_all_10_overlap_no_DG.csv")
overlap$year <- substr(overlap$Date, 0 , 4)

ov_16 <- overlap[overlap$year == 2016,]
ov_17 <- overlap[overlap$year == 2017,]
ov_18 <- overlap[overlap$year == 2018,]

IUU <- read.csv("IUU_Data_catches.csv", header = T)
IUU <- IUU[,1:4]
IUU_6 <- IUU[IUU$Year == 2016,]
IUU_7 <- IUU[IUU$Year == 2017,]
IUU_8 <- IUU[IUU$Year == 2018,]

plot_ov_18 <- ggplot(data=ov_18, aes(x= Longitude_GPS, y= Latitude_GPS)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  ggtitle("Overlap for 2018") +
  geom_hex(aes(x=Longitude_GPS,y=Latitude_GPS)) +
  scale_fill_continuous(type = "viridis", limits = c(0, 100), oob = scales::squish) +
  xlim(70,74) +
  ylim(-8.5, -4) +
  #geom_point(data= IUU, aes(x= Longitude, y= Latitude), shape = 23, fill = "lightblue", size = 1) +
  geom_point(data= IUU_8, aes(x= Longitude, y= Latitude), shape = 23, fill = "orange", size = 2) +
  coord_equal() +
  ggsn::scalebar(Chagos_island,transform = T, dist = 50, dist_unit = "km", model = 'WGS84') +
  theme_bw()


plot_ac_18 <- ggplot(data=ac_18, aes(x= Longitude, y= Latitude)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  ggtitle("Acoustic detections for 2018") +
  #geom_hex(aes(x=Longitude,y=Latitude)) +
  geom_hex(aes(fill = stat(log(count)), xbins = 60)) +
  scale_fill_continuous(type = "viridis", limits = c(0, 9), oob = scales::squish) +
  xlim(70,74) +
  ylim(-8.5, -4) +
  #geom_point(data= IUU, aes(x= Longitude, y= Latitude), shape = 23, fill = "lightblue", size = 2) +
  geom_point(data= IUU_8, aes(x= Longitude, y= Latitude), shape = 23, fill = "orange", size = 2) +
  coord_equal() +
  ggsn::scalebar(Chagos_island,transform = T, dist = 50, dist_unit = "km", model = 'WGS84') +
  theme_bw()

pdf("../results/acoustic/comparison_overlap_aocustic.pdf", onefile = TRUE)
p <- plot_grid(plot_ov_16, plot_ac_16, plot_ov_17, plot_ac_17, plot_ov_18, plot_ac_18, labels = "AUTO", ncol = 2)
dev.off()





