#!/usr/bin/env Rscript

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: GPDD_Data.R
#Desc: Loads and plots the species abundance worldwide using the maps package and saves it as a pdf	
#Arguments: Uses GPDDFiltered.RData in data
#Outputs: Species abundance worldwide
#Date: Oct 2019  
rm(list=ls()) #Clear global environment 
setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data/shape_files/")
#Map of the world American
#Load the data 
#install.packages('raster') # Core raster GIS data package
#install.packages('sf') # Core vector GIS data package
#install.packages('rgeos') # Extends vector data functionality
#install.packages('lwgeom') # Extends vector data functionality
#install.packages('viridis') # Because we like the colour scheme!
#install.packages('ggmap')
#install.packages('rgdal')
#install.packages("hexbin")

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
#chagos_array <- read_sf(dsn = ".", layer = "Chagos_array")
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


setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data")

BPV <- read.csv("BPV_formatted_times.csv", header = T)
GPS_all <- read.csv("shark_GPS_all.csv", colClasses=c("numeric", "character", "numeric", "numeric"))

trying3 <- GPS_all[with(GPS_all, !((Latitude_GPS >= 72.3 & Latitude_GPS <= 72.5) | (Longitude_GPS >= -7 & Longitude_GPS <= -7.6))), ]


cols <- c("Code", "Date", "Latitude", "Longitude")
colnames(GPS_all) <- cols 

BPV <- data.frame(BPV, stringsAsFactors=FALSE)
GPS_all <- data.frame(GPS_all, stringsAsFactors=FALSE)

BPV <- BPV[,-5]
BPV <- BPV[,-1]
GPS_all$Date <- dmy_hm(GPS_all$Date)
GPS_all$Date <- round_date(GPS_all$Date, unit = "hour")

BPV$Date <- as.POSIXct(BPV$Date, format="%Y-%m-%d %H:%M:%S")
BPV$Date <- round_date(BPV$Date, unit = "hour")
BPV$Longitude <- as.numeric(levels(BPV$Longitude))[BPV$Longitude]
BPV$Latitude <- as.numeric(levels(BPV$Latitude))[BPV$Latitude]



m1 <- merge(GPS_all, BPV, by.x = "Date", by.y = "Date")



cols_10 <- c("Date","Code", "Longitude_GPS", "Latitude_GPS", "Longitude_BPV", "Latitude_BPV")

colnames(m1) = cols_10





r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 20
m1$distance<-distHaversine(m1[,4:3], m1[,5:6], r=r.km)
GPS_10_overlap <- m1[m1$distance<sep.km,]


trying3 <- GPS_10_overlap[with(GPS_10_overlap, !((Latitude_GPS >= 72.3 & Latitude_GPS <= 72.5) | (Longitude_GPS >= -7 & Longitude_GPS <= -7.6))), ]



yourdata %>%
  filter(columntofilter >= 1.5 & columntofilter <=2)


pdf("../results/overlap_10.pdf")
 ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  geom_point(data=GPS_10_overlap, aes(x= Longitude_BPV, y= Latitude_BPV),size=2, pch = 21, colour = "Blue", fill = "Blue") +
  geom_point(data=GPS_10_overlap, aes(x= Latitude_GPS, y= Longitude_GPS),size=2, pch = 21, colour = "Red", fill = "Red")
dev.off()

ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  geom_point(data=trying3, aes(x= Latitude_GPS, y= Longitude_GPS),size=2, pch = 21, colour = "Red", fill = "Red")
################ Rounding overlap by day 

BPV$Day <- substr(BPV$Date, 0, 10)
GPS_all$Day <- substr(GPS_all$Date, 0, 10)
m1_day <- merge(GPS_all, BPV, by.x = "Day", by.y = "Day")
cols_10 <- c("Day","Code", "Date", "Longitude_GPS", "Latitude_GPS", "Date", "Longitude_BPV", "Latitude_BPV")
colnames(m1_day) = cols_10

m1_day$distance<-distHaversine(m1_day[,5:4], m1_day[,7:8], r=r.km)
GPS_10_overlap_day <- m1_day[m1_day$distance<sep.km,]

GPS_10_overlap_day <- GPS_10_overlap_day[,-3]


pdf("../results/overlap_20_DAY_GPS_extended_sharks.pdf")
ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  geom_point(data=GPS_10_overlap_day, aes(x= Latitude_GPS, y= Longitude_GPS),size=2, pch = 21, colour = "Red", fill = "Red")
dev.off()

############# Overlap by one day ##################


GPS_all$Round <- round_date(GPS_all$Date, "12 hours")
BPV$Round <- round_date(BPV$Date, "12 hours")

m1_4 <- merge(GPS_all, BPV, by = "Round", all.x = T)
cols_10 <- c("Round","Code", "Date", "Longitude_GPS", "Latitude_GPS", "Date_BPV", "Longitude_BPV", "Latitude_BPV")
colnames(m1_4) = cols_10

m1_4 <- m1_4[,-3]
m1_4 <- m1_4[,-5]
m1_4$distance<-distHaversine(m1_4[,4:3], m1_4[,5:6], r=r.km)
#m1_4 <- m1_4[, -9]
GPS_10_overlap_4 <- m1_4[m1_4$distance<sep.km,]




pdf("../results/overlap_10_DAY.pdf")
ggplot() + geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  geom_point(data=GPS_10_overlap_4, aes(x= Longitude_BPV, y= Latitude_BPV),size=2, pch = 21, colour = "Blue", fill = "Blue") +
  geom_point(data=GPS_10_overlap_4, aes(x= Latitude_GPS, y= Longitude_GPS),size=2, pch = 21, colour = "Red", fill = "Red")
dev.off()

########### looking at one shark route 

shark1 <- GPS_all[which(GPS_all$Code=='135902'), ]
ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  geom_point(data=shark1, aes(x= Longitude , y= Latitude),size=2, pch = 21, colour = "Blue", fill = "Blue")

all_nestmonths <- GPS_all %>%
  nest(data= -Code) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that


pdf("GPS_sharks_outer.pdf")
for (i in 1:length(all_nestmonths$Code)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$Code[[i]]
  year_islands_i <- ggplot() + geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
    geom_point(data=monthdata, aes(x= Longitude , y= Latitude),size=2, pch = 21, colour = "Blue", fill = "Blue") +
    ggtitle(month) 
  plot(year_islands_i)
}
dev.off()


