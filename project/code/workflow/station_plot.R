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





setwd("/Users/emmadeeks/Dropbox/Overlap_data")
stations <- read.csv("Station_attributes_full.csv")

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data")
pdf("../results/acoustic_GPS/stations_coloured_years.pdf")
ggplot(data=stations, aes(x= x, y= y, colour = factor(Year.Installed))) + 
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_point(size = 3) +
  ggtitle("Station") +
  coord_equal() +
  ggsn::scalebar(Chagos_island,transform = T, dist = 25, dist_unit = "km", model = 'WGS84') +
  theme_bw()
dev.off()

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
#acoustic <- read.csv("acoustic_no_DG.csv")
#BPV <- read.csv("BPV_no_DG.csv")
#BPV$Longitude <- as.numeric(levels(BPV$Longitude))[BPV$Longitude]
#BPV$Latitude <- as.numeric(levels(BPV$Latitude))[BPV$Latitude]

#acoustic$NewDate <- substr(acoustic$Date, 0, 7)

#stations1 <- stations[1,]
BPV <- read.csv("../data/New_data_no_dg_hour/BPV_formatted_CORRECT_hour_no_dg.csv")




r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 20


BPV_final = as.data.frame(matrix(nrow = 1, ncol = 6))
col <- c("X", "Date", "Longitude", "Latitude", "NewDate", "distance")
colnames(BPV_final) <- col


for (i in 1:93){
  temp <- c()
  stations1 <- stations[i,] #select station 
  BPV$distance<-distHaversine(BPV[,3:4], stations1[,2:3], r=r.km) #Calcuate the distance between every BPV entry and that particular station 
  temp <- BPV[BPV$distance<sep.km,] ###  subset entries based on whether or not they are within 20km of a station
  different.names <- (!temp$Date %in% BPV_final$Date) ### find rows that are different between each dataframe 
  not.in.a2 <- temp[different.names,] ###### all rows that are not already in the current dataframe 
  BPV_final <- rbind(BPV_final, not.in.a2)
}




BPV_final <- BPV_final[-1,]

write.csv(BPV_final, "New_data_no_dg_hour/BPV_stations_20km.csv")






