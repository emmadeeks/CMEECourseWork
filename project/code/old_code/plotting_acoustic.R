
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


setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
acoustic <- read.csv("acoustic_formatted_times.csv")
BPV <- read.csv("BPV_formatted_times.csv")
BPV$Longitude <- as.numeric(levels(BPV$Longitude))[BPV$Longitude]
BPV$Latitude <- as.numeric(levels(BPV$Latitude))[BPV$Latitude]

acoustic$NewDate <- substr(acoustic$Date, 0, 7)


all_nestmonths <- acoustic %>%
  nest(data= -NewDate) #



pdf("EVERY_acoustic_log_new.pdf")
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude, y= Latitude)) + 
    geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(aes(fill = stat(log(count)))) +
    scale_fill_continuous(type = "viridis", limits = c(0, 10), oob = scales::squish) +
    theme_bw()
  plot(year_islands_i)
}
dev.off()





combined <- merge(acoustic, BPV, by.x = "Date", by.y = "Date")

all_combined <- combined %>%
  nest(data= -NewDate) #

monthdata <- all_combined$data[[1]]
month <- all_combined$NewDate[[1]]
year_islands_i <- ggplot() + 
  geom_point(data=monthdata, aes(x= Longitude.x, y= Latitude.x)) +
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  ggtitle(month) +
  geom_hex(aes(fill = stat(log(count)))) +
  scale_fill_continuous(type = "viridis", limits = c(0, 10), oob = scales::squish) +
  theme_bw() 
  p <- ggplot(data=monthdata, aes(x= Longitude.y, y= Latitude.y)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  geom_point() +  
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point() +
  geom_density2d() + stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE)
  
  
  
  
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude.y, y= Latitude.y)) + 
    geom_point() +
    geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(data=monthdata, aes(x= Longitude.x, y= Latitude.x, fill = stat(log(count)))) +
    scale_fill_continuous(type = "viridis", limits = c(0, 10), oob = scales::squish) +
    theme_bw() + 
    geom_density2d() + stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon') +
    #scale_fill_gradient(low = "green", high = "red") +
    scale_alpha(range = c(0.00, 0.25), guide = FALSE)
  
  p <- ggplot(data=monthdata, aes(x= Longitude.y, y= Latitude.y)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
    geom_point() +  
    geom_density2d() + stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon') +
    scale_fill_gradient(low = "green", high = "red") +
    scale_alpha(range = c(0.00, 0.25), guide = FALSE)
  
  
  
  pdf("EVERY_acoustic_BPV.pdf")
  for (i in 1:length(all_combined$NewDate)){
    monthdata <- all_combined$data[[i]]
    month <- all_combined$NewDate[[i]]
    year_islands_i <- ggplot(data=monthdata, aes(x= Longitude.y, y= Latitude.y)) + 
      geom_point() +
      geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
      ggtitle(month) +
      geom_hex(data=monthdata, aes(x= Longitude.x, y= Latitude.x, fill = stat(log(count)))) +
      scale_fill_continuous(type = "viridis", limits = c(0, 10), oob = scales::squish) +
      theme_bw() + 
      geom_density2d() + stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon') +
      #scale_fill_gradient(low = "green", high = "red") +
      scale_alpha(range = c(0.00, 0.25), guide = FALSE)
    plot(year_islands_i)
  }
  dev.off()
