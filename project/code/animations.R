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
library(datasauRus)
library(ggplot2)
library(gapminder)
library(gganimate)
library(gifski)

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


#### reading in data 
setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data")
all_overlap <- read.csv("all_overlap_10.csv", header = T, stringsAsFactors = F)
april_overlap <- read.csv("april_overlap.csv", header = T, stringsAsFactors = F)
BPV <- read.csv("BPV_formatted_times.csv", header = T, stringsAsFactors = F)
acoustic <- read.csv("acoustic_formatted_times.csv", header = T, stringsAsFactors = F)
BPV$NewDate <- substr(BPV$Date, 0, 7)

######## preliminary plotting 

map_april <- ggplot(overlap_18, aes(x= Longitude, y= Latitude)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_point() 

map_april + geom_density_2d(aes(color = ..level..)) 

############# animating pathway 

overlap_18 <- BPV[grep("2018-04", BPV$Date),] #extract one year of the data
acoustic_18 <- acoustic[grep("2018-04-15", acoustic$Date),] # extract the same year of the data 

overlap_18$Longitude <-  as.numeric(as.character(overlap_18$Longitude))
overlap_18$Latitude<-  as.numeric(as.character(overlap_18$Latitude))


april_islands <- ggplot() + 
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=overlap_18, aes(x= Longitude, y= Latitude),size=2, pch = 21, colour = "Blue", fill = "Blue") 

april_islands

geom_hex(aes(fill = stat(log(count)))) +
  scale_fill_continuous(type = "viridis")

########### animating preliminary plot through time 


# https://github.com/thomasp85/gganimate/issues/222



library(gapminder)

p <- ggplot(
  airquality,
  aes(Day, Temp, group = Month, color = factor(Month))
) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature") +
  theme(legend.position = "top")
p


p <- ggplot(overlap_18, aes(x= Longitude, y= Latitude)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_line()


overlap_18$NewDate <- substr(overlap_18$Date, 0, 10)
overlap_18$NewDate <- as.POSIXct(overlap_18$NewDate, format="%Y-%m-%d")
p + 
  geom_point() +
  transition_reveal(overlap_18$NewDate) +
  shadow_wake(wake_length = 0.5)


anim_save("month_example.gif")
