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

cols <- c("Code", "Date", "Latitude", "Longitude")
colnames(GPS_all) <- cols 

BPV <- data.frame(BPV, stringsAsFactors=FALSE)
GPS_all <- data.frame(GPS_all, stringsAsFactors=FALSE)


GPS_all$Date <- dmy_hm(GPS_all$Date)
GPS_all$Date <- round_date(GPS_all$Date, unit = "hour")

m1 <- merge(GPS_all, BPV, by.x = "Date", by.y = "Date")

cols_10 <- c("Date","Code", "Longitude_GPS", "Latitude_GPS", "Longitude_BPV", "Latitude_BPV")
colnames(m1) = cols_10

r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 30
m1$distance<-distHaversine(m1[,4:3], m1[,5:6], r=r.km)
GPS_10_overlap <- m1[m1$distance<sep.km,]

ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  geom_point(data=GPS_30_overlap, aes(x= Longitude_BPV, y= Latitude_BPV),size=2, pch = 21, colour = "Blue", fill = "Blue") +
  geom_point(data=GPS_30_overlap, aes(x= Latitude_GPS, y= Longitude_GPS),size=2, pch = 21, colour = "Pink", fill = "Pink")


sep.km   <- 10
m1$distance<-distHaversine(m1[,4:3], m1[,5:6], r=r.km)
GPS_30_overlap <- m1[m1$distance<sep.km,]

