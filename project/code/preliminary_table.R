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

################# acoustic and BPV 1
acoustic <- read.table("/Users/emmadeeks/Dropbox/Overlap_data/Chagos_ALL_acoustic_2019.txt", header = TRUE, sep = ",", dec = ".") #read in the data 
BPV <- read.csv("BPV Pacific Marlin 2013-2016.csv", header = TRUE) # read in the data


acoustic$detect_date <- as.POSIXct(acoustic$detect_date, format="%Y-%m-%d %H:%M:%S")
acoustic$new_date <- round_date(acoustic$detect_date, unit = "hour")

BPV$date <- dmy_hms(BPV$Date)
BPV$Date <- round_date(BPV$date, unit = "hour")
################################### BPV 2


#THIS DATA NOW WORKS 
BPV2 <- read.csv("BPV_Position_Data_Combined_2017_2019.csv", header = TRUE, stringsAsFactors = T) # read in the data


######## ALL data but not the new BPV YET ############
cols <- c("Date","Longitude", "Latitude", "Code", "olddate")
new_acoustic <- data.table(acoustic$new_date, acoustic$receiver_lon, acoustic$receiver_lat, acoustic$code, acoustic$detect_date)
colnames(new_acoustic) = cols #assigning column names 

cols_BPV <- c("Date","Longitude", "Latitude", "olddate")
new_BPV <- data.table(BPV$Date, BPV$Longitude, BPV$Latitude, BPV$date)
colnames(new_BPV) = cols_BPV


######### FORMATTING BPV2 ############

cols_BPV <- c("Date","Longitude", "Latitude", "olddate")
BPV2$newDate <- paste(BPV2$Date, BPV2$Time)
BPV2$Date <- paste(BPV2$Date, BPV2$Time)
new_BPV_2 <- data.table(BPV2$newDate, BPV2$DEC_LON, BPV2$DEC_LAT, BPV2$Date)
colnames(new_BPV_2) = cols_BPV

new_BPV_2$Date <- dmy_hms(new_BPV_2$Date)
new_BPV_2$Date <- round_date(new_BPV_2$Date, unit = "hour")
df.new = new_BPV_2[seq(1, nrow(new_BPV_2), 24), ]
df.new$olddate <- dmy_hms(df.new$olddate)

new_BPV_bind <- rbind(new_BPV, df.new)


new_BPV_bind$Longitude <- as.numeric(levels(new_BPV_bind$Longitude))[new_BPV_bind$Longitude]
new_BPV_bind$Latitude <- as.numeric(levels(new_BPV_bind$Latitude))[new_BPV_bind$Latitude]
BPV_formatted <- new_BPV_bind[with(new_BPV_bind, !((Longitude >= 72.3 & Longitude <= 72.5) | (Latitude >= -7 & Latitude <= -7.6))), ]

acoustic_formatted <- new_acoustic[with(new_acoustic, !((Longitude >= 72.3 & Longitude <= 72.5) | (Latitude >= -7 & Latitude <= -7.6))), ]


write.csv(BPV_formatted, "BPV_no_DG.csv")
write.csv(new_BPV_bind, "BPV_formatted_times.csv")
write.csv(new_acoustic, "acoustic_formatted_times.csv")
write.csv(acoustic_formatted, "acoustic_no_DG.csv")

################## 