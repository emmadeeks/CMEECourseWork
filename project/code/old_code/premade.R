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

acoustic <- read.table("/Users/emmadeeks/Dropbox/Overlap_data/Chagos_ALL_acoustic_2019.txt", header = TRUE, sep = ",", dec = ".") #read in the data 
BPV <- read.csv("BPV Pacific Marlin 2013-2016.csv", header = TRUE) # read in the data

acoustic$detect_date <- as.POSIXct(acoustic$detect_date, format="%Y-%m-%d %H:%M:%S")
acoustic$detect_date <- round_date(acoustic$detect_date, unit = "hour")

BPV$Date <- dmy_hms(BPV$Date)
BPV$Date <- round_date(BPV$Date, unit = "hour")


#THIS DATA NOW WORKS 
BPV2 <- read.csv("BPV_Position_Data_Combined_2017_2019.csv", header = TRUE, stringsAsFactors = T) # read in the data


######### FORMATTING BPV2 ############
#new_BPV_2 <- data.table(BPV2$Date, BPV2$Time, BPV2$LAT, BPV2$LON)


cols_BPV <- c("Date","Longitude", "Latitude")
BPV2$Date <- paste(BPV2$Date, BPV2$Time)
new_BPV_2 <- data.table(BPV2$Date, BPV2$DEC_LON, BPV2$DEC_LAT)
colnames(new_BPV_2) = cols_BPV
#new_BPV_2$Latitude <- as.numeric(new_BPV_2$Latitude)
new_BPV_2$Date <- dmy_hms(new_BPV_2$Date)
new_BPV_2$Date <- round_date(new_BPV_2$Date, unit = "hour")
#new_BPV_2 <- as.data.frame(new_BPV_2)
#new_BPV_3 <- new_BPV_2[as.numeric(new_BPV_2$Latitude) > 0, ]
#new_BPV_4 <- new_BPV_2[as.numeric(new_BPV_2$Latitude)<0, ]
df.new = new_BPV_2[seq(1, nrow(new_BPV_2), 24), ]



df.new$Longitude <- as.numeric(levels(df.new$Longitude))[df.new$Longitude]
df.new$Latitude <- as.numeric(levels(df.new$Latitude))[df.new$Latitude]
#df.new <- df.new[,-2]
#df.new <- df.new[,-2]

new_BPV_bind <- rbind(new_BPV, df.new)

######### reading in Tag data 

summary <- read.csv('summary_sharks.csv')
tags <- read.csv('summary_tags.csv')


######################## USING STAT_DENSITY FOR ANIMATIONS ########

################################ April BPV ########################

BPV_april <- BPV[grep("2016-04", BPV$Date),] #extract one year of the data
acoustic_april <- acoustic[grep("2016-04", acoustic$detect_date),] # extract the same year of the data 

map_april <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_point(data=acoustic_april, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5)) + geom_density_2d()

#map_april <- ggplot(data=acoustic_april, aes(x= receiver_lon, y= receiver_lat)) + geom_point() +
#  xlim(70, 73) +
#  ylim(-9, -4)

#map_april + geom_density_2d() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA)


############## THIS FORMAT WORKS ##################
map_april <- ggplot(data=acoustic_april, aes(x= receiver_lon, y= receiver_lat)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point()
map_april + geom_density_2d()



map_april <- ggplot(BPV_april, aes(x= Longitude, y= Latitude)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point()
map_april + geom_density_2d()






library(datasauRus)
library(ggplot2)
library(gganimate)

ggplot(datasaurus_dozen, aes(x=x, y=y))+
  geom_point()+
  theme_minimal() +
  transition_states(dataset, 3, 1) + 
  ease_aes('cubic-in-out')




ggplot(datasaurus_dozen, aes(x=x, y=y))+
  geom_point()+
  theme_minimal() +
  transition_states(dataset, 3, 1) + 
  ease_aes('cubic-in-out')

