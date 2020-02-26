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

acoustic <- read.table("Chagos_ALL_acoustic_2019.txt", header = TRUE, sep = ",", dec = ".") #read in the data 
BPV <- read.csv("BPV Pacific Marlin 2013-2016.csv", header = TRUE) # read in the data

acoustic$detect_date <- as.POSIXct(acoustic$detect_date, format="%Y-%m-%d %H:%M:%S")
acoustic$detect_date <- round_date(acoustic$detect_date, unit = "hour")

BPV$Date <- dmy_hms(BPV$Date)
BPV$Date <- round_date(BPV$Date, unit = "hour")


#THIS DATA NOW WORKS 
BPV2 <- read.csv("BPV_Position_Data_Combined_2017_2019.csv", header = TRUE) # read in the data

####################### birthday ##################

BPV_birthday <- BPV[grep("2016-02", BPV$Date),] #extract one year of the data
acoustic_birthday <- acoustic[grep("2016-02", acoustic$detect_date),] # extract the same year of the data 

map_birthday <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_birthday, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_birthday, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))


###### Formatting data to be processed by the function 
cols <- c("Date","Longitude", "Latitude")
new_acoustic_birthday <- data.table(acoustic_birthday$detect_date, acoustic_birthday$receiver_lon, acoustic_birthday$receiver_lat)
colnames(new_acoustic_birthday) = cols #assigning column names 

new_BPV_birthday <- data.table(BPV_birthday$Date, BPV_birthday$Longitude, BPV_birthday$Latitude)
colnames(new_BPV_birthday) = cols

birthday <- merge(new_BPV_birthday, new_acoustic_birthday, by = "Date", all.x = TRUE)

######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
birthday$distance<-distHaversine(birthday[,2:3], birthday[,4:5], r=r.km)
less_than_10 <- birthday[birthday$distance<sep.km,]


map_islands <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=less_than_10, aes(x= Longitude.x, y= Latitude.x),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=less_than_10, aes(x= Longitude.y, y= Latitude.y),size=2, pch = 21, colour = "Pink", fill = "Deep Pink", alpha=I(0.5))

################# March #################

BPV_march <- BPV[grep("2016-03", BPV$Date),] #extract one year of the data
acoustic_march <- acoustic[grep("2016-03", acoustic$detect_date),] # extract the same year of the data 

map_march <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_march, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_march, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))



###### Formatting data to be processed by the function 
cols <- c("Date","Longitude", "Latitude")
new_acoustic_march <- data.table(acoustic_march$detect_date, acoustic_march$receiver_lon, acoustic_march$receiver_lat)
colnames(new_acoustic_march) = cols #assigning column names 

new_BPV_march <- data.table(BPV_march$Date, BPV_march$Longitude, BPV_march$Latitude)
colnames(new_BPV_march) = cols


#longterm <- subset(count(collating_BPV$Date, new_acoustic$Date))
march_overlap <- merge(new_BPV_march, new_acoustic_march, by = "Date", all.x = TRUE)

######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
march_overlap$distance<-distHaversine(march_overlap[,2:3], march_overlap[,4:5], r=r.km)
march_10_overlap <- march_overlap[march_overlap$distance<sep.km,]


march_islands <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=march_10_overlap, aes(x= Longitude.x, y= Latitude.x),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=march_10_overlap, aes(x= Longitude.y, y= Latitude.y),size=2, pch = 21, colour = "Pink", fill = "Deep Pink", alpha=I(0.5))


################################ April BPV ########################

BPV_april <- BPV[grep("2016-04", BPV$Date),] #extract one year of the data
acoustic_april <- acoustic[grep("2016-04", acoustic$detect_date),] # extract the same year of the data 

map_april <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_april, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_april, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))



###### Formatting data to be processed by the function 
cols <- c("Date","Longitude", "Latitude")
new_acoustic_april <- data.table(acoustic_april$detect_date, acoustic_april$receiver_lon, acoustic_april$receiver_lat)
colnames(new_acoustic_april) = cols #assigning column names 

new_BPV_april <- data.table(BPV_april$Date, BPV_april$Longitude, BPV_april$Latitude)
colnames(new_BPV_april) = cols


#longterm <- subset(count(collating_BPV$Date, new_acoustic$Date))
april_overlap <- merge(new_BPV_april, new_acoustic_april, by = "Date", all.x = TRUE)

######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
april_overlap$distance<-distHaversine(april_overlap[,2:3], april_overlap[,4:5], r=r.km)
april_10_overlap <- april_overlap[april_overlap$distance<sep.km,]


april_islands <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=april_10_overlap, aes(x= Longitude.x, y= Latitude.x),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=april_10_overlap, aes(x= Longitude.y, y= Latitude.y),size=2, pch = 21, colour = "Pink", fill = "Deep Pink", alpha=I(0.5))


##################### HEAT MAPS ##############
april_islands <- ggplot(data=april_10_overlap, aes(x= Longitude.x, y= Latitude.x)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_hex(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()


april_islands <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=april_10_overlap, aes(x= Longitude.x, y= Latitude.x),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_raster(aes(fill = density)) +
  scale_fill_gradientn(colours = terrain.colors(10))

april_islands <- ggplot(data=april_10_overlap, aes(x= Longitude.x, y= Latitude.x)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_hex(bins = 70) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme_bw()

############################### August ########################

BPV_august <- BPV[grep("2016-06", BPV$Date),] #extract one year of the data
acoustic_august <- acoustic[grep("2016-06", acoustic$detect_date),] # extract the same year of the data 

map_august <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_august, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_august, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))



###### Formatting data to be processed by the function 
cols <- c("Date","Longitude", "Latitude")
new_acoustic_august <- data.table(acoustic_august$detect_date, acoustic_august$receiver_lon, acoustic_august$receiver_lat)
colnames(new_acoustic_august) = cols #assigning column names 

new_BPV_august <- data.table(BPV_august$Date, BPV_august$Longitude, BPV_august$Latitude)
colnames(new_BPV_august) = cols


#longterm <- subset(count(collating_BPV$Date, new_acoustic$Date))
august_overlap <- merge(new_BPV_august, new_acoustic_august, by = "Date", all.x = TRUE)

######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
august_overlap$distance<-distHaversine(august_overlap[,2:3], august_overlap[,4:5], r=r.km)
august_10_overlap <- august_overlap[august_overlap$distance<sep.km,]


august_islands <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=august_10_overlap, aes(x= Longitude.x, y= Latitude.x),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=august_10_overlap, aes(x= Longitude.y, y= Latitude.y),size=2, pch = 21, colour = "Pink", fill = "Deep Pink", alpha=I(0.5))


########################################## YEAR 2016 ################################


BPV_year <- BPV[grep("2016-", BPV$Date),] #extract one year of the data
acoustic_year <- acoustic[grep("2016-", acoustic$detect_date),] # extract the same year of the data 

map_year <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_year, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_year, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))



###### Formatting data to be processed by the function 
cols <- c("Date","Longitude", "Latitude", "Code")
new_acoustic_year <- data.table(acoustic_year$detect_date, acoustic_year$receiver_lon, acoustic_year$receiver_lat, acoustic_year$code)
colnames(new_acoustic_year) = cols #assigning column names 

cols_BPV <- c("Date","Longitude", "Latitude")
new_BPV_year <- data.table(BPV_year$Date, BPV_year$Longitude, BPV_year$Latitude)
colnames(new_BPV_year) = cols_BPV


#longterm <- subset(count(collating_BPV$Date, new_acoustic$Date))
year_overlap <- merge(new_BPV_year, new_acoustic_year, by = "Date", all.x = TRUE)
cols_10 <- c("Date","Longitude_BPV", "Latitude_BPV", "Longitude_acoustic", "Latitude_acoustic", "Code")
colnames(year_overlap) = cols_10

######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
year_overlap$distance<-distHaversine(year_overlap[,2:3], year_overlap[,4:5], r=r.km)
year_10_overlap <- year_overlap[year_overlap$distance<sep.km,]


year_islands <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=year_10_overlap, aes(x= Longitude.x, y= Latitude.x),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=year_10_overlap, aes(x= Longitude.y, y= Latitude.y),size=2, pch = 21, colour = "Pink", fill = "Deep Pink", alpha=I(0.5))

year_islands <- ggplot(data=year_10_overlap, aes(x= Longitude.x, y= Latitude.x)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_hex(bins = 30) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()


year_islands <- ggplot(data=year_10_overlap, aes(x= Longitude_BPV, y= Latitude_BPV)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_hex(bins = 30) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

# Trying to create a forloop to iterate 
year_10_overlap$NewDate <- substr(year_10_overlap$Date, 0, 7)

nestmonths <- year_10_overlap %>%
  nest(data= -NewDate) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that


summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
for (i in 1:length(nestmonths$NewDate)){
  monthdata <- nestmonths$data[[i]]
  month <- nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  year_islands <- ggplot(data=monthdata, aes(x= Longitude_BPV, y= Latitude_BPV)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    geom_hex(bins = 50) +
    ggtitle(month) +
    scale_fill_continuous(type = "viridis") +
    theme_bw()
  toadd <- c(month, rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
  ggsave(year_islands, file=paste0("plot_", i,".png"), width = 14, height = 10)
}
