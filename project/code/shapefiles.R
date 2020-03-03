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

acoustic <- read.table("/Users/emmadeeks/Dropbox/Overlap_data/Chagos_ALL_acoustic_2019.txt", header = TRUE, sep = ",", dec = ".") #read in the data 
BPV <- read.csv("BPV Pacific Marlin 2013-2016.csv", header = TRUE) # read in the data

acoustic$detect_date <- as.POSIXct(acoustic$detect_date, format="%Y-%m-%d %H:%M:%S")
acoustic$detect_date <- round_date(acoustic$detect_date, unit = "hour")

BPV$Date <- dmy_hms(BPV$Date)
BPV$Date <- round_date(BPV$Date, unit = "hour")


#THIS DATA NOW WORKS 
BPV2 <- read.csv("BPV_Position_Data_Combined_2017_2019.csv", header = TRUE, stringsAsFactors = T) # read in the data


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

pdf("myOut.pdf")
summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
for (i in 1:length(nestmonths$NewDate)){
  monthdata <- nestmonths$data[[i]]
  month <- nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude_BPV, y= Latitude_BPV)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    geom_hex(bins = 50) +
    ggtitle(month) +
    scale_fill_continuous(type = "viridis") +
    theme_bw()
  toadd <- c(month, rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
  plot(year_islands_i)
}
dev.off()


######## ALL data but not the new BPV YET ############
cols <- c("Date","Longitude", "Latitude", "Code")
new_acoustic <- data.table(acoustic$detect_date, acoustic$receiver_lon, acoustic$receiver_lat, acoustic$code)
colnames(new_acoustic) = cols #assigning column names 

cols_BPV <- c("Date","Longitude", "Latitude")
new_BPV <- data.table(BPV$Date, BPV$Longitude, BPV$Latitude)
colnames(new_BPV) = cols_BPV


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
df.new <- df.new[,-2]
df.new <- df.new[,-2]

new_BPV_bind <- rbind(new_BPV, df.new)
###################### 
all_overlap <- merge(new_BPV_bind, new_acoustic, by = "Date", all.x = TRUE, allow.cartesian=TRUE)
cols_10 <- c("Date","Longitude_BPV", "Latitude_BPV", "Longitude_acoustic", "Latitude_acoustic", "Code")
colnames(all_overlap) = cols_10


#df.new <- as.numeric(levels(df.new$Longitude))[df.new$Longitude]
#list2$latitude.fix <- as.numeric(levels(list2$latitude))[list2$latitude]
######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
all_overlap$distance<-distHaversine(all_overlap[,2:3], all_overlap[,4:5], r=r.km)
all_10_overlap <- all_overlap[all_overlap$distance<sep.km,]


# Trying to create a forloop to iterate 
all_10_overlap$NewDate <- substr(all_10_overlap$Date, 0, 7)

all_nestmonths <- all_10_overlap %>%
  nest(data= -NewDate) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that

pdf("all_out.pdf")
summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude_BPV, y= Latitude_BPV)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    geom_hex(bins = 50) +
    ggtitle(month) +
    scale_fill_continuous(type = "viridis") +
    theme_bw()
  toadd <- c(month, rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
  plot(year_islands_i)
}
dev.off()

summary <- c("month", "count", "number_of_different_sharks")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]
summary_sharks <- as.data.frame(summary_sharks, header=TRUE,stringsAsFactors=TRUE)
summary_sharks$month2<-as.Date(format(summary_sharks$month, "%Y-%m-%d"))
df2<-aggregate(count~month2, data=summary_sharks, FUN=sum)
summary_sharks$newdate <- substr(summary_sharks$month, 0, 4)
summary_sharks$newmonth <- substr(summary_sharks$month, 6, 7)
plot(summary_sharks[,1], summary_sharks[,4], typ="l")



plot(as.numeric(summary_sharks$month),summary_sharks$row_counts, type = "l",xaxt="n",main = "Counts")
pdf("shark_counts.pdf")
plot(x = summary_sharks$month, y = summary_sharks$row_counts, col="green", lwd=5, xlab="months", ylab="shark count", main="Count frequency", ylim = c(1,710) )
dev.off()



