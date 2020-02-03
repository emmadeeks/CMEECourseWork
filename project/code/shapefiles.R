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

library(raster) #Require these packages 
library(sf)     #Require 
library(viridis)
library('units')
library('rgdal')

require(ggmap)
require(rgdal)
require(sf)

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

#THIS DATA IS WEIRD AND NEEDS TO BE SORTED OUT 
BPV2 <- read.csv("BPV_Position_Data_Combined_2017_2019.csv", header = TRUE) # read in the data
##### Plotting 2014 ###########

BPV_2k14 <- BPV[grep("2014", BPV$Date),] #extract one year of the data
acoustic_2k14 <- acoustic[grep("2014-", acoustic$detect_date),] # extract the same year of the data 

map_2k14 <- ggplot() + geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_2k14, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_2k14, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))

#points(x = acoustic_2k14[,7], y = acoustic_2k14[,6], col="red", cex = 1, pch = 19)

#points(x = BPV_2k14[,3], y = BPV_2k14[,2], col="blue", cex = 1, pch = 19, asp = 1)

##### Plotting 2016 ###########

BPV_2k16 <- BPV[grep("2016", BPV$Date),] #extract one year of the data
acoustic_2k16 <- acoustic[grep("2016-", acoustic$detect_date),] # extract the same year of the data 

map_2k16 <- ggplot() + geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_2k16, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_2k16, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))

########### Plotting one day in 2016 #########

BPV_birthday <- BPV[grep("16/02/2016", BPV$Date),] #extract one year of the data
acoustic_birthday <- acoustic[grep("2016-02-16", acoustic$detect_date),] # extract the same year of the data 

map_birthday <- ggplot() + geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_birthday, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_birthday, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))

####### month 

BPV_birthday <- BPV[grep("02/2016", BPV$Date),] #extract one year of the data
acoustic_birthday <- acoustic[grep("2016-02", acoustic$detect_date),] # extract the same year of the data 

map_birthday <- ggplot() + geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_birthday, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_birthday, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))


###### CREATING RADIUS ###########

########## Experimenting with data 
library(data.table)
library(geosphere)

sep.km   <- 10      # critical separation ((km))

#Preparing data 
### Creating a data frame with the code of the tag, long and latitude from data from feburary 2016
cols <- c("Code","Longitude", "Latitude")

new_acoustic <- cbind(acoustic_birthday$code, acoustic_birthday$receiver_lon, acoustic_birthday$receiver_lat)
new_acoustic <- data.table(new_acoustic)
colnames(new_acoustic) = cols #assigning column names 

collating_BPV <- cbind(BPV_birthday$Degrees , BPV_birthday$Longitude, BPV_birthday$Latitude)
collating_BPV <- data.table(collating_BPV)
colnames(collating_BPV) = cols

####### Function that finds datapoints within 10km  radius of eachother 

d <- function(x){                     # distance between station[i] and all subsequent stations
  r.ft <- 6378137*3.28084             # radius of the earth, in feet
  r.km   <- r.ft*0.0003048
  if (x[1]==nrow(new_acoustic)) return()  # don't process last row
  ref <- new_acoustic[(x[1]+1):nrow(new_acoustic),]
  z <- distHaversine(ref[,2:3,with=F],x[2:3], r=r.km)
  z <- data.table(BPV_time=x[1], tag.2=ref$Code, dist=z, long.1=x[2], lat.1=x[3], long.2=ref$Longitude, lat.2=ref$Latitude)
  return(z[z$dist<sep.km,])
}
#coloc.2 = do.call(rbind,apply(acoustic,1,d))
comparison = do.call(rbind,apply(collating_BPV,1,d))

map_overlap <- ggplot() + geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=comparison, aes(x= long.1, y= lat.1),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=comparison, aes(x= long.2, y= lat.2),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))

map_islands <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=comparison, aes(x= long.1, y= lat.1),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=comparison, aes(x= long.2, y= lat.2),size=2, pch = 21, colour = "Pink", fill = "Deep Pink", alpha=I(0.5))


###### This tells me how many overlaps there were from a certain position in a certain day but it doesnt tell me what they were 
## For this to work I would need to know if there was an overlap (which is what this does) and where this overlap is 
# Another issue with this code is that they are only using this dataset not the other dataset 
# I would need to either combine datasets and then fish out which datasets are BPV and acoustic overlaps 
cbind(new_acoustic, X=rowSums(distm (new_acoustic[,3:2], 
                                     fun = distHaversine) / 1 <= 10)) # number of points within distance 10000 km


