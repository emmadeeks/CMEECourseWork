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


setwd("/Users/emmadeeks/Dropbox/Overlap_data")
stations <- read.csv("Station_attributes_full.csv")
station_plot <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_point(data=stations, aes(x= x, y= y), colour = "Blue", shape = 7) +
  ggtitle("Station") +
  theme_bw()



setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
#acoustic <- read.csv("acoustic_no_DG.csv")
#BPV <- read.csv("BPV_no_DG.csv")
#BPV$Longitude <- as.numeric(levels(BPV$Longitude))[BPV$Longitude]
#BPV$Latitude <- as.numeric(levels(BPV$Latitude))[BPV$Latitude]

#acoustic$NewDate <- substr(acoustic$Date, 0, 7)

#stations1 <- stations[1,]


r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10


BPV_final = as.data.frame(matrix(nrow = 1, ncol = 6))
col <- c("X", "Date", "Longitude", "Latitude", "olddate", "distance")
colnames(BPV_final) <- col


for (i in 1:93){
  temp <- c()
  stations1 <- stations[i,]
  BPV$distance<-distHaversine(BPV[,3:4], stations1[,2:3], r=r.km)
  temp <- BPV[BPV$distance<sep.km,]
  different.names <- (!temp$Date %in% BPV_final$Date)
  not.in.a2 <- temp[different.names,]
  BPV_final <- rbind(BPV_final, not.in.a2)
}


write.csv(BPV_final, "standard/BPV_stations_10km")

BPV <- read.csv("standard/BPV_stations_10km")
acoustic <- read.csv("acoustic_no_DG.csv")

#BPV$Longitude <- as.numeric(levels(BPV$Longitude))[BPV$Longitude]
#BPV$Latitude <- as.numeric(levels(BPV$Latitude))[BPV$Latitude]

acoustic$NewDate <- substr(acoustic$Date, 0, 7)


#all_nestmonths <- acoustic %>%
 # nest(data= -NewDate) #


different.names <- (!acoustic$Date %in% BPV$Date)
not.in.a2 <- acoustic[different.names,]
not.in.a2$Date <- as.POSIXct(not.in.a2$Date, format="%Y-%m-%d %H:%M:%S")
not.in.a2$Date <- not.in.a2$Date + 1*60*60
combined2 <- merge(not.in.a2, BPV, by.x = "Date", by.y = "Date")


combined <- merge(acoustic, BPV, by.x = "Date", by.y = "Date")



combined$NewDate <- substr(combined$Date, 0, 7)

all_combined <- combined %>%
  nest(data= -NewDate) #


all_overlap <- merge(new_BPV_bind, new_acoustic, by = "Date", all.x = TRUE, allow.cartesian=TRUE)
cols_10 <- c("Date","Longitude_BPV", "Latitude_BPV", "Code", "Longitude_acoustic", "Latitude_acoustic")
colnames(combined) = cols_10

all_overlap <- combined

#df.new <- as.numeric(levels(df.new$Longitude))[df.new$Longitude]
#list2$latitude.fix <- as.numeric(levels(list2$latitude))[list2$latitude]
######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
all_overlap$distance<-distHaversine(all_overlap[,2:3], all_overlap[,5:6], r=r.km)
all_10_overlap <- all_overlap[all_overlap$distance<sep.km,]

write.csv(all_10_overlap, "../results/acoustic/standardised/ALTERED_BPV_10_overlap_no_DG.csv")
overlap <- read.csv("../results/acoustic/all_10_overlap_no_DG.csv")
# Trying to create a forloop to iterate 
all_10_overlap$NewDate <- substr(all_10_overlap$Date, 0, 7)

all_nestmonths <- all_10_overlap %>%
  nest(data= -NewDate) #this is experimenting with 

table(BPV$monthyear)
table <- as.data.frame(table)
cols <- c("monthyear", "freq")
colnames(table) <- cols
BPV$NewDate <- substr(BPV$Date, 0, 7)

summary_tags <- read.csv("acoustic/after_standardisation_summary.csv")
merging <- merge(table, summary_tags, by = "monthyear")


merging$new_standard <- (merging$count / (merging$freq * merging$count_tag * 46) * 100)

summary_tags <- merging

pdf("../results/acoustic/standardised/standardised_OVERLAP_overlaid.pdf")
ggplot(summary_tags, aes(x=month, y=new_standard, fill=year, colour = year, group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  xlab("Month") +# for the x axis label
  ylab("Number of overlaps") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

pdf("../results/acoustic/standardised/standardised_OVERLAP_plots_not_overlaid.pdf")
ggplot(summary_tags, aes(x=monthyear, y=new_standard, group = 1)) + 
  geom_line() +
  geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Number of overlaps") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()





