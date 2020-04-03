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

BPV <- read.csv("BPV_no_DG.csv", header = T)
GPS_all <- read.csv("shark_GPS_all.csv", colClasses=c("numeric", "character", "numeric", "numeric"))

cols <- c("Code", "Date", "Longitude", "Latitude")
colnames(GPS_all) <- cols 

BPV <- data.frame(BPV, stringsAsFactors=FALSE)
GPS_all <- data.frame(GPS_all, stringsAsFactors=FALSE)

BPV <- BPV[,-5]
BPV <- BPV[,-1]
GPS_all$Date <- dmy_hm(GPS_all$Date)
GPS_all$Date <- round_date(GPS_all$Date, unit = "hour")

BPV$Date <- as.POSIXct(BPV$Date, format="%Y-%m-%d %H:%M:%S")
BPV$Date <- round_date(BPV$Date, unit = "hour")
#BPV$Longitude <- as.numeric(levels(BPV$Longitude))[BPV$Longitude]
#BPV$Latitude <- as.numeric(levels(BPV$Latitude))[BPV$Latitude]

GPS_all <- GPS_all[with(GPS_all, !((Latitude >= 72.3 & Latitude <= 72.5) | (Longitude >= -7 & Longitude <= -7.6))), ]

m1 <- merge(GPS_all, BPV, by.x = "Date", by.y = "Date")



cols_10 <- c("Date","Code", "Longitude_GPS", "Latitude_GPS", "Longitude_BPV", "Latitude_BPV")

colnames(m1) = cols_10





r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 20
m1$distance<-distHaversine(m1[,4:3], m1[,5:6], r=r.km)
GPS_10_overlap <- m1[m1$distance<sep.km,]


#trying3 <- GPS_10_overlap[with(GPS_10_overlap, !((Latitude_GPS >= 72.3 & Latitude_GPS <= 72.5) | (Longitude_GPS >= -7 & Longitude_GPS <= -7.6))), ]




pdf("../../results/satellite/overlap_20_hour.pdf")
ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  geom_point(data=GPS_10_overlap, aes(x= Longitude_BPV, y= Latitude_BPV),size=2, pch = 21, colour = "Blue", fill = "Blue") +
  geom_point(data=GPS_10_overlap, aes(x= Latitude_GPS, y= Longitude_GPS),size=2, pch = 21, colour = "Red", fill = "Red")
dev.off()


################ Rounding overlap by day 

BPV$Day <- substr(BPV$Date, 0, 10)
GPS_all$Day <- substr(GPS_all$Date, 0, 10)
m1_day <- merge(GPS_all, BPV, by.x = "Day", by.y = "Day")
cols_10 <- c("Day","Code", "Date", "Longitude_GPS", "Latitude_GPS", "Date", "Longitude_BPV", "Latitude_BPV")
colnames(m1_day) = cols_10

m1_day$distance<-distHaversine(m1_day[,5:4], m1_day[,7:8], r=r.km)
GPS_10_overlap_day <- m1_day[m1_day$distance<sep.km,]

GPS_10_overlap_day <- GPS_10_overlap_day[,-3]


pdf("../../results/satellite/overlap_20_DAY_GPS_DG.pdf")
ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  geom_point(data=GPS_10_overlap_day, aes(x= Latitude_GPS, y= Longitude_GPS),size=2, pch = 21, colour = "Red", fill = "Red")
dev.off()

pdf("../results/satellite/HEX_MPA_overlap_20_DAY_GPS_DG.pdf")
ggplot() + geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  xlim(71,73) +
  ylim(-8, -5) +
  geom_hex(data=overlap, aes(x= Latitude_GPS, y= Longitude_GPS))
  #geom_point(data=overlap, aes(x= Latitude_GPS, y= Longitude_GPS),size=2, pch = 21, colour = "Red", fill = "Red")
dev.off()

####################### year plot 

GPS_10_overlap_day$NewDate <- substr(GPS_10_overlap_day$Date, 0, 7)

write.csv(GPS_10_overlap_day, "no_dg_GPS_10_day_overlap.csv")

all_nestmonths <- GPS_10_overlap_day %>%
  nest(data= -NewDate) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 



pdf("../results/satellite/all_out_GPS_BPV_no_dg.pdf")
#summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude_BPV, y= Latitude_BPV)) + 
    geom_point() +
    geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(data=monthdata, aes(x= Latitude_GPS, y= Longitude_GPS)) +
    theme_bw()  
  plot(year_islands_i)
}
dev.off()



pdf("../results/satellite/all_out_GPS_no_dg.pdf")
summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude_BPV, y= Latitude_BPV)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(aes(x=Latitude_GPS,y=Longitude_GPS)) +
    scale_fill_continuous(type = "viridis", limits = c(0, 150), oob = scales::squish) +
    theme_bw()
  toadd <- c(month, rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
  plot(year_islands_i)
}
dev.off()



summary <- c("month", "count", "number_sharks")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]
write.csv(summary_sharks,'../results/satellite/GPS_summary_sharks_no_dg.csv')
summary <- read.csv('../results/satellite/GPS_summary_sharks_no_dg.csv')



GPS_all$NewDate <- substr(GPS_all$Date, 0, 4)

nest <- GPS_all %>%
  nest(data= -NewDate)

thir <- nest$data[[1]]
four <- nest$data[[2]]
eight <- nest$data[[3]]



y141 <- summary[grep("2014", summary$month),]
y141$cumulative <- 7

y151 <- summary[grep("2018", summary$month),]
y151$cumulative <- 5


summary_tags <- rbind(y141, y151)


setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data")
write.csv(summary_tags,'GPS_summary_tags_no_dg.csv')
#write.csv(all_10_overlap, "all_overlap_10.csv")



summary_tags <- read.csv('GPS_summary_tags_no_dg.csv', header = T)

summary_tags$standard <- (summary_tags$count / summary_tags$cumulative) * 100


summary_tags$year <- substr(summary_tags$month, 0, 4)
summary_tags$month <- substr(summary_tags$month, 6, 7)

pdf("../results/satellite/GPS_overlaid_years_counts_no_dg.pdf")
ggplot(summary_tags, aes(x=month, y=standard, fill=year, colour = year, group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  xlab("Month") +# for the x axis label
  ylab("Number of overlaps") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

pdf("../results/satellite/GPS_standardised_plots_not_overlaid_no_dg.pdf")
ggplot(summary_tags, aes(x=month, y=standard, group = 1)) + 
  geom_line() +
  geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Number of overlaps") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()



