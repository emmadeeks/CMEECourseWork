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

  
setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 

acoustic <- read.table("/Users/emmadeeks/Dropbox/Overlap_data/Chagos_ALL_acoustic_2019.txt", header = TRUE, sep = ",", dec = ".") #read in the data 
BPV <- read.csv("BPV Pacific Marlin 2013-2016.csv", header = TRUE) # read in the data

acoustic$detect_date <- as.POSIXct(acoustic$detect_date, format="%Y-%m-%d %H:%M:%S")
acoustic$detect_date <- round_date(acoustic$detect_date, unit = "hour")

BPV$Date <- dmy_hms(BPV$Date)
BPV$Date <- round_date(BPV$Date, unit = "hour")


#THIS DATA NOW WORKS 
BPV2 <- read.csv("BPV_Position_Data_Combined_2017_2019.csv", header = TRUE, stringsAsFactors = T) # read in the data


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
#df.new <- df.new[,-2]
#df.new <- df.new[,-2]

new_BPV_bind <- rbind(new_BPV, df.new)
###################### 
#new_BPV_1 <- new_BPV[,-4]
#df.new_1 <- df.new[,-4]
#new_BPV_bind <- rbind(new_BPV_1, df.new_1)
#new_acoustic <- new_acoustic[,-4]

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

write.csv(all_10_overlap, "all_10_overlap.csv")
# Trying to create a forloop to iterate 
all_10_overlap$NewDate <- substr(all_10_overlap$Date, 0, 7)

all_nestmonths <- all_10_overlap %>%
  nest(data= -NewDate) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 



pdf("all_out_acoustic.pdf")
summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude_acoustic, y= Latitude_acoustic)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(aes(x=Longitude_acoustic,y=Latitude_acoustic)) +
    scale_fill_continuous(type = "viridis", limits = c(0, 2000), oob = scales::squish) +
    theme_bw()
  toadd <- c(month, rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
  plot(year_islands_i)
}
dev.off()



############## LOGGING BINS
pdf("all_out_acoustic_LOG.pdf")
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude_acoustic, y= Latitude_acoustic)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(aes(fill = stat(log(count)))) +
    scale_fill_continuous(type = "viridis", limits = c(0, 7.5), oob = scales::squish) +
    theme_bw()
  plot(year_islands_i)
}
dev.off()



##### 
summary <- c("month", "count", "number_sharks")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]
write.csv(summary_sharks,'summary_sharks.csv')
summary <- read.csv('summary_sharks.csv')

# Plotting summary without log 
pdf("month_counts_NO_LOG.pdf")
ggplot(summary, aes(x=as.factor(summary$month), y=summary$count, group=1)) + geom_path() +
theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()
# Plotting summary with log 
summary$new <- log(summary$count)
pdf("month_counts_LOG.pdf")
ggplot(summary, aes(x=as.factor(summary$month), y=summary$new, group=1)) + geom_path() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()


setwd("/Users/emmadeeks/Dropbox/Overlap_data")
tags <- read.csv('Chagos_ALL_tag_metadata_Apr2018.csv')
tags19 <- read.csv('Chagos_2019_tagging_EMMA_DEEKS.csv')

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data/")
tag_additions = as.data.frame(matrix(nrow = 1, ncol = 3))
y13 <- nrow(tags[grep("2013", tags$taggingdate),])
y14 <- nrow(tags[grep("2014", tags$taggingdate),])
y15 <- nrow(tags[grep("2015", tags$taggingdate),])
y16 <- nrow(tags[grep("2016", tags$taggingdate),])
y17 <- nrow(tags[grep("2017", tags$taggingdate),])
y18 <- nrow(tags[grep("2018", tags$taggingdate),])
y19 <- nrow(tags19)

y141 <- summary[grep("2014", summary$month),]
y141$X <- y14
y141$cumulative <- 30

y151 <- summary[grep("2015", summary$month),]
y151$X <- y15
y151$cumulative <- 86

y161 <- summary[grep("2016", summary$month),]
y161$X <- y16
y161$cumulative <- 220
 
y171 <- summary[grep("2017", summary$month),]
y171$X <- y17
y171$cumulative <- 220

y181 <- summary[grep("2018", summary$month),]
y181$X <- y18
y181$cumulative <- 291

y191 <- summary[grep("2019", summary$month),]
y191$X <- y19
y191$cumulative <- 357

summary_tags <- rbind(y141, y151, y161, y171, y181, y191)

tag_names <- c("tags_added", "month" ,"count", "number_sharks", "cumulative_tags")
colnames(summary_tags) <- tag_names

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data")
write.csv(summary_tags,'summary_tags.csv')
#write.csv(all_10_overlap, "all_overlap_10.csv")








p <- ggplot(overlap_18, aes(x= Longitude, y= Latitude)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point()

p + stat_density2d()

p + stat_density2d(aes(fill=..level..), geom="polygon",bins=12)+
  scale_fill_gradient(low="skyblue2", high="firebrick1", name="Distribution")

a <- p +  geom_density2d() + stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon') +
     scale_fill_gradient(low = "green", high = "red") +
     scale_alpha(range = c(0.00, 0.25), guide = FALSE)



all[grep("59999", all$Code), ]

