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

pdf("../results/chagos_mpa.pdf")
ggplot() + 
  geom_point() +
  geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()



################# acoustic and BPV 1
acoustic <- read.table("/Users/emmadeeks/Dropbox/Overlap_data/Chagos_ALL_acoustic_2019.txt", header = TRUE, sep = ",", dec = ".") #read in the data 
BPV <- read.csv("BPV Pacific Marlin 2013-2016.csv", header = TRUE) # read in the data


BPV$date <- dmy_hms(BPV$Date)
BPV$Date <- round_date(BPV$date, unit = "hour")

toadd = as.data.frame(matrix(nrow = 1, ncol = 3))
col <- c("Date", "lon", "lat")
colnames(toadd) <- col
############################# calculating mid long and lat for pacific marlin BPV vessel 


for (i in 1:nrow(BPV)){
  row1 <- BPV[i,]
  row2 <- BPV[i+1,]
  midpoint <- midPoint(c(row1$Longitude, row1$Latitude), c(row2$Longitude, row2$Latitude))
  date <- row1$Date + 1*60*60
  whole <- cbind(as.character(date), midpoint)
  colnames(whole) <- col
  toadd <- rbind(toadd, whole)

}

toadd <- toadd[-1,]

cols_BPV <- c("Date","Longitude", "Latitude")
new_BPV <- data.table(BPV$Date, BPV$Longitude, BPV$Latitude)
toadd_table <- data.table(toadd)
colnames(new_BPV) = cols_BPV
colnames(toadd_table) = cols_BPV

toadd_table$Date <- as.POSIXct(toadd_table$Date, format="%Y-%m-%d %H:%M:%S")


#This is now a datatable with the mid points for the data frame 
BPV_moretimes <- rbindlist(list(new_BPV,toadd_table), use.names=TRUE)

############# reading the acoustic data in and rounding up 

acoustic$detect_date <- as.POSIXct(acoustic$detect_date, format="%Y-%m-%d %H:%M:%S")
acoustic$new_date <- round_date(acoustic$detect_date, unit = "hour")

#BPV$date <- dmy_hms(BPV$Date)
#BPV$Date <- round_date(BPV$date, unit = "hour")
################################### BPV 2


#THIS DATA NOW WORKS 
BPV2 <- read.csv("BPV_Position_Data_Combined_2017_2019.csv", header = TRUE, stringsAsFactors = T) # read in the data


######## ALL data but not the new BPV YET ############
cols <- c("Date","Longitude", "Latitude", "Code", "station")
new_acoustic <- data.table(acoustic$new_date, acoustic$receiver_lon, acoustic$receiver_lat, acoustic$code, acoustic$station)
colnames(new_acoustic) = cols #assigning column names 

#cols_BPV <- c("Date","Longitude", "Latitude")
#new_BPV <- data.table(BPV$Date, BPV$Longitude, BPV$Latitude, BPV$date)
#colnames(new_BPV) = cols_BPV


######### FORMATTING BPV2 ############

cols_BPV <- c("Date","Longitude", "Latitude")
BPV2$newDate <- paste(BPV2$Date, BPV2$Time)
BPV2$Date <- paste(BPV2$Date, BPV2$Time)
new_BPV_2 <- data.table(BPV2$Date, BPV2$DEC_LON, BPV2$DEC_LAT)
colnames(new_BPV_2) = cols_BPV

new_BPV_2$Date <- dmy_hms(new_BPV_2$Date)
new_BPV_2$Date <- round_date(new_BPV_2$Date, unit = "hour")
#df.new = new_BPV_2[seq(1, nrow(new_BPV_2), 24), ]
#df.new$olddate <- dmy_hms(df.new$olddate)

new_uniq <- new_BPV_2[!duplicated(new_BPV_2[,1]),] 
#View(new_uniq) 

formatted_BPV2 <- rbind(BPV_moretimes, new_uniq)
#new_BPV_bind <- rbind(new_BPV, df.new)
#bind_add_dates <- rbind(BPV_moretimes, df.new)

########### excluding diego garcia 
#bind_add_dates$Longitude <- as.numeric(levels(bind_add_dates$Longitude))[bind_add_dates$Longitude]
#bind_add_dates$Latitude <- as.numeric(levels(bind_add_dates$Latitude))[bind_add_dates$Latitude]
#BPV_formatted_times <- bind_add_dates[with(bind_add_dates, !((Longitude >= 72.3 & Longitude <= 72.5) | (Latitude >= -7 & Latitude <= -7.6))), ]

################## This is the one being used for the analysis 
formatted_BPV2$Longitude <- as.numeric(levels(formatted_BPV2$Longitude))[formatted_BPV2$Longitude]
formatted_BPV2$Latitude <- as.numeric(levels(formatted_BPV2$Latitude))[formatted_BPV2$Latitude]
#BPV_formatted_times <- formatted_BPV2[with(formatted_BPV2, !((Longitude >= 72.3 & Longitude <= 72.5) | !(Latitude >= -7 & Latitude <= -7.6))), ]
DG <- formatted_BPV2%>% filter(between(Latitude, -7.6, -7.0))
within_dg <- DG%>% filter(between(Longitude, 72.3, 72.5))

formatted_BPV2_nodg <- formatted_BPV2[!(formatted_BPV2$Date %in% within_dg$Date),]



#new_BPV_bind$Longitude <- as.numeric(levels(new_BPV_bind$Longitude))[new_BPV_bind$Longitude]
#new_BPV_bind$Latitude <- as.numeric(levels(new_BPV_bind$Latitude))[new_BPV_bind$Latitude]
#BPV_formatted <- new_BPV_bind[with(new_BPV_bind, !((Longitude >= 72.3 & Longitude <= 72.5) | (Latitude >= -7 & Latitude <= -7.6))), ]

#acoustic_formatted <- new_acoustic[with(new_acoustic, !((Longitude >= 72.3 & Longitude <= 72.5) | (Latitude >= -7 & Latitude <= -7.6))), ]


#write.csv(BPV_formatted, "BPV_no_DG.csv")
#write.csv(new_BPV_bind, "BPV_formatted_times.csv") # This data is not every hour so is wrong 
#write.csv(new_acoustic, "acoustic_formatted_times.csv")
#write.csv(acoustic_formatted, "acoustic_no_DG.csv")
write.csv(formatted_BPV2_nodg, "New_data_no_dg_hour/BPV_formatted_CORRECT_hour_NO_dg.csv")
write.csv(formatted_BPV2, "New_data_no_dg_hour/BPV_formatted_CORRECT_hour_INCLUDE_dg.csv")
################## 
############################# MAKING DATA JUST ELASMIOBRANCHS #####################

acoustic <- new_acoustic
#acoustic <- read.csv("acoustic_formatted_times.csv")
tags_to_delete <- c(54879, 1344, 1334, 1338, 1790024, 1339, 1330, 28648, 28651, 54960, 27612, 27587, 54963, 14920, 2386)


for (i in 1:length(tags_to_delete)){
  i = tags_to_delete[i]
  acoustic <- acoustic[!grepl(i, acoustic$Code), ]
}


write.csv(acoustic, "acoustic_no_elas.csv")


########################### NOW WE ARE MOVING ONTO COMBINING THE GPS TAG DATA 

#acoustic <- read.csv("acoustic_no_elas.csv")
#acoustic <- acoustic[,-1]
#acoustic <- acoustic[,-1]
#acoustic <- acoustic[,-5]

GPS_all <- read.csv("shark_GPS_all.csv", colClasses=c("numeric", "character", "numeric", "numeric"))
GPS_all$station <- "GPS"

cols <- c("Code", "Date", "Latitude", "Longitude", "station")
colnames(GPS_all) <- cols 

GPS_all <- data.frame(GPS_all, stringsAsFactors=FALSE)

GPS_all$Date <- dmy_hm(GPS_all$Date)
GPS_all$Date <- round_date(GPS_all$Date, unit = "hour")

############ rearranging as got the pngitudes the wrong way round 
GPS <- cbind(as.character(GPS_all$Date), GPS_all$Longitude, GPS_all$Latitude, GPS_all$Code, GPS_all$station)
GPS <- data.frame(GPS, stringsAsFactors=FALSE)
acoustic <- data.frame(acoustic, stringsAsFactors=FALSE)
cols <- c("Date","Longitude", "Latitude", "Code", "station")
colnames(GPS) <- cols 



GPS$Date <- as.POSIXct(GPS$Date, format="%Y-%m-%d %H:%M:%S")
acoustic$Date <- as.POSIXct(acoustic$Date, format="%Y-%m-%d %H:%M:%S")
GPS_acoustic <- rbind(acoustic, GPS)

write.csv(GPS_acoustic, "New_data_no_dg_hour/acoustic_GPS_no_elas.csv")


