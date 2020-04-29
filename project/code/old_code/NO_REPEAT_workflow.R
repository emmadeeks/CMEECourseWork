
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




all_10_overlap <- read.csv("../results/acoustic/all_10_overlap_no_DG.csv")

##### THIS IS THE CORRECT COMBINED DATA overlap <- read.csv("../results/acoustic_GPS/NO_REPEAT_AG_all_10_overlap_no_DG.csv")
# Trying to create a forloop to iterate 
all_10_overlap$NewDate <- substr(all_10_overlap$Date, 0, 7)



new_uniq <- all_10_overlap[!duplicated(all_10_overlap[c('Date', 'Code')]),] 
View(new_uniq) 

all_10_overlap = new_uniq

all_nestmonths <- all_10_overlap %>%
  nest(data= -NewDate) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 


summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
pdf("../results/acoustic/no_repeats/all_overlap_no_dg_NOREPEAT.pdf")
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude_acoustic, y= Latitude_acoustic)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(aes(x=Longitude_acoustic,y=Latitude_acoustic)) +
    scale_fill_continuous(type = "viridis", limits = c(0, 100), oob = scales::squish) +
    theme_bw()
  toadd <- c(month, rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
  plot(year_islands_i)
}
dev.off()



 ############## LOGGING BINS
pdf("../results/acoustic/no_repeats/all_overlap_no_dg_NOREPEAT_LOG.pdf")
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude_acoustic, y= Latitude_acoustic)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(aes(fill = stat(log(count)))) +
    scale_fill_continuous(type = "viridis", limits = c(0, 3), oob = scales::squish) +
    theme_bw()
  plot(year_islands_i)
}
dev.off()



##### 
summary <- c("month", "count", "number_sharks")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]
write.csv(summary_sharks,'../results/acoustic/no_repeats/summary_sharks_no_dg_NOREPEATS.csv')
summary <- read.csv('../results/acoustic/no_repeats/summary_sharks_no_dg_NOREPEATS.csv')


################################## FINDING THE TAGS AT LIBERTY ####################

tags_at_liberty <- read.csv("acoustic/standardising_tags.csv")  

summary_tags <- summary
summary_tags$month <- paste0(summary_tags$month, "-01")


summary_tags$count_tag <- 
  sapply(summary_tags$month, function(x)
    sum(as.Date(tags_at_liberty$min, "%Y-%m-%d") <= as.Date(x, "%Y-%m-%d") &
          as.Date(tags_at_liberty$max, "%Y-%m-%d") >= as.Date(x, "%Y-%m-%d")))



################################# FIRST STANDARDISATION FORM ###########################

setwd("/Users/emmadeeks/Dropbox/Overlap_data")
stations <- read.csv("Station_attributes_full.csv")
#station_plot <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
# geom_point(data=stations, aes(x= x, y= y), colour = "Blue", shape = 7) +
#ggtitle("Station") +
#theme_bw()



setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
#acoustic <- read.csv("acoustic_no_DG.csv")
#BPV <- read.csv("BPV_no_DG.csv")



BPV_stations <- read.csv("standard/BPV_stations_10km.csv")

BPV_stations$monthyear<- substr(BPV_stations$Date, 0, 7)
summary_tags$monthyear<- substr(summary_tags$month, 0, 7)


table <- table(BPV_stations$monthyear)
table <- as.data.frame(table)
cols <- c("monthyear", "Boat_station_freq")
colnames(table) <- cols

summary_tags <- merge(summary_tags, table, by = "monthyear")

############################## adding stations 

summary_tags$year <- substr(summary_tags$month, 0, 4)
summary_tags$stations <- ifelse(summary_tags$year == 2013, 29,
                                ifelse(summary_tags$year == 2014, 49, 
                                       ifelse(summary_tags$year == 2015, 64, 93)
                                )
)

############## Standardisation 1 ########
summary_tags$month <- substr(summary_tags$month, 6, 7)
summary_tags$standard1 <- (summary_tags$count / (summary_tags$Boat_station_freq * summary_tags$count_tag))
write.csv(summary_tags,'../results/acoustic/no_repeats/NO_REPEATS_summary_tags_no_dg.csv')

pdf("../results/acoustic/no_repeats/NO_REPEAT_standardised_1_OVERLAP_overlaid.pdf")
ggplot(summary_tags, aes(x=month, y=standard1, fill=year, colour = year, group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to the potential (station~BPV)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

pdf("../results/acoustic/no_repeats/NO_REPEAT_standardised_1_OVERLAP_plots_not_overlaid.pdf")
ggplot(summary_tags, aes(x=monthyear, y=standard1, group = 1)) + 
  geom_line() +
  geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to the potential (station~BPV)") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()


##################### Standardised 2 ###########################



summary_tags$standard2 <- (summary_tags$count / (744 * summary_tags$count_tag))

pdf("../results/acoustic/no_repeats/NO_REPEAT_standardised_2_OVERLAP_overlaid.pdf")
ggplot(summary_tags, aes(x=month, y=standard2, fill=year, colour = year, group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

pdf("../results/acoustic/no_repeats/NO_REPEAT_standardised_2_OVERLAP_plots_not_overlaid.pdf")
ggplot(summary_tags, aes(x=monthyear, y=standard2, group = 1)) + 
  geom_line() +
  geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

######   
