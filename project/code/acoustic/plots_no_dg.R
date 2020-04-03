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
acoustic <- read.csv("acoustic_no_DG.csv")
BPV <- read.csv("BPV_no_DG.csv")
#BPV$Longitude <- as.numeric(levels(BPV$Longitude))[BPV$Longitude]
#BPV$Latitude <- as.numeric(levels(BPV$Latitude))[BPV$Latitude]

acoustic$NewDate <- substr(acoustic$Date, 0, 7)


all_nestmonths <- acoustic %>%
  nest(data= -NewDate) #


combined <- merge(acoustic, BPV, by.x = "Date", by.y = "Date")

all_combined <- combined %>%
  nest(data= -NewDate) #

############# PLOTTING INITIAL DATA TO HAVE A LOOK AT SPATIAL DISTRIBUTION #####

pdf("../results/acoustic/EVERY_acoustic_BPV_nodg.pdf")
for (i in 1:length(all_combined$NewDate)){
  monthdata <- all_combined$data[[i]]
  month <- all_combined$NewDate[[i]]
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude.y, y= Latitude.y)) + 
    geom_point() +
    geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(data=monthdata, aes(x= Longitude.x, y= Latitude.x, fill = stat(log(count)))) +
    scale_fill_continuous(type = "viridis", limits = c(0, 10), oob = scales::squish) +
    theme_bw() + 
    geom_density2d() + stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon') +
    #scale_fill_gradient(low = "green", high = "red") +
    scale_alpha(range = c(0.00, 0.25), guide = FALSE)
  plot(year_islands_i)
}
dev.off()

########## START LOOKING AT OVERLAP #################
combined <- combined[,-2]
combined <- combined[,-5]
combined <- combined[,-5]
combined <- combined[,-7]

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

write.csv(all_10_overlap, "../results/acoustic/all_10_overlap_no_DG.csv")
overlap <- read.csv("../results/acoustic/all_10_overlap_no_DG.csv")
# Trying to create a forloop to iterate 
all_10_overlap$NewDate <- substr(all_10_overlap$Date, 0, 7)

all_nestmonths <- all_10_overlap %>%
  nest(data= -NewDate) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 



pdf("../results/acoustic/all_out_acoustic_no_dg.pdf")
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
pdf("../results/acoustic/all_out_acoustic_LOG_no_dg.pdf")
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
write.csv(summary_sharks,'../results/acoustic/summary_sharks_no_dg.csv')
summary <- read.csv('../results/acoustic/summary_sharks_no_dg.csv')


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
write.csv(summary_tags,'summary_tags_no_dg.csv')
#write.csv(all_10_overlap, "all_overlap_10.csv")



summary_tags <- read.csv('summary_tags_no_dg.csv', header = T)

summary_tags$standard <- (summary_tags$count / summary_tags$cumulative_tags) * 100


summary_tags$year <- substr(summary_tags$month, 0, 4)
summary_tags$months <- substr(summary_tags$month, 6, 7)

pdf("../results/acoustic/overlaid_years_counts_no_dg.pdf")
ggplot(summary_tags, aes(x=months, y=standard, fill=year, colour = year, group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  xlab("Month") +# for the x axis label
  ylab("Number of overlaps") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

pdf("../results/acoustic/standardised_plots_not_overlaid_no_dg.pdf")
ggplot(summary_tags, aes(x=month, y=standard, group = 1)) + 
  geom_line() +
  geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Number of overlaps") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

lm(standard~X, data = summary_tags)
summary(lm(summary_tags$standard ~ poly(summary_tags$X, 3, raw = TRUE)))

######################## OVERLAYING BPV MONTHS #################
BPV$NewDate <- substr(BPV$Date, 0, 7)
BPV$year <- substr(BPV$Date, 0, 4)
BPV$month <- substr(BPV$Date, 6, 7)

nest_BPV <- BPV %>%
  nest(data= -month)

pdf("../results/acoustic/stat_each_month_BPV_plotted_compare.pdf")
for (i in 1:length(nest_BPV$month)){
  monthdata <- nest_BPV$data[[i]]
  month <- nest_BPV$month[[i]]
  year_plot_i <- ggplot(monthdata, aes(x=Longitude, y=Latitude, fill=year, colour = year, group=factor(year))) + 
    geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    geom_point(size = 1, shape=21) +
    ggtitle(month) +
    xlab("Month") +# for the x axis label
    ylab("Number of overlaps") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    theme_bw()
  plot(year_plot_i + stat_density_2d(aes(x=Longitude, y=Latitude, fill=year, color = year), geom="polygon", bins = 4, alpha=.1))
}

dev.off()









