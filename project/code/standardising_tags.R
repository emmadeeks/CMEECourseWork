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



setwd("/Users/emmadeeks/Dropbox/Overlap_data")
tags2018 <- read.csv("Chagos_ALL_tag_metadata_Apr2018.csv", header = T)
tags2019 <- read.csv("Chagos_2019_tagging_EMMA_DEEKS.csv", header = T)
setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data")
summary_tags <- read.csv('summary_tags.csv', header = T)

summary_tags$standard <- (summary_tags$count / summary_tags$cumulative_tags) * 100


summary_tags$year <- substr(summary_tags$month, 0, 4)
summary_tags$month <- substr(summary_tags$month, 6, 7)

pdf("../results/overlaid_years_counts.pdf")
ggplot(summary_tags, aes(x=month, y=standard, fill=year, colour = year, group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  xlab("Month") +# for the x axis label
  ylab("Number of overlaps") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

pdf("../results/standardised_plots_not_overlaid.pdf")
ggplot(summary_tags, aes(x=month, y=standard, group = 1)) + 
  geom_line() +
  geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Number of overlaps") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()



pdf("../results/year_plot_overlap.pdf")
ggplot(summary_tags, aes(x=month, y=standard)) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  xlab("Month") +# for the x axis label
  ylab("Number of overlaps") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()


pdf("../results/years_boxplot_month.pdf")
boxplot(standard~month, data = summary_tags)
dev.off()

boxplot(standard~month, data = summary_tags)

modified <- read.csv("all_sharks_numbers_modified.csv", header = T)
modified[modified==0] <- NA 

rownames(modified) <- modified[,1]
modified[,1] <- NULL


bxpdat <- boxplot(modified[1:51], ylim = c(0,200))
text(bxpdat$group,                                              # the x locations 
     bxpdat$out,                                                # the y values
     rownames(modified)[which(modified == bxpdat$out, arr.ind=TRUE)[, 1]],  # the labels
     pos = 4) 


which(modified == bxpdat$out, arr.ind=TRUE)
# it is 59994, 59984, 54958
#      row col
#59994 178  19
#59984 170  26
#54958 141  49

############## boxplot missing values #########



is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

dat <- mtcars %>% tibble::rownames_to_column(var="outlier") %>% group_by(cyl) %>% mutate(is_outlier=ifelse(is_outlier(drat), drat, as.numeric(NA)))
dat$outlier[which(is.na(dat$is_outlier))] <- as.numeric(NA)

ggplot(dat, aes(y=drat, x=factor(cyl))) + geom_boxplot() + geom_text(aes(label=outlier),na.rm=TRUE,nudge_y=0.05)
        