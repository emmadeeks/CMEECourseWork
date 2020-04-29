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
acoustic <- read.csv("acoustic_no_DG_elas.csv")
BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_no_dg.csv")



#### subsetting the routes of the BPV by year 2014 and then 2018. 

########### Doing the same for 2015 as that has all the months 
BPV_2014 <- filter(BPV, grepl("2014",Date))
BPV_2018 <- filter(BPV, grepl("2014",Date))

BPV_2018$date18 <- 2018
BPV_2018$sub <- substr(BPV_2018$Date, 5, 19)
BPV_2018$date18 <- paste(BPV_2018$date18,BPV_2018$sub, sep="")
BPV_2018$date18 <- as.POSIXct(BPV_2018$date18, format="%Y-%m-%d %H:%M:%S")
BPV_2018_2 <- data.table(BPV_2018$date18, BPV_2018$Longitude, BPV_2018$Latitude)
########## NOW WHAT HAS HAPPENED IS THE 2018 HAS BEEN TURNED TO 2014 DATA SO WE CAN RUN THE SIMULATION ON IT 

cols <- c("Date", "Longitude", "Latitude")
colnames(BPV_2018_2) <- cols

#acoustic <- acoustic[,-1] #x2 

########### we now have the 2018 data as 2014  so next we are going to apply 
### the 2014 route on the 2018 acoustic monitoring 
acoustic$NewDate <- substr(acoustic$Date, 0, 7)
BPV_2018_2$Date <- as.POSIXct(BPV_2018_2$Date, format="%Y-%m-%d %H:%M:%S")
acoustic$Date <- as.POSIXct(acoustic$Date, format="%Y-%m-%d %H:%M:%S")


df_new <- left_join(BPV_2018_2, acoustic, by = 'Date')

all_overlap <- df_new


r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
all_overlap$distance<-distHaversine(all_overlap[,2:3], all_overlap[,4:5], r=r.km)
all_10_overlap <- all_overlap[all_overlap$distance<sep.km,]
overlap_10 <- filter(all_overlap, distance < sep.km)




all_nestmonths <- overlap_10 %>%
  nest(data= -NewDate) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that

#setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 


#setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 


summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  toadd <- c(month, rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
}


summary <- c("month", "count", "number_sharks")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]
#summary_sharks <- summary_sharks[-5,]

summary <- read.csv('../results/acoustic/summary_tags_no_dg.csv')

original <- summary[38:47,]

rowwiwant <- original[,8]

simulation_2 <- cbind(summary_sharks, rowwiwant)

simulation_2$new <- (as.numeric(simulation_2$count) / (744 * as.numeric(simulation_2$rowwiwant)))


new <- original[,13]

original2_2 <- cbind(simulation_2, new)
original2_2 <- original2_2[,-5]

#names <- c("month", "count", "number_sharks", "rowwiwant", "new")
#colnames(simulation) <- names

newgo <- rbind(original2_2, simulation_2)

newgo$cat <- c(2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014)
newgo$months <- substr(newgo$month, 6, 7)

pdf("../results/acoustic/no_repeats/simulation1_2018_14.pdf", onefile = TRUE)
ggplot(newgo, aes(x=months, y=new, group=as.factor(cat), colour = as.factor(cat))) +
  geom_point() +
  geom_line(size=1) +
  ggtitle("Overlap score on 2018 acoustic data with BPV route from 2018 and 2014") +
  xlab("Month") +# for the x axis label
  ylab("Number of overlaps") +
  scale_colour_manual(name = "BPV route year", labels = c("2014 BPV route (sim)", "2018 BPV route (original)"), values = c('#E69F00','#999999')) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
dev.off()


############ THE CORRECT WAY ROUND 


########### Doing the same for 2015 as that has all the months 

BPV_2014 <- filter(BPV, grepl("2014",Date))
BPV_2015 <- filter(BPV, grepl("2014",Date))

BPV_2015$date15 <- 2015
BPV_2015$sub <- substr(BPV_2015$Date, 5, 19)
BPV_2015$date15 <- paste(BPV_2015$date15,BPV_2015$sub, sep="")
BPV_2015$date15 <- as.POSIXct(BPV_2015$date15, format="%Y-%m-%d %H:%M:%S")
BPV_2015_2 <- data.table(BPV_2015$date15, BPV_2015$Longitude, BPV_2015$Latitude)
########## NOW WHAT HAS HAPPENED IS THE 2018 HAS BEEN TURNED TO 2014 DATA SO WE CAN RUN THE SIMULATION ON IT 

cols <- c("Date", "Longitude", "Latitude")
colnames(BPV_2015_2) <- cols

#acoustic <- acoustic[,-1] #x2 

########### we now have the 2018 data as 2014  so next we are going to apply 
### the 2014 route on the 2018 acoustic monitoring 
#acoustic$NewDate <- substr(acoustic$Date, 0, 7)
BPV_2015_2$Date <- as.POSIXct(BPV_2015_2$Date, format="%Y-%m-%d %H:%M:%S")
acoustic$Date <- as.POSIXct(acoustic$Date, format="%Y-%m-%d %H:%M:%S")


df_new <- left_join(BPV_2015_2, acoustic, by = 'Date')

all_overlap <- df_new


r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
all_overlap$distance<-distHaversine(all_overlap[,2:3], all_overlap[,4:5], r=r.km)
all_10_overlap <- all_overlap[all_overlap$distance<sep.km,]
overlap_10 <- filter(all_overlap, distance < sep.km)




all_nestmonths <- overlap_10 %>%
  nest(data= -NewDate) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that

#setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 


#setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 


summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  toadd <- c(month, rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
}


summary <- c("month", "count", "number_sharks")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]
summary_sharks <- summary_sharks[-4,]

summary <- read.csv('../results/acoustic/summary_tags_no_dg.csv')

original <- summary[13:24,]

rowwiwant <- original[,8]

simulation <- cbind(summary_sharks, rowwiwant)

simulation$new <- (as.numeric(simulation$count) / (744 * as.numeric(simulation$rowwiwant)))


new <- original[,13]

original2 <- cbind(simulation, new)
original2 <- original2[,-5]

#names <- c("month", "count", "number_sharks", "rowwiwant", "new")
#colnames(simulation) <- names

newgo <- rbind(original2, simulation)

newgo$cat <- c(2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2015, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014)
newgo$months <- substr(newgo$month, 6, 7)

pdf("../results/acoustic/no_repeats/simulation2_2015_14.pdf", onefile = TRUE)
ggplot(newgo, aes(x=months, y=new, group=as.factor(cat), colour = as.factor(cat))) + geom_line(size=1) + 
  geom_point() +
  ggtitle("Overlap score on 2015 acoustic data with BPV route from 2015 and 2014") +
  xlab("Month") +# for the x axis label
  ylab("Number of overlaps") +
  scale_colour_manual(name = "BPV route year", labels = c("2014 BPV route (sim)", "2015 BPV route (original)"), values = c('#E69F00','#999999')) +
  xlab("Month") +# for the x axis label
  ylab("Number of overlaps") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

