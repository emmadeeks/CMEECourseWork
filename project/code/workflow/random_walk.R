rm(list=ls()) #Clear global environment 
setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data/shape_files/")




#install.packages("remotes")
#remotes::install_github("jsta/glatos")
library(glatos)
library(dplyr)
library(ggplot2)
library(raster) #Require these packages 
library(sf)     #Require 
library(viridis)
library('units')
library('rgdal')
library(raster) #Require these packages 
library(sf)     #Require 
library(viridis)
library('units')
library(sp)
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
library(tidyverse)
library("wesanderson")



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


my_polygon <- crw_in_polygon(spdf, initPos=c(72.5,-7.5), stepLen = 20000, initHeading = 180, nsteps = 303, sp_out = F)
#theta = c(0,90)
#fort <- as.data.frame(my_polygon)


ggplot(data=my_polygon, aes(x= x, y= y)) +
  geom_point() +
  #geom_line() +
  geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) 


# https://rdrr.io/github/jsta/glatos/man/crw_in_polygon.html
######### initPos = DG coordinate thats where it starts 
### polyg = That is the patial coordinate of the entire MPA 
### stepLen = 20km per hour but i believe this is in meters 
########## nsteps = average amount of steps BPV takes per month - take an average for number of steps or is it just 744? 
##########

############## Isolating months of interest 

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
GPS_acoustic <- read.csv("New_data_no_dg_hour/acoustic_GPS_no_elas.csv")
BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_no_dg.csv")

GPS_acoustic$NewDate <- substr(GPS_acoustic$Date, 0 , 7)

high_month_BPV <- BPV[BPV$NewDate == '2014-12',]
high_month_ac <- GPS_acoustic[GPS_acoustic$NewDate == '2014-12',]

months <- high_month_BPV$Date
overlap <- cbind(my_polygon, months)
names <- c("Longitude", "Latitude", "Date")
colnames(overlap) <- names

m1 <- merge(overlap, high_month_ac, by = "Date")
all_overlap <- m1

#df.new <- as.numeric(levels(df.new$Longitude))[df.new$Longitude]
#list2$latitude.fix <- as.numeric(levels(list2$latitude))[list2$latitude]
######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 20
all_overlap$distance<-distHaversine(all_overlap[,2:3], all_overlap[,5:6], r=r.km)
all_10_overlap <- all_overlap[all_overlap$distance<sep.km,]

all_10_overlap <- all_10_overlap[!duplicated(all_10_overlap[c('Date', 'Code')]),] 

summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
#pdf("../results/acoustic_GPS/AG_NR_all_overlap_no_dg_NOREPEAT.pdf")
  monthdata <- all_10_overlap
  month <- "2014-12_random_BPV"
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    geom_point(data=monthdata, aes(x= Longitude.x, y= Latitude.x), colour = "red") +
    ggtitle(month) +
    #geom_hex(aes(x=Longitude.x,y=Latitude.x)) +
    #scale_fill_continuous(type = "viridis", limits = c(0, 100), oob = scales::squish) +
    coord_equal() +
    ggsn::scalebar(Chagos_island,transform = T, dist = 50, dist_unit = "km", model = 'WGS84') +
    theme_bw()
  toadd <- c(month, rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
#dev.off()
  summary <- c("month", "count", "number_sharks")
  colnames(summary_sharks) <- summary
  summary_sharks <- summary_sharks[-1,]
  
summary_original <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')
original_2k14 <- summary_original[12,]
counts <- c(5, 36)
months <- c("random_BPV", "original_BPV")
to_plot <- data.frame(months, counts)

pdf("../results/acoustic_GPS/random_walk/2k14_12_random_walk_No-repeat.pdf")
barplot(to_plot$counts, main = "2014-12 acoustic data overlap with random walk compared to original BPV route", ylab = "Counts of overlap", col="#69b3a2", names.arg = c("random_BPV", "original_BPV"))
dev.off()
######## nice way of plotting islands 
sp::plot(chagos_v6, col = "lightgrey", border = "grey")




############## ACOUSTIC ARRAY 

#theta = c(0,90)
#fort <- as.data.frame(my_polygon)




x_coord <- c(71, 71, 73, 73)
y_coord <- c(-4.5, -7.5, -7.5, -4.5)
xym <- cbind(x_coord, y_coord)
xym

p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
plot(sps)

proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
data = data.frame(f=99.9)
spdf = SpatialPolygonsDataFrame(sps, data)


centre <- BPV%>% filter(between(Latitude, -7.5, -4.5))
centre_nodg <- centre%>% filter(between(Longitude, 71, 73))


high_month_BPV <- centre_nodg [centre_nodg$NewDate == '2014-12',]
high_month_ac <- GPS_acoustic[GPS_acoustic$NewDate == '2014-12',]

my_polygon <- crw_in_polygon(spdf, initPos=c(72.5,-7.4), theta = c(0,40), stepLen = 20000, initHeading = 180, nsteps = 253, sp_out = F)
#theta = c(0,90)
#fort <- as.data.frame(my_polygon)


ggplot(data=my_polygon, aes(x= x, y= y)) +
  geom_point() +
  #geom_line() +
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) 

months <- high_month_BPV$Date
overlap <- cbind(my_polygon, months)
names <- c("Longitude", "Latitude", "Date")
colnames(overlap) <- names

m1 <- merge(overlap, high_month_ac, by = "Date")
all_overlap <- m1

#df.new <- as.numeric(levels(df.new$Longitude))[df.new$Longitude]
#list2$latitude.fix <- as.numeric(levels(list2$latitude))[list2$latitude]
######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 20
all_overlap$distance<-distHaversine(all_overlap[,2:3], all_overlap[,5:6], r=r.km)
all_10_overlap <- all_overlap[all_overlap$distance<sep.km,]

all_10_overlap <- all_10_overlap[!duplicated(all_10_overlap[c('Date', 'Code')]),] 

summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
#pdf("../results/acoustic_GPS/AG_NR_all_overlap_no_dg_NOREPEAT.pdf")
monthdata <- all_10_overlap
month <- "2014-12_random_BPV"
rows <- nrow(monthdata)
no_sharks <- unique(monthdata$Code)
no_sharks <- length(no_sharks)
ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_point(data=monthdata, aes(x= Longitude.x, y= Latitude.x), colour = "red") +
  ggtitle(month) +
  #geom_hex(aes(x=Longitude.x,y=Latitude.x)) +
  #scale_fill_continuous(type = "viridis", limits = c(0, 100), oob = scales::squish) +
  coord_equal() +
  ggsn::scalebar(Chagos_island,transform = T, dist = 50, dist_unit = "km", model = 'WGS84') +
  theme_bw()
toadd <- c(month, rows, no_sharks)
summary_sharks <- rbind(summary_sharks, toadd)
#dev.off()
summary <- c("month", "count", "number_sharks")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]

summary_original <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')
original_2k14 <- summary_original[12,]
counts <- c(11, 36)
months <- c("random_BPV", "original_BPV")
to_plot <- data.frame(months, counts)

pdf("../results/acoustic_GPS/random_walk/2k14_12_random_walk_No-repeat.pdf")
barplot(to_plot$counts, main = "2014-12 acoustic data overlap with random walk compared to original BPV route", ylab = "Counts of overlap", col="#69b3a2", names.arg = c("random_BPV", "original_BPV"))
dev.off()


