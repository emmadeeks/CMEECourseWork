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
library(tidyverse)
library("wesanderson")
library(marmap)
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



island <- Chagos_island[,1:2]
papoue_2 <- getNOAA.bathy(lon1 = 70.7, lon2 = 73,
                        lat1 = -8, lat2 = -4.3, resolution = 1)


IUU <- read.csv("IUU_Data_catches.csv", header = T)
IUU <- IUU[,1:4]
ggplot() +
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  #scale_fill_gradientn(colours = pal) +
  coord_equal() +
  ggsn::scalebar(Chagos_island,transform = T, dist = 100, dist_unit = "km", model = 'WGS84') +
  #geom_point(data= IUU, aes(x= Longitude, y= Latitude), shape = 23, fill = "orange", size = 3) +
  theme_bw() 





blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")



plot(papoue_2, image = TRUE, land = TRUE, lwd = 0.1,
     bpal = list(c(0, max(papoue_2), "grey"),
                 c(min(papoue_2),0,blues)))
scaleBathy(papoue_2, deg = 2, x = "bottomleft", inset = 5)

plot(papoue_2, deep = 0, shallow = 0, step = 0,
     lwd = 0.4, add = TRUE)


points(IUU$Longitude, IUU$Latitude)


papoue_2 <- getNOAA.bathy(lon1 = 70.7, lon2 = 73,
                          lat1 = -8, lat2 = -4.3, resolution = 3)

 
autoplot(papoue_2, geom=c("r", "c"), colour="white", size=0.1) + scale_fill_etopo() +
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='white', fill = NA) 


autoplot.bathy(papoue_2, geom=c("r","c"), colour="white", size=0.1) + scale_fill_etopo() +
  scale_fill_gradientn(colours = c("lightsteelblue1", "black", "red", 'blue', 'pink', 'darkgreen'),
                       values = scales::rescale(c(-4000, -3000, -2000, -1000, 0, 1000))) +
  #scale_fill_gradient2(low="lightsteelblue1", mid="lightsteelblue3", high="darkgreen") +
  geom_point(data= IUU, aes(x= Longitude, y= Latitude), shape = 23, fill = "orange", size = 2) +
  labs(y = "Latitude", x = "Longitude", fill = "Elevation") +
  xlim(70.7,73) +
  ylim(-8, -4.3) +
  #geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='white', size = 0.07, fill = NA) +
  #coord_cartesian(expand = 0)+
  ggtitle("A marmap map with ggplot2") 



plot(papoue_2, deep = 0, shallow = 0, step = 0,
     lwd = 0.4, add = TRUE) 












geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) 



papoue_2 <- getNOAA.bathy(lon1 = 70.7, lon2 = 73,
                          lat1 = -7.7, lat2 = -5, resolution = 1, antimeridian = TRUE)



plot(aleu, image = TRUE, land = TRUE, axes = FALSE, lwd=0.1,
     bpal = list(c(0, max(aleu), grey(.7), grey(.9), grey(.95)),
                 c(min(aleu), 0, "darkblue", "lightblue")))
plot(aleu, n = 1, lwd = 0.5, add = TRUE)
antimeridian.box(aleu)

#geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + 


