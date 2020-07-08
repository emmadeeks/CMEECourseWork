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

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 

######### Overall plot 
IUU <- read.csv("IUU_Data_catches.csv", header = T)
IUU <- IUU[,1:4]

points(IUU$Longitude, IUU$Latitude)


#papoue_2 <- getNOAA.bathy(lon1 = 70.7, lon2 = 73,
#                          lat1 = -8, lat2 = -4.3, resolution = 3)

 
autoplot(papoue_2, geom=c("r", "c"), colour="white", size=0.1) + scale_fill_etopo() +
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='white', fill = NA, size = 0.1) +
  geom_point(data= IUU, aes(x= Longitude, y= Latitude), shape = 23, fill = "orange", size = 2) + 
  xlim(70.7,73) +
  ylim(-8, -4.3) +
  #geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='white', size = 0.07, fill = NA) +
  #coord_cartesian(expand = 0)+
  ggtitle("A marmap map with ggplot2") 

autoplot.bathy(papoue_2, geom=c("r","c"), colour="white", size=0.1) + scale_fill_etopo() +
  scale_fill_gradientn(colours = c("lightsteelblue1", "lightsteelblue2", "lightsteelblue3", 'lightblue'),
                       values = scales::rescale(c(-4000, -3000, -2000, -1000, 0, 1000))) +
  #scale_fill_gradient2(low="lightsteelblue1", mid="lightsteelblue3", high="darkgreen") +
  geom_point(data= IUU, aes(x= Longitude, y= Latitude), shape = 23, fill = "orange", size = 2) +
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='white', fill = NA, size = 0.1) +
  labs(y = "Latitude", x = "Longitude", fill = "Elevation") +
  xlim(70.7,73) +
  ylim(-8, -4.3) +
  #geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='white', size = 0.07, fill = NA) +
  #coord_cartesian(expand = 0)+
  ggtitle("A marmap map with ggplot2") 



plot(papoue_2, deep = 0, shallow = 0, step = 0,
     lwd = 0.4, add = TRUE) 






######### stations 
library("viridis") 

library(RColorBrewer)
display.brewer.all()
brewer.pal(n, name)
scale_color_grey(start = 0.8, end = 0.2)

setwd("/Users/emmadeeks/Dropbox/Overlap_data")
stations <- read.csv("Station_attributes_full.csv")

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data")

pdf("../results/Thesis_figures/station_plot.pdf")
autoplot(papoue_2, geom=c("r", "c"), colour="white", size=0.1, show.legend = FALSE) + scale_fill_etopo() +
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='lightsteelblue3', fill = NA, size = 0.1) +
  geom_point(data= stations, mapping = aes(x=x, y=y, color=as.factor(stations$Year)), pch = 17, size = 3) + 
  xlim(70.7,73) +
  ylim(-8, -4.3) +
  scale_color_manual(name = "Year", labels = c("2013", "2014", "2015", "2016"), values = c("#33A02C","#E31A1C", "#FF7F00", "#6A3D9A")) +
  #geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='white', size = 0.07, fill = NA) +
  #coord_cartesian(expand = 0)+
  theme_bw() +
  coord_equal() +
  #scalebar(Chagos_island,transform = T, dist = 40, dist_unit = "km", model = 'WGS84', location = "bottomleft", st.size = 2.5) +
  north(Chagos_island, symbol= 3, location = "bottomright") +
  guides(color = guide_legend(override.aes = list(size = 2.1)), guides(shape = guide_legend(override.aes = list(size = 2.1)))) +
  theme(axis.ticks.y=element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.title.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), legend.position = c(0, 1), 
        legend.justification = c(0, 1), axis.title.x=element_blank(), legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7)) 
dev.off()

  ggtitle("A marmap map with ggplot2") 



  scaleBathy(papoue_2, deg = 2, x = "bottomleft", inset = 5)

  setwd("/Users/emmadeeks/Dropbox/Overlap_data")
  stations <- read.csv("Station_attributes_full.csv")

ggplot(stations, mapping = aes(x=x, y=y, color=as.factor(stations$Year))) +
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_point(size = 2, shape = 17) +
  scale_color_manual(name = "Year", labels = c("2013", "2014", "2015", "2016"), values = c('#FDDDA0','#74A089', '#56B4E9', '#972D15')) +
  xlim(70.5,73.5) +
  ylim(-8, -4.5) +
  #ylab("Proportion of BPV enforcement vessel activity") +
  scale_color_manual(values = c('#FDDDA0','#74A089', '#56B4E9', '#972D15'),  guide = FALSE) +
  #scale_color_manual(name = "Year", labels = c("2013", "2014", "2015", "2016"), values = c('#FDDDA0','#74A089', '#56B4E9', '#972D15')) +
  #scale_x_discrete(breaks = 1:10) +
  theme_bw() +
  coord_equal() +
  scalebar(Chagos_island,transform = T, dist = 40, dist_unit = "km", model = 'WGS84', location = "bottomleft", st.size = 2.5) +
  north(Chagos_island, symbol= 3, location = "bottomright") +
  guides(color = guide_legend(override.aes = list(size = 2.1)), guides(shape = guide_legend(override.aes = list(size = 2.1)))) +
  theme(axis.ticks.y=element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.title.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), legend.position = c(0, 1), 
        legend.justification = c(0, 1), axis.title.x=element_blank(), legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7)) 






geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) 



papoue_2 <- getNOAA.bathy(lon1 = 70.7, lon2 = 73,
                          lat1 = -7.7, lat2 = -5, resolution = 1, antimeridian = TRUE)



plot(aleu, image = TRUE, land = TRUE, axes = FALSE, lwd=0.1,
     bpal = list(c(0, max(aleu), grey(.7), grey(.9), grey(.95)),
                 c(min(aleu), 0, "darkblue", "lightblue")))
plot(aleu, n = 1, lwd = 0.5, add = TRUE)
antimeridian.box(aleu)

#geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + 


