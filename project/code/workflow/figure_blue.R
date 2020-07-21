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
library(ggsn)
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
  xlim(70.8,73) +
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
  #coord_equal() +
  #scalebar(Chagos_island,transform = T, dist = 40, dist_unit = "km", model = 'WGS84', location = "left", st.size = 2.5) +
  #north(Chagos_island, symbol= 3, location = "right") +
  guides(color = guide_legend(override.aes = list(size = 2.1)), guides(shape = guide_legend(override.aes = list(size = 2.1)))) +
  theme(axis.ticks.y=element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.title.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), legend.position = c(0, 1), 
        legend.justification = c(0, 1), axis.title.x=element_blank(), legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7)) 
dev.off()

  ggtitle("A marmap map with ggplot2") 



  scaleBathy(papoue_2, deg = 2, x = "bottomleft", inset = 5)

  setwd("/Users/emmadeeks/Dropbox/Overlap_data")
  stations <- read.csv("Station_attributes_full.csv")

  
  ########## Big method figure 

  BPV2 <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_INCLUDE_dg.csv")
  
  BPV2$NewDate <- substr(BPV2$Date, 0 , 7)
  
  nest_BPV_2 <- BPV2 %>%
    nest(data= -NewDate)
  
  monthdata <- nest_BPV_2$data[[29]]
  month <- nest_BPV_2$NewDate[[29]]
  
  
  figure_A_chagos <- getNOAA.bathy(lon1 = 67.8, lon2 = 76,
                            lat1 = -10.9, lat2 = -2, resolution = 1)
  
  a <-  autoplot(figure_A_chagos, geom=c("r", "c"), colour="white", size=0.05, show.legend = FALSE) + scale_fill_etopo() +
    geom_polygon(data=Chagos_try, mapping = aes(x=long, y=lat, group=group), color='black', fill = NA, size = 0.5) +
    geom_point(monthdata, mapping = aes(x=Longitude, y=Latitude), size = 0.5, shape=19, colour = 'black') + 
    stat_density_2d(monthdata, mapping = aes(x=Longitude, y=Latitude), geom="polygon", bins = 5, alpha=.2) +
    #xlim(70.7,73) +
    #ylim(-8, -4.3) +
    #scale_color_manual(name = "Year", labels = c("2013", "2014", "2015", "2016"), values = c("#33A02C","#E31A1C", "#FF7F00", "#6A3D9A")) +
    #geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='white', size = 0.07, fill = NA) +
    #coord_cartesian(expand = 0)+
    theme_bw() +
    north(Chagos_try, symbol= 3, location = 'bottomright', 0.07) + 
    scalebar(Chagos_try,transform = T, dist = 100, dist_unit = "km", model = 'WGS84', location = "bottomleft", st.size = 2.5) +
    #coord_equal() +
    #scalebar(Chagos_island,transform = T, dist = 40, dist_unit = "km", model = 'WGS84', location = "left", st.size = 2.5) +
    #north(Chagos_island, symbol= 3, location = "right") +
    #guides(color = guide_legend(override.aes = list(size = 2.1)), guides(shape = guide_legend(override.aes = list(size = 2.1)))) +
    theme(axis.ticks.y=element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.title.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), legend.position = c(0, 1), 
          legend.justification = c(0, 1), axis.title.x=element_blank(), legend.title = element_text(size = 7), 
          legend.text = element_text(size = 7), aspect.ratio=4/4) 
  
  #a <- a + scalebar(data = Chagos_try, location='topleft', dist=100, transform = T, dist_unit = "km", model='WGS84',
                    st.dist=.02, st.size = 4, height = 0.006)  
  #a <- a + north2(a, x=.3, y=.18, symbol=9, 0.07)
  
  ######### stations 
 n <-  autoplot(papoue_2, geom=c("r", "c"), colour="white", size=0.1, show.legend = FALSE) + scale_fill_etopo() +
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='lightsteelblue3', fill = NA, size = 0.1) +
  geom_point(data= stations, mapping = aes(x=x, y=y, color=as.factor(stations$Year)), pch = 17, size = 3) + 
  xlim(70.7,73) +
  ylim(-8, -4.3) +
  scale_color_manual(name = "Year", labels = c("2013", "2014", "2015", "2016"), values = c("#33A02C","#E31A1C", "#FF7F00", "#6A3D9A")) +
  #geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='white', size = 0.07, fill = NA) +
  #coord_cartesian(expand = 0)+
  theme_bw() +
  north(Chagos_island, symbol= 3, location = 'bottomright', 0.07) + 
   scalebar(Chagos_island,transform = T, dist = 40, dist_unit = "km", model = 'WGS84', location = "bottomleft", st.size = 2.5) +
  #coord_equal() +
  #scalebar(Chagos_island,transform = T, dist = 40, dist_unit = "km", model = 'WGS84', location = "left", st.size = 2.5) +
  #north(Chagos_island, symbol= 3, location = "right") +
  guides(color = guide_legend(override.aes = list(size = 2.1)), guides(shape = guide_legend(override.aes = list(size = 2.1)))) +
  theme(axis.ticks.y=element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.title.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), legend.position = c(0, 1), 
        legend.justification = c(0, 1), axis.title.x=element_blank(), legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7), aspect.ratio=4/4) 

 #n <- n + scalebar(data = Chagos_island, location="bottomleft", dist=40, transform = T, dist_unit = "km", model='WGS84',
                   #st.dist=.02, st.size = 4, height = 0.009) 
# n <- n + north2(n, x=.3, y=.18, symbol=9, 0.07)


 ################### density plot of overlap 
 
 IUU <- read.csv("IUU_Data_catches.csv", header = T)
 IUU <- IUU[,1:4]
 
 overlap <- read.csv("../results/acoustic_GPS/NO_REPEAT_Ac_GPS_all_10_overlap_no_DG.csv")
 
 DG <- IUU%>% filter(between(Latitude, -5.4, -7.4))
 within_dg <- IUU%>% filter(between(Longitude, 71, 72.6))

b <- autoplot(papoue_2, geom=c("r", "c"), colour="white", size=0.1, show.legend = FALSE) + scale_fill_etopo(papoue_2) +
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='lightsteelblue3', fill = NA, size = 0.1) +
  xlim(70.7,73) +
  ylim(-8, -4.3) +
  geom_point(data= within_dg, aes(x= Longitude, y= Latitude), shape = 23, fill = "orange", size = 2) +
  theme_bw() +
  #north(Chagos_island, symbol= 3, location = 'bottomright', 0.07) + 
    scalebar(Chagos_island,transform = T, dist = 40, dist_unit = "km", model = 'WGS84', location = "bottomleft", st.size = 2.5) +
    north(Chagos_island, symbol= 3, location = "bottomright") +
  theme(axis.ticks.y=element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.title.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), legend.position = c(0, 1), 
        legend.justification = c(0, 1), axis.title.x=element_blank(), legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7), aspect.ratio=4/4) 
 

b <- b +
  stat_density_2d(overlap, mapping = aes(x= Longitude_BPV, y= Latitude_BPV, col = stat(log(level))),
                  geom = "polygon",
                  contour = T, 
                  n = 1000 ,
                  bins = 1000, alpha=.3) +
  viridis::scale_color_viridis(name = "Log count", option = 'magma')

  
############### Schematic diagram 

schematic  <- getNOAA.bathy(lon1 = 71.5, lon2 = 72.5,
                          lat1 = -6, lat2 = -5, resolution = 1)


c <- autoplot(schematic, geom=c("r", "c"), colour="white", size=0.1, show.legend = FALSE) + scale_fill_etopo(papoue_2) +
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='lightsteelblue4', fill = NA, size = 0.1) +
  xlim(71.5,72.5) +
  ylim(-6, -5) +
  #geom_point(data= within_dg, aes(x= Longitude, y= Latitude), shape = 23, fill = "orange", size = 2) +
  theme_bw() +
  #north(Chagos_island, symbol= 3, location = 'bottomright', 0.07) + 
  #scalebar(schematic,transform = T, dist = 40, dist_unit = "km", model = 'WGS84', location = "bottomleft", st.size = 2.5) +
  #north(Chagos_island, symbol= 3, location = "bottomright") +
  theme(axis.ticks.y=element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.title.y=element_blank(), panel.grid = element_blank(), 
        panel.spacing = unit(-0.8, "lines"), legend.position = c(0, 1), 
        legend.justification = c(0, 1), axis.title.x=element_blank(), legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7), aspect.ratio=4/4) 





blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")

require(cowplot)
p <- plot_grid(a, n, c, b, labels = "auto", ncol = 2, align="hv")

pdf("../results/Thesis_figures/fig_methods.pdf")
p
dev.off()



 
 
 ########################### notes 
geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) 



papoue_2 <- getNOAA.bathy(lon1 = 70.7, lon2 = 73,
                          lat1 = -7.7, lat2 = -5, resolution = 1, antimeridian = TRUE)



plot(aleu, image = TRUE, land = TRUE, axes = FALSE, lwd=0.1,
     bpal = list(c(0, max(aleu), grey(.7), grey(.9), grey(.95)),
                 c(min(aleu), 0, "darkblue", "lightblue")))
plot(aleu, n = 1, lwd = 0.5, add = TRUE)
antimeridian.box(aleu)

#geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + 


