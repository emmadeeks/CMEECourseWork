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
library(sp)
library("wesanderson")
#chagos_array <- read_sf(dsn = ".", layer = "Chagos_array")
chagos_v6 <- read_sf(dsn = ".", layer = "Chagos_v6") #read in the shapefiles
#chagosEEZ <- read_sf(dsn = ".", layer = "ChagosEEZ") # read in the shapefiles
#chagos_E <- read_sf(dsn = ".", layer = "ChagosEEZ")
#plot(st_geometry(chagos_v6), asp=1, axes=TRUE)
#plot(st_geometry(chagosEEZ), asp=1, axes=TRUE, main='Chagos outline') #plot the shapefiles

tracts <- readOGR(dsn = ".", layer = "ChagosEEZ") %>%
  spTransform("+proj=longlat +ellps=WGS84")
Chagos_try <- fortify(tracts)


chagos_v6 <- readOGR(dsn = ".", layer = "Chagos_v6") %>%
  spTransform("+proj=longlat +ellps=WGS84")
Chagos_island <- fortify(chagos_v6)




setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_INCLUDE_dg.csv")


#BPV$Longitude <- as.numeric(levels(BPV$Longitude))[BPV$Longitude]
#BPV$Latitude <- as.numeric(levels(BPV$Latitude))[BPV$Latitude]

BPV$Latitude <- as.numeric(as.character(BPV$Latitude))
BPV$Longitude <- as.numeric(as.character(BPV$Longitude))

setwd("/Users/emmadeeks/Dropbox/Overlap_data")
stations <- read.csv("Station_attributes_full.csv")
#station_plot <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
# geom_point(data=stations, aes(x= x, y= y), colour = "Blue", shape = 7) +
#ggtitle("Station") +
#theme_bw()



setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
#acoustic <- read.csv("acoustic_no_DG.csv")
#BPV <- read.csv("BPV_no_DG.csv")

# Cutting out the outside areas of the array 
#BPV <- BPV[which(BPV$Latitude >= -11 & BPV$Latitude <= -2), ]



BPV$NewDate <- substr(BPV$Date, 0, 7)

BPV_loop <- BPV %>%
  nest(data= -NewDate)

################## In Diego Garcia ###########
adding = as.data.frame(matrix(nrow = 1, ncol = 4))
cols <- c("month", "Hrs_in_DG", "Hrs_in_centre_(no_DG)", "hrs_in_outer_MPA")
colnames(adding) <- cols


for ( i in 1:nrow(BPV_loop)){
  data <- BPV_loop$data[[i]]
  month <- BPV_loop$NewDate[[i]]
  border <- data[which(data$Latitude >= -11 & data$Latitude <= -2), ]
  #border_num <- (nrow(data) - nrow(border))
  DG <- data%>% filter(between(Latitude, -7.6, -7.0))
  within_dg <- DG%>% filter(between(Longitude, 72.3, 72.5))
  DG_final <- nrow(within_dg)
  border_nodg <- border[!border$Date %in% within_dg$Date,]
  centre <- border_nodg%>% filter(between(Latitude, -8, -5))
  centre_nodg <- centre%>% filter(between(Longitude, 71, 73))
  centre_final <- nrow(centre_nodg)
  elsewhere <- border_nodg[!border_nodg$Date %in% centre_nodg$Date,]
  elsewhere_num <- nrow(elsewhere)
  toadd <- c(month, DG_final, centre_final, elsewhere_num)
  adding <- rbind(adding, toadd)
}



ggplot(within_dg, aes(x=Longitude, y=Latitude)) + 
  geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_point(size = 1, shape=21) +
  ggtitle(month) +
  xlab("Month") +# for the x axis label
  ylab("Number of overlaps") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw()

adding <- adding[-1,]

stations_BPV <- read.csv("standard/BPV_stations_10km.csv")

stations_BPV$monthyear<- substr(stations_BPV$Date, 0, 7)
#summary_tags$monthyear<- substr(summary_tags$month, 0, 7)


table <- table(stations_BPV$monthyear)
table <- as.data.frame(table)
cols <- c("month", "Boat_station_freq")
colnames(table) <- cols

summary_activity <- merge(adding, table, by = "month")
cols <- c("month", "Hrs_in_DG", "Hrs_in_centre_no_DG", "hrs_in_outer_MPA", "Boat_station_freq")
colnames(summary_activity) <- cols
summary_activity$Hrs_in_centre_no_DG_STATION <- summary_activity[,4] - summary_activity[,6]
summary_activity$Hrs_in_centre_no_DG_STATION <- as.numeric(summary_activity$Hrs_in_centre_no_DG) - as.numeric(summary_activity$Boat_station_freq)


############ stacked bar plot 
plotting <- adding
plotting$year <- substr(plotting$month, 0, 4)

year_plotting <- plotting %>%
  nest(data= -year)

pdf("../results/acoustic/no_repeats/BPV_summary_reduced.pdf", onefile = TRUE)
par(mfrow = c(3, 4))
for (i in 2:nrow(year_plotting)){
  trial <- year_plotting$data[[i]]
  year <- year_plotting$year[[i]]
  long <- trial %>% gather(period, n, -month)
  long$month <- substr(long$month, 6, 7)
  eight <- ggplot(long, aes(x = month, y = n)) + 
    geom_bar(aes(fill = period), stat = "identity", position="fill") +
    ylab("Hours in each vincinity") +
    coord_cartesian(ylim=c(0.5)) +
    #scale_fill_discrete(name = "Area", labels = c("Hrs in centre (noDG)", "Hrs in DG", "Hrs in outer MPA")) +
    scale_fill_manual(name = "Area", labels = c("Hrs in centre (noDG)", "Hrs in DG", "Hrs in outer MPA"), values = c('#d8b365','#999999','#5ab4ac')) +
    ggtitle(year) +
    theme_bw() +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"))
}
dev.off()



  
pdf("../results/acoustic/no_repeats/BPV_summary.pdf", onefile = TRUE)
p 
dev.off()

p <- plot_grid(two, three, four, six, seven, eight, labels = "AUTO", ncol = 2)




############# second plot ##########
BPV$NewDate <- substr(BPV$Date, 0, 7)

BPV_loop <- BPV %>%
  nest(data= -NewDate)


adding = as.data.frame(matrix(nrow = 1, ncol = 4))
cols <- c("month", "Hrs_in_DG", "Hrs_patrol", "elsewhere")
colnames(adding) <- cols


for ( i in 1:nrow(BPV_loop)){
  data <- BPV_loop$data[[i]]
  month <- BPV_loop$NewDate[[i]]
  border <- data[which(data$Latitude >= -11 & data$Latitude <= -2), ]
  border_num <- (nrow(data) - nrow(border))
  DG <- data%>% filter(between(Latitude, -7.6, -7.0))
  within_dg <- DG%>% filter(between(Longitude, 72.3, 72.5))
  DG_final <- nrow(within_dg)
  #border_nodg <- border[!border$Date %in% within_dg$Date,]
  patrol_nodg <- nrow(border) - DG_final
  MPA_num <- nrow(border_nodg)
  #centre <- border_nodg%>% filter(between(Latitude, -8, -5))
  #centre_nodg <- centre%>% filter(between(Longitude, 71, 73))
  #centre_final <- nrow(centre_nodg)
  #elsewhere <- border_nodg[!border_nodg$Date %in% centre_nodg$Date,]
  #elsewhere_num <- nrow(elsewhere)
  #binded_MPA <- sum(centre_final, elsewhere_num)
  toadd <- c(month, DG_final, patrol_nodg, border_num)
  adding <- rbind(adding, toadd)
}

ggplot(data, aes(x=Longitude, y=Latitude)) + 
  geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_point(size = 1, shape=21) +
  ggtitle(month) +
  xlab("Month") +# for the x axis label
  ylab("Number of overlaps") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw()

adding <- adding[-1,]


plotting <- adding
plotting$year <- substr(plotting$month, 0, 4)

year_plotting <- plotting %>%
  nest(data= -year)


scale_fill_gradientn(colours = pal)

pdf("../results/acoustic/no_repeats/BPV_summary_reduced.pdf", onefile = TRUE)
par(mfrow = c(3, 4))
pal <- RColorBrewer::brewer.pal(4, "wes_palette")[3:5]
RColorBrewer::brewer.pal(5, wes_palette("Royal2"))

for (i in 2:nrow(year_plotting)){
  trial <- year_plotting$data[[i]]
  year <- year_plotting$year[[i]]
  long <- trial %>% gather(period, n, -month)
  long$month <- substr(long$month, 6, 7)
  eight <- ggplot(long, aes(x = month, y = n)) + 
    geom_bar(aes(fill = period), stat = "identity", position="fill") +
    ylab("Hours in each vincinity") +
    coord_cartesian(ylim=c(0.5)) +
    #scale_fill_discrete(name = "Area", labels = c("Hrs in centre (noDG)", "Hrs in DG", "Hrs in outer MPA")) +
    scale_fill_manual(name = "Area", labels = c("Other", "Hrs in DG", "Hrs patrolling"), values = c('#FDDDA0','#74A089','#972D15')) +
    #scale_fill_manual(name = "Area", labels = c("Hrs in centre (noDG)", "Hrs in DG", "Hrs in outer MPA"), values = wes_palette("BottleRocket1")) +
    ggtitle(year) +
    theme_bw() +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"))
}
dev.off()




pdf("../results/acoustic/no_repeats/BPV_summary_3.pdf", onefile = TRUE)
p 
dev.off()

p <- plot_grid(two, three, four, six, seven, eight, labels = "AUTO", ncol = 2)

  