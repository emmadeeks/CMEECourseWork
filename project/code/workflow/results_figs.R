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
library(cowplot)
library(dplyr)
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

######### Overall plot 
IUU <- read.csv("IUU_Data_catches.csv", header = T)
IUU <- IUU[,1:4]

overlap <- read.csv("../results/acoustic_GPS/NO_REPEAT_Ac_GPS_all_10_overlap_no_DG.csv")
pal <- wes_palette("Zissou1", 100, type = "continuous")

  #a <- ggplot(data=overlap, aes(x= Longitude_GPS, y= Latitude_GPS)) + 
   # geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
    #xlim(70,73) +
    #ylim(-7.5, -4.5) +
    #geom_hex(aes(fill = stat(log(count)), xbins = 60)) +
    #scale_fill_continuous(type = "viridis", limits = c(0, 7), oob = scales::squish) +
    #scale_fill_gradientn(colours = pal) +
  #  coord_equal() +
   # blank() + 
    #scalebar(Chagos_island,transform = T, dist = 40, dist_unit = "km", model = 'WGS84') +
    #north(Chagos_island, symbol= 3, location = "topleft") +
    #geom_point(data= IUU, aes(x= Longitude, y= Latitude), shape = 23, fill = "orange", size = 1) +
    #theme_bw() 
  
   #m <- ggplot(data=overlap, aes(x= Longitude_BPV, y= Latitude_BPV)) + 
  #  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  #  xlim(70.5,73.5) +
  #  ylim(-8, -4.5) +
  #  geom_point(size = 1.5, alpha = 0.05, colour = "red") + 
    #geom_hex(aes(fill = stat(log(count)), xbins = 60)) +
    #scale_fill_continuous(type = "viridis", limits = c(0, 7), oob = scales::squish) +
    #scale_fill_gradientn(colours = pal) +
  #  scale_fill_viridis_c() + 
  #  coord_equal() +
  #  blank() + 
  #  scalebar(Chagos_island,transform = T, dist = 40, dist_unit = "km", model = 'WGS84') +
  #  north(Chagos_island, symbol= 3, location = "topleft") +
  #  geom_point(data= IUU, aes(x= Longitude, y= Latitude), shape = 23, fill = "orange", size = 2) +
  #  theme_bw() 
   
   
      d <- ggplot(data=overlap, aes(x= Longitude_BPV, y= Latitude_BPV)) + 
     #geom_point() +
     stat_density_2d(aes(fill = stat(log(level))),
                         geom = "polygon",
                     contour = T, 
                         n = 1000 ,
                         bins = 1000, alpha=.3) +
        xlim(70.5,73.5) +
          ylim(-8, -4.5) +
       scale_fill_viridis(name = "Log count", option="magma") +
       geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
     coord_equal() +
        theme_bw() +
        guides(color = guide_legend(override.aes = list(size = 0.5)), guides(shape = guide_legend(override.aes = list(size = 2)))) +
     scalebar(Chagos_island,transform = T, dist = 40, dist_unit = "km", model = 'WGS84', location = "bottomleft", st.size = 2.5) +
     north(Chagos_island, symbol= 3, location = "bottomright") +
       theme(axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), legend.position = c(0, 1), 
             legend.justification = c(0, 1), plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.title.x=element_blank(), legend.title = element_text(size = 7), 
             legend.text = element_text(size = 7)) 

  
     DG <- IUU%>% filter(between(Latitude, -5.4, -7.4))
     within_dg <- IUU%>% filter(between(Longitude, 71, 72.6))
     d <- d + geom_point(data= within_dg, aes(x= Longitude, y= Latitude), shape = 23, fill = "orange", size = 2)

#### Acoustic plot 
  
GPS_acoustic <- read.csv("acoustic_no_elas.csv")
GPS_acoustic$NewDate <- substr(GPS_acoustic$Date, 0 , 7)
GPS_acoustic <- GPS_acoustic[!duplicated(GPS_acoustic[c('Date', 'Code')]),] 
  
  
GPS_acoustic$year <- substr(GPS_acoustic$NewDate, 0, 4)
b <- ggplot(data=GPS_acoustic, aes(x= Longitude, y= Latitude)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  #ggtitle("Acoustic detections for 2016") +
  #geom_hex(aes(x=Longitude,y=Latitude)) +
  geom_hex(aes(fill = stat(log(count)), xbins = 60)) +
  scale_fill_continuous(type = "viridis", limits = c(0, 10), oob = scales::squish) +
  xlim(70.5,73.5) +
  ylim(-8, -4.5) +
  #geom_point(data= IUU, aes(x= Longitude, y= Latitude), shape = 23, fill = "lightblue", size = 2) +
  #geom_point(data= IUU_6, aes(x= Longitude, y= Latitude), shape = 23, fill = "orange", size = 2) +
  coord_equal() +
  ggsn::scalebar(Chagos_island,transform = T, dist = 50, dist_unit = "km", model = 'WGS84') +
  theme_bw()

######### Station plot 
setwd("/Users/emmadeeks/Dropbox/Overlap_data")
stations <- read.csv("Station_attributes_full.csv")

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data")
a <- ggplot() +
geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  xlim(71.5,72.5) +
  ylim(-6, -5) +
  coord_equal() +
  scalebar(Chagos_island,transform = T, dist = 5, dist_unit = "km", model = 'WGS84', location = "bottomleft", st.size = 2.5) +
  north(Chagos_island, symbol= 3, location = "bottomright") +
  theme_bw() +
  theme(axis.ticks.y=element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.title.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), 
        axis.title.x=element_blank())

#geom_point(stations, mapping = aes(x=x, y=y, color=as.factor(stations$Year), shape=as.factor(stations$Year)), size = 4) + 
#scale_shape_manual(values=c(15, 16, 17, 18))+ 
#scale_color_manual(values=c('#FDDDA0','#74A089', '#56B4E9', '#972D15'))+
#  theme(axis.text.y=element_blank(),
#        axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), legend.position = c(1, 1), 
#        legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5))  +
#scale_fill_manual(name = "Area", labels = c("2013", "2014", "2015", "2016")) +
#theme_bw()




c <-  ggplot(stations, mapping = aes(x=x, y=y, color=as.factor(stations$Year))) +
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_point(size = 2, shape = 17) +
  xlim(70.5,73.5) +
  ylim(-8, -4.5) +
  #ylab("Proportion of BPV enforcement vessel activity") +
  #scale_color_manual(values = c('#FDDDA0','#74A089', '#56B4E9', '#972D15'),  guide = FALSE) +
  scale_color_manual(name = "Year", labels = c("2013", "2014", "2015", "2016"), values = c('#FDDDA0','#74A089', '#56B4E9', '#972D15')) +
  #scale_x_discrete(breaks = 1:10) +
  theme_bw() +
  coord_equal() +
  scalebar(Chagos_island,transform = T, dist = 40, dist_unit = "km", model = 'WGS84', location = "bottomleft", st.size = 2.5) +
  north(Chagos_island, symbol= 3, location = "bottomright") +
  guides(color = guide_legend(override.aes = list(size = 2.1)), guides(shape = guide_legend(override.aes = list(size = 2.1)))) +
  theme(axis.ticks.y=element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"), axis.title.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), legend.position = c(0, 1), 
legend.justification = c(0, 1), axis.title.x=element_blank(), legend.title = element_text(size = 7), 
legend.text = element_text(size = 7)) 

         
######## BPV routes 
#BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_no_dg.csv")
BPV2 <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_INCLUDE_dg.csv")

BPV2$NewDate <- substr(BPV2$Date, 0 , 7)
#BPV$year <- substr(BPV$Date, 0, 4)
#BPV$month <- substr(BPV$Date, 6, 7)

nest_BPV_2 <- BPV2 %>%
  nest(data= -NewDate)

#nest_BPV <- BPV %>%
#  nest(data= -month)

#pdf("../results/acoustic/stat_each_month_BPV_plotted_compare.pdf")
#for (i in 1:length(nest_BPV$month)){
monthdata <- nest_BPV_2$data[[29]]
month <- nest_BPV_2$NewDate[[29]]
 # monthdata <- nest_BPV$data[[i]]
#  month <- nest_BPV$month[[i]]
 # d <- ggplot(monthdata, aes(x=Longitude, y=Latitude, fill=year, colour = year, group=factor(year))) + 
  #  geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) +
   # geom_point(size = 0.5, shape=21, colour = 'black') +
    #xlim(7,73.5) +
  #  ylim(-11, -2) +
  #  ggtitle(month) +
  #  xlab("Month") +# for the x axis label
  #  ylab("Number of overlaps") +
  #  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  #  theme_bw()
  #d <- plot(d + stat_density_2d(aes(x=Longitude, y=Latitude), geom="polygon", bins = 5, alpha=.1))
#}

   b <- ggplot(monthdata, aes(x=Longitude, y=Latitude)) + 
    geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    geom_point(size = 0.5, shape=19, colour = 'black') +
    #xlim(7,73.5) +
    ylim(-11, -2) +
    theme_bw() +
    coord_equal() +
    scalebar(Chagos_try,transform = T, dist = 90, dist_unit = "km", model = 'WGS84', location = "bottomleft", st.size = 2.5) +
    north(Chagos_try, symbol= 3, location = "bottomright") + 
    stat_density_2d(aes(x=Longitude, y=Latitude), geom="polygon", bins = 5, alpha=.2) +
    theme(axis.ticks.y=element_blank(), axis.title.y=element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), axis.title.x=element_blank()) 
#dev.off()

########## plotting figure 1 
  p <- plot_grid(b, c, a, d, labels = "AUTO", ncol = 2, align="v")
pdf("../results/fig_1.3_results.pdf")
p
dev.off()


###################### FIGURE 2 

#summary_tags <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')
final <- read.csv('../results/acoustic_GPS/IUU_POTENIAL_ORIGINAL_summary_sharks_no_dg_NOREPEATS.csv')
#summary_tags$standard3 <- (summary_tags$count / (744 * summary_tags$count_tag * summary_tags$stations))

summary_tags <- read.csv("../results/acoustic_GPS/updated_AG_NR_summary_sharks_no_dg_NOREPEATS.csv")  

########## overlaid plot

e <- ggplot(summary_tags, aes(x=month, y=standard1, fill= factor(year), colour = factor(year), group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  ggtitle("Standardisation 1: Hours in station vicinity and tags at liberty") +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  scale_colour_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) + 
  scale_fill_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


f <- ggplot(summary_tags, aes(x= as.factor(month), y=standard2, fill=factor(year), colour = factor(year), group=factor(year))) + geom_line(size=1.1) + 
  geom_point(size = 3) +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to the potential (station~BPV)") +
  ggtitle("Standardisation 2: Hours recorded and tags at liberty") +
  #scale_shape_manual(values = c(0,1,3,0,1,3)) +
  scale_colour_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) + 
  scale_fill_manual(name = "Year", values = c('#B40F20', '#046C9A', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

h <- ggplot(final, aes(x=month_plot, y=actual_plot, fill= factor(year), colour = factor(year), group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  ggtitle("Standardisation 3: Potential overlap is station with highest sharks, tags at liberty and hours in station") +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  scale_colour_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) + 
  scale_fill_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


g <- ggplot(summary_tags, aes(x=month, y=potential, fill= factor(year), colour = factor(year), group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  ggtitle("Standardisation 4: Potential overlap is total sharks recorded in each time period, tags at liberty and hours in station") +
  scale_colour_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) + 
  scale_fill_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



i <- plot_grid(e, f, h,g,  labels = "AUTO", ncol = 2)
pdf("../results/fig_2_results.pdf")
i
dev.off()

####### year plot 

j <- ggplot(summary_tags, aes(x=monthyear, y=standard1, group = 1)) + 
  geom_line() +
  geom_point() + 
  ggtitle("Standardisation 1: Hours in station vicinity and tags at liberty") +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


lm(standard~X, data = summary_tags)
summary(lm(summary_tags$standard1 ~ poly(summary_tags$X, 2, raw = TRUE)))


k <- ggplot(summary_tags, aes(x=monthyear, y=standard2, group = 1)) + 
  geom_line() +
  geom_point() + 
  ggtitle("Standardisation 2: Hours recorded and tags at liberty") +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to the potential (station~BPV)") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

lm(standard~X, data = summary_tags)
summary(lm(summary_tags$standard2 ~ poly(summary_tags$X, 2, raw = TRUE)))

m <- ggplot(final, mapping =  aes(x=Month, y=to_plot, group = 1)) + 
  geom_line(size = 1) +
  ggtitle("Standardisation 3: Potential overlap is station with highest sharks, tags at liberty and hours in station") +
  scale_y_continuous(sec.axis = sec_axis(~. / 9, "Overlap score (between 0 and 1), Potential being location with most sharks")) +
  geom_bar(final, mapping = aes(x=Month, y=IUU_events), stat="identity", colour = "black", alpha = 0.3) +
  scale_x_discrete() +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Date") +# for the x axis label
  ylab("Number of IUU interceptions") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

summary(lm(final$to_plot ~ poly(final$X, 2, raw = TRUE)))

l <- ggplot(summary_tags, aes(x=monthyear, y=potential, group = 1)) + 
  geom_line() +
  geom_point() + 
  ggtitle("Standardisation 4: Potential overlap is total sharks recorded in each time period, tags at liberty and hours in station") +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

lm(standard~X, data = summary_tags)
summary(lm(summary_tags$potential ~ poly(summary_tags$X, 2, raw = TRUE)))


############### plotting the other four plots 
summary_tags$std5_s1_ss <- summary_tags$count / summary_tags$Boat_station_freq * summary_tags$number_sharks
summary(lm(summary_tags$std5_s1_ss ~ poly(summary_tags$X, 2, raw = TRUE)))

summary_tags$std6_s2_ss <- summary_tags$count / summary_tags$actual_hours * summary_tags$number_sharks
summary(lm(summary_tags$std6_s2_ss ~ poly(summary_tags$X, 2, raw = TRUE)))




n <- plot_grid(j, k, m, l,labels = "AUTO", ncol = 2)
pdf("../results/fig_3_results.pdf")
n
dev.off()
