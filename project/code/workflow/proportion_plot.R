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
library(cowplot)
library(dplyr)
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

############# second plot ##########
BPV$NewDate <- substr(BPV$Date, 0, 7)
BPV$day <- substr(BPV$Date, 0, 10)


BPV_loop <- BPV %>%
  nest(data= -NewDate)


adding = as.data.frame(matrix(nrow = 1, ncol = 4))
cols <- c("month", "Hrs_in_DG", "Hrs_patrol", "elsewhere")
colnames(adding) <- cols

km_inner = as.data.frame(matrix(nrow = 1, ncol = 5))
km_outer = as.data.frame(matrix(nrow = 1, ncol = 5))
cols <- colnames(outer)
colnames(km_inner) <- cols
colnames(km_outer) <- cols

for ( i in 1:nrow(BPV_loop)){
  data <- BPV_loop$data[[i]]
  month <- BPV_loop$NewDate[[i]]
  border <- data[which(data$Latitude >= -11 & data$Latitude <= -3), ]
  outer <- data[which(data$Latitude >= -3), ]
  border_num <- (nrow(data) - nrow(border))
  DG <- data%>% filter(between(Latitude, -7.6, -7.0))
  within_dg <- DG%>% filter(between(Longitude, 72.3, 72.5))
  DG_final <- nrow(within_dg)
  border_nodg <- border[!border$Date %in% within_dg$Date,]
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
  km_inner <- rbind(km_inner, border)
  km_outer <- rbind(km_outer, outer)
}

write.csv(km_inner, "../data/stats/km_inner.csv")

write.csv(km_outer, "../data/stats/km_outer.csv")


ggplot(outer, aes(x=Longitude, y=Latitude)) + 
  geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_point(size = 1, shape=21) +
  ggtitle(month) +
  xlab("Month") +# for the x axis label
  ylab("Number of overlaps") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw()


adding <- adding[-1,]

missing_dataframe = as.data.frame(matrix(nrow = 18, ncol = 3))
missing <- c("2016-07", "2016-08", "2016-09", "2016-10", "2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04", "2017-05", "2018-01", "2018-07", "2019-08", "2019-09", "2019-10", "2019-11", "2019-12")
missing_dataframe <- cbind(missing, missing_dataframe)
missing_dataframe <- data.table(missing_dataframe)

names <- c("month", "Hrs_in_DG", "Hrs_patrol", "elsewhere")
colnames(missing_dataframe) <- names
new_df <- missing_dataframe



long_adding <- adding %>% gather(period, n, -month)
long_adding$group_fill <- long_adding$period


long_df <- new_df %>% gather(period, n, -month)
long_df$group_fill <- 'no'
long_df$n <- 1
#new_df$no_dat <- 1000
adding_long <- rbind(long_adding, long_df)

#adding[57:69,2:5] <- NA
#new_df$month <- paste0(new_df$month, "-01")

#new_df <- new_df[order(as.Date(new_df$month, format="%Y-%m-%d")),]
#new_df$month<- substr(new_df$month, 0, 7)

plotting <- adding_long
#plotting <- adding
plotting$year <- substr(plotting$month, 0, 4)

year_plotting <- plotting %>%
  nest(data= -year)


#scale_fill_gradientn(colours = pal)


#################   WAIT! FOR THIS YOU MUST INDIVIDUALLY MAKE ALL OF THE PLOTS 
#pdf("../results/acoustic/no_repeats/BPV_summary_reduced.pdf", onefile = TRUE)
#par(mfrow = c(3, 4))
pal <- RColorBrewer::brewer.pal(4, "wes_palette")[3:5]
RColorBrewer::brewer.pal(5, wes_palette("Royal1"))

i = 2
#for (i in 2:nrow(year_plotting)){
  trial <- year_plotting$data[[i]]
  year <- year_plotting$year[[i]]
  #long <- trial %>% gather(period, n, -month)
  trial$month <- substr(trial$month, 6, 7)
  two <- ggplot(trial, aes(x = month, y = n, fill = group_fill), color= 'white') +
    geom_bar(aes(fill = group_fill), stat = "identity", position="fill") +
    coord_cartesian(ylim=c(0.5)) +
    scale_color_manual(values = c('#FDDDA0','#74A089','#972D15', 'white'),  guide = FALSE) +
    scale_fill_manual(name = "Area", labels = c("Other", "Hrs in DG", "Hrs patrolling", "no data"), values = c('#FDDDA0','#74A089','#972D15', 'white')) +
    ggtitle(year) +
    #scale_x_discrete(breaks = 1:10) +
    theme_bw() +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"))
  
  trial <- year_plotting$data[[2]]
  year <- year_plotting$year[[2]]
  #long <- trial %>% gather(period, n, -month)
  trial$month <- substr(trial$month, 6, 7)
  two <- ggplot(trial, aes(x = month, y = n, fill = group_fill), color= 'white') +
    geom_bar(aes(fill = group_fill), stat = "identity", position="fill") +
    coord_cartesian(ylim=c(0.5)) +
    ylab("Proportion of BPV enforcement vessel activity") +
    #xlab("Month") + 
    scale_color_grey() +
    #scale_color_manual(values = c('grey1','#74A089','grey80', 'white'),  guide = FALSE) +
    scale_fill_manual(name = "Area", labels = c("Other", "Hrs in DG", "Hrs patrolling", "no data"), values = c('grey19','grey58','grey80', 'white')) +
    ggtitle(year) +
    #scale_x_discrete(breaks = 1:10) +
    theme_bw() +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), legend.position = "none", axis.title.x=element_blank(),
          axis.text.x=element_blank(), plot.title = element_text(hjust = 0.5))
  
    
  trial <- year_plotting$data[[3]]
  year <- year_plotting$year[[3]]
  #long <- trial %>% gather(period, n, -month)
  trial$month <- substr(trial$month, 6, 7)
    three <- ggplot(trial, aes(x = month, y = n, fill = group_fill), color= 'white') +
      geom_bar(aes(fill = group_fill), stat = "identity", position="fill") +
      coord_cartesian(ylim=c(0.5)) +
      #ylab("Proportion of BPV enforcement vessel activity") +
      #xlab("Month")
     scale_color_manual(values = c('grey1','#74A089','grey80', 'white'),  guide = FALSE) +
      scale_fill_manual(name = "Area", labels = c("Other", "Hrs in DG", "Hrs patrolling", "no data"), values = c('grey1','grey58','grey80', 'white')) +
      ggtitle(year) +
      #scale_x_discrete(breaks = 1:10) +
      theme_bw() +
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),axis.title.y=element_blank(),  panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), legend.position = "none", axis.title.x=element_blank(),
            axis.text.x=element_blank(), plot.title = element_text(hjust = 0.5))
    
    
    trial <- year_plotting$data[[4]]
    year <- year_plotting$year[[4]]
    #long <- trial %>% gather(period, n, -month)
    trial$month <- substr(trial$month, 6, 7)
    four <- ggplot(trial, aes(x = month, y = n, fill = group_fill), color= 'white') +
      geom_bar(aes(fill = group_fill), stat = "identity", position="fill") +
      coord_cartesian(ylim=c(0.5)) +
      ylab("Proportion of BPV activity") +
      #xlab("Month")
      scale_color_manual(values = c('grey1','#74A089','grey80', 'white'),  guide = FALSE) +
      scale_fill_manual(name = "Area", labels = c("Other", "Hrs in DG", "Hrs patrolling", "no data"), values = c('grey1','grey58','grey80', 'white')) +
      ggtitle(year) +
      #scale_x_discrete(breaks = 1:10) +
      theme_bw() +
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),  panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), legend.position = "none", axis.title.x=element_blank(),
            axis.text.x=element_blank(), plot.title = element_text(hjust = 0.5))
    
    
    trial <- year_plotting$data[[6]]
    year <- year_plotting$year[[6]]
    #long <- trial %>% gather(period, n, -month)
    trial$month <- substr(trial$month, 6, 7)
    six <- ggplot(trial, aes(x = month, y = n, fill = group_fill), color= 'white') +
      geom_bar(aes(fill = group_fill), stat = "identity", position="fill") +
      coord_cartesian(ylim=c(0.5)) +
      ylab("Proportion of BPV enforcement vessel activity") +
      #xlab("Month")
      scale_color_manual(values = c('grey1','#74A089','grey80', 'white'),  guide = FALSE) +
      scale_fill_manual(name = "Area", labels = c("Other", "Hrs in DG", "Hrs patrolling", "No data"), values = c('grey1','grey58','grey80', 'white')) +
      ggtitle(year) +
      #scale_x_discrete(breaks = 1:10) +
      theme_bw() +
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), axis.title.x=element_blank(), legend.position = "none", 
            axis.text.x=element_blank(), plot.title = element_text(hjust = 0.5))  
#}
#dev.off()
    
    trial <- year_plotting$data[[7]]
    year <- year_plotting$year[[7]]
    #long <- trial %>% gather(period, n, -month)
    trial$month <- substr(trial$month, 6, 7)
    seven <- ggplot(trial, aes(x = month, y = n, fill = group_fill), color= 'white') +
      geom_bar(aes(fill = group_fill), stat = "identity", position="fill") +
      coord_cartesian(ylim=c(0.5)) +
      ylab("Proportion of BPV enforcement vessel activity") +
      xlab("Month") +
      scale_color_manual(values = c('grey1','#74A089','grey80', 'white'),  guide = FALSE) +
      scale_fill_manual(name = "Area", labels = c("Other", "Hrs in DG", "Hrs patrolling", "No data"), values = c('grey1','grey58','grey80', 'white')) +
      ggtitle(year) +
      #scale_x_discrete(breaks = 1:10) +
      theme_bw() +
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), legend.position = "none", plot.title = element_text(hjust = 0.5))  
    #}

    trial <- year_plotting$data[[8]]
    year <- year_plotting$year[[8]]
    #long <- trial %>% gather(period, n, -month)
    trial$month <- substr(trial$month, 6, 7)
    eight <- ggplot(trial, aes(x = month, y = n, fill = group_fill), color= 'white') +
      geom_bar(aes(fill = group_fill), stat = "identity", position="fill") +
      coord_cartesian(ylim=c(0.5)) +
      #ylab("Proportion of BPV enforcement vessel activity") +
      xlab("Month") +
      scale_color_manual(values = c('grey1','#74A089','grey80', 'white'),  guide = FALSE) +
      scale_fill_manual(name = "Area", labels = c("Other", "Hrs in DG", "Hrs patrolling", "No data"), values = c('grey1','grey58','grey80', 'white')) +
      ggtitle(year) +
      #scale_x_discrete(breaks = 1:10) +
      theme_bw() +
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank(), axis.title.y=element_blank(), panel.grid = element_blank(), panel.spacing = unit(-0.8, "lines"), legend.position = c(1, 1), 
            legend.justification = c(1, 1), plot.title = element_text(hjust = 0.5))  
    
    
    
pdf("../results/Thesis_figures//BPV_summary_2.pdf", onefile = TRUE)
p 
dev.off()

p <- plot_grid(two, three, four, six, seven, eight, labels = "AUTO", ncol = 2)

#percentages 
adding$Hrs_in_DG <- as.numeric(as.character(adding$Hrs_in_DG))
adding$Hrs_patrol <- as.numeric(as.character(adding$Hrs_patrol))
adding$elsewhere <- as.numeric(as.character(adding$elsewhere))
adding$sum <- adding$Hrs_in_DG + adding$Hrs_patrol + adding$elsewhere
adding$patrol_per <- adding$Hrs_patrol / adding$sum


adding$month <- paste0(adding$month, "-01")

adding$month <- as.Date(adding$month, format="%Y-%m-%d")
adding$month <- substr(adding$month, 0, 7)
plot(adding$month, adding$patrol_per)

ggplot(adding, aes(x=month, y=Hrs_patrol, group = 1)) + 
  ylab("Number of hours the vessel spends patrolling in each month") +
  geom_point() +
  geom_line() +
  geom_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 1, raw=TRUE),colour="red") +
  #geom_line(summary_tags, mapping = aes(x=monthyear, y=standard2, group = 1)) +
  #geom_bar(alpha = 0.5) +
  #theme(axis.text.x = element_text(angle = 90)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), axis.text.x = element_text(angle = 90), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

adding$X <- 1:nrow(adding)
lm(standard~X, data = summary_tags)
summary(lm(adding$Hrs_patrol ~ poly(adding$X, 1, raw = TRUE)))



pdf("../results/Hrs_patrolling.pdf")
ggplot(adding, aes(x=month, y=Hrs_patrol, group = 1)) + geom_bar(stat="identity", colour = "black", alpha = 0.3) +
  ylab("Number of hours the vessel spends patrolling in each month") +
  #geom_point() +
  #geom_line() +
  geom_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  #geom_line(summary_tags, mapping = aes(x=monthyear, y=standard2, group = 1)) +
  #geom_bar(alpha = 0.5) +
  #theme(axis.text.x = element_text(angle = 90)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), axis.text.x = element_text(angle = 90), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
dev.off()

ggplot(adding, aes(x=month, y=Hrs_patrol, group = 1)) + 
  geom_line() +
  geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to the potential (station~BPV)") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))





pdf("../results/chagos_island.pdf")
ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  scalebar(Chagos_island,transform = T, dist = 40, dist_unit = "km", model = 'WGS84') +
  north(Chagos_island, symbol= 3, location = "topleft") +
  theme_bw()
dev.off()
  