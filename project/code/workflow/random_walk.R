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
library(cowplot)
library(dplyr)



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


my_polygon <- crw_in_polygon(tracts, initPos=c(72.5,-7.5), theta = c(0,40), stepLen = 20000, initHeading = 140, nsteps = 303, sp_out = F)
#my_polygon <- crw_in_polygon(tracts, initPos=c(72.5,-7.5), stepLen = 20000, initHeading = 180, nsteps = 303, sp_out = F)
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
counts <- c(4, 36)
months <- c("random_BPV", "original_BPV")
to_plot <- data.frame(months, counts)

pdf("../results/acoustic_GPS/random_walk/2k14_03_random_walk_No-repeat.pdf")
barplot(to_plot$counts, ylim=c(0,143), main = "2014-03 acoustic data overlap with random walk compared to original BPV route", ylab = "Counts of overlap", col="#69b3a2", names.arg = c("random_BPV", "original_BPV"))
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


high_month_BPV <- centre_nodg[centre_nodg$NewDate == '2017-12',]
high_month_ac <- GPS_acoustic[GPS_acoustic$NewDate == '2017-12',]

nrow(high_month_BPV)

my_polygon <- crw_in_polygon(spdf, initPos=c(72.5,-7.4), theta = c(0,40), stepLen = 20000, initHeading = 337, nsteps = 465, sp_out = F)
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
month <- "2014-03_random_BPV"
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
counts_2014_03 <- c(10, 143)/15
counts_2014_12 <- c(4, 36)/16
counts_2015_02 <- c(10,68)/17
counts_2018_10 <- c(58, 120)/120
counts_2017_12 <- c(20, 62)/90

counts_2014_03 <- c(10, 143)/15
counts_2014_12 <- c(4, 36)/16
counts_2015_02 <- c(10,68)/17
counts_2018_10 <- c(58, 120)/120
counts_2017_12 <- c(20, 62)/90


box <- rbind(counts_2014_03, counts_2014_12, counts_2015_02, counts_2017_12, counts_2018_10)
box <- as.data.frame(box)


months <- c("B1", "B2")
to_plot_12 <- data.frame(months, counts_12)
to_plot_03 <- data.frame(months, counts_03)

colnames(box) <- months
box <- setDT(box, keep.rownames = TRUE)[]

add1 <- cbind(box$rn, box$B1)
add2 <- cbind(box$rn, box$B2)
add1 <- as.data.frame(add1)
add2 <- as.data.frame(add2)
add2$ID <- 'Actual'
add1$ID <- 'Simulation'
plotting <- rbind(add1, add2)
names <- c('Date', 'Count', 'ID')
colnames(plotting) <- names 

plotting$Count <- as.numeric(as.character(plotting$Count))

plotting$Count <- round(plotting$Count, digits = 2)


ggplot(plotting, aes(factor(Date), Count, fill = ID)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set2") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size=14, color = "black"), axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), legend.position= c(0.8,0.9),
        legend.text = element_text(size = 14),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



Simulation  <- c(0.6087582,  0.4424439)
Actual <- c(3.494444, 3.618211)

plot <- rbind(Simulation, Actual)
plot <- as.data.frame(plot)
names <- c("mean", "sd")
colnames(plot) <- names
plot <- setDT(plot, keep.rownames = TRUE)[]

ggplot(plot) +
  geom_bar( aes(x=rn, y=mean*10), stat="identity", fill="skyblue", alpha=0.7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size=14, color = "black"), axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), legend.position= c(0.8,0.9),
        legend.text = element_text(size = 14),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  #geom_errorbar( aes(x=rn, ymin=mean-sd*10, ymax=mean +sd*10), width=0.4, colour="orange", alpha=0.9, size=1.3)




pdf("../results/acoustic_GPS/random_walk/high_low_barplots.pdf")


par(mfrow=c(2,1))
barplot(to_plot_12$counts_12, ylim=c(0,143), main = "2014-12 acoustic data overlap with random walk compared to original BPV route", ylab = "Counts of overlap", col="#69b3a2", names.arg = c("random_BPV", "original_BPV"))

barplot(to_plot_03$counts_03, ylim=c(0,143), main = "2014-03 acoustic data overlap with random walk compared to original BPV route", ylab = "Counts of overlap", col="#69b3a2", names.arg = c("random_BPV", "original_BPV"))

dev.off()
#c <- barplot(to_plot$counts, ylim=c(0,143), main = "2017-09 acoustic data overlap with random walk compared to original BPV route", ylab = "Counts of overlap", col="#69b3a2", names.arg = c("random_BPV", "original_BPV"))
#dev.off()


##################################### Making a whole year plot 

GPS_acoustic$year <- substr(GPS_acoustic$Date, 0 , 4)
BPV$year <- substr(BPV$NewDate, 0, 4)

high_month_BPV <- BPV[BPV$year == '2014',]
high_month_ac <- GPS_acoustic[GPS_acoustic$year == '2014',]

nrow(high_month_BPV)



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


high_month_BPV <- centre_nodg[centre_nodg$year == '2014',]
high_month_ac <- GPS_acoustic[GPS_acoustic$year == '2014',]

nrow(high_month_BPV)

my_polygon <- crw_in_polygon(spdf, initPos=c(72.5,-7.4), theta = c(0,40), stepLen = 20000, initHeading = 337, nsteps = 3692, sp_out = F)
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
month <- "2014-03_random_BPV"
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

overlap <- read.csv("../results/acoustic_GPS/NO_REPEAT_Ac_GPS_all_10_overlap_no_DG.csv")

overlap_GPS <- overlap[overlap$station == 'GPS',]


overlap$year <- substr(overlap$Date, 0, 4)
overlap_2014 <- overlap[overlap$year == '2014',]


library(data.table)
Simulation  <- c(206)
Actual <- c(1221)

plot <- rbind(Simulation, Actual)
names <- "count"
colnames(plot) <- names
plot <- as.data.frame(plot)
plot <- setDT(plot, keep.rownames = TRUE)[]


ggplot(plot) +
  geom_bar(aes(x = rn, y = count), stat="identity", fill="skyblue", alpha=0.7) +
  theme_bw() +
  theme(
        text = element_text(size=14, color = "black"), axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), legend.position= c(0.8,0.9),
        legend.text = element_text(size = 14), axis.line = element_line(colour = "black"))


pdf("../results/Thesis_figures/random_walk.pdf")
ggplot(plot, mapping = aes(x = rn, y = count)) + geom_bar(stat="identity", colour = 'gray50', fill = c('red', '#208EA3'), alpha = 0.7) +
  ylab("Counts of overlap") +
  xlab("") +
  theme_bw() +
  #scale_fill_manual(values = c('red', '#208EA3')) +
  #geom_errorbar(aes(ymin=x.x-x.y, ymax=x.x+x.y), width=.2) +
  #geom_line(summary_tags, mapping = aes(x=monthyear, y=standard2, group = 1)) +
  #geom_bar(alpha = 0.5) +
  #theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text.y   = element_text(size=18),
        axis.text.x   = element_text(size=18),
        axis.title.y  = element_text(size=19),
        axis.title.x  = element_text(size=20),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=2))

dev.off()








BPV_ex <- BPV[BPV$NewDate == '2015-02',]
ggplot() + geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_point(data=BPV_ex, aes(x= Longitude, y= Latitude), colour = "red") +
  ggtitle(month) +
  #geom_hex(aes(x=Longitude.x,y=Latitude.x)) +
  #scale_fill_continuous(type = "viridis", limits = c(0, 100), oob = scales::squish) +
  coord_equal() +
  ggsn::scalebar(Chagos_island,transform = T, dist = 50, dist_unit = "km", model = 'WGS84') +
  theme_bw()



################
ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_point(data=monthdata, aes(x= Longitude.x, y= Latitude.x), colour = "red") +
  geom_point(data=overlap_2014, aes(x= Longitude_BPV, y= Latitude_BPV), colour = "blue") +
  ggtitle(month) +
  #geom_hex(aes(x=Longitude.x,y=Latitude.x)) +
  #scale_fill_continuous(type = "viridis", limits = c(0, 100), oob = scales::squish) +
  coord_equal() +
  ggsn::scalebar(Chagos_island,transform = T, dist = 50, dist_unit = "km", model = 'WGS84') +
  theme_bw()



a <- a +
  stat_density_2d(overlap_2014, mapping = aes(x= Longitude_BPV, y= Latitude_BPV, col = stat(log(level))), alpha = .1, 
                  geom = "polygon",
                  bins = 10) 



  viridis::scale_color_viridis(name = "Log count", option = 'magma', alpha = 0.5)



a <- a +
  stat_density_2d(data=monthdata, mapping = aes(x= Longitude.x, y= Latitude.x, col = stat(log(level))), alpha = 0.1,
                  geom = "polygon",
                  contour = T, 
                  n = 1000 ,
                  bins = 1000) 
  viridis::scale_color_viridis(name = "Log count", option = 'viridis', alpha = 0.1)



