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


setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
acoustic <- read.csv("acoustic_no_DG_elas.csv")
BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_no_dg.csv")
#BPV$Longitude <- as.numeric(levels(BPV$Longitude))[BPV$Longitude]
#BPV$Latitude <- as.numeric(levels(BPV$Latitude))[BPV$Latitude]

acoustic$NewDate <- substr(acoustic$Date, 0, 7)


combined <- merge(acoustic, BPV, by.x = "Date", by.y = "Date")

all_combined <- combined %>%
  nest(data= -NewDate) #

############# PLOTTING INITIAL DATA TO HAVE A LOOK AT SPATIAL DISTRIBUTION #####

pdf("../results/acoustic/EVERY_acoustic_BPV_nodg.pdf")
for (i in 1:length(all_combined$NewDate)){
  monthdata <- all_combined$data[[i]]
  month <- all_combined$NewDate[[i]]
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude.y, y= Latitude.y)) + 
    geom_point() +
    geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(data=monthdata, aes(x= Longitude.x, y= Latitude.x, fill = stat(log(count)))) +
    scale_fill_continuous(type = "viridis", limits = c(0, 10), oob = scales::squish) +
    theme_bw() + 
    geom_density2d() + stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon') +
    #scale_fill_gradient(low = "green", high = "red") +
    scale_alpha(range = c(0.00, 0.25), guide = FALSE)
  plot(year_islands_i)
}
dev.off()




########## START LOOKING AT OVERLAP #################
combined <- combined[,-2]
combined <- combined[,-5]
combined <- combined[,-5]


#all_overlap <- merge(new_BPV_bind, new_acoustic, by = "Date", all.x = TRUE, allow.cartesian=TRUE)
cols_10 <- c("Date","Longitude_BPV", "Latitude_BPV", "Code", "Longitude_acoustic", "Latitude_acoustic")
colnames(combined) = cols_10

all_overlap <- combined

#df.new <- as.numeric(levels(df.new$Longitude))[df.new$Longitude]
#list2$latitude.fix <- as.numeric(levels(list2$latitude))[list2$latitude]
######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
all_overlap$distance<-distHaversine(all_overlap[,2:3], all_overlap[,5:6], r=r.km)
all_10_overlap <- all_overlap[all_overlap$distance<sep.km,]

write.csv(all_10_overlap, "../results/acoustic/all_10_overlap_no_DG.csv")
overlap <- read.csv("../results/acoustic/all_10_overlap_no_DG.csv")
# Trying to create a forloop to iterate 
all_10_overlap$NewDate <- substr(all_10_overlap$Date, 0, 7)



new_uniq <- overlap[!duplicated(overlap[c('Date', 'Code')]),] 
View(new_uniq) 



all_nestmonths <- all_10_overlap %>%
  nest(data= -NewDate) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 


summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
pdf("../results/acoustic/all_out_acoustic_no_dg.pdf")
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude_acoustic, y= Latitude_acoustic)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(aes(x=Longitude_acoustic,y=Latitude_acoustic)) +
    scale_fill_continuous(type = "viridis", limits = c(0, 2000), oob = scales::squish) +
    theme_bw()
  toadd <- c(month, rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
  plot(year_islands_i)
}
dev.off()



############## LOGGING BINS
pdf("../results/acoustic/all_out_acoustic_LOG_no_dg.pdf")
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude_acoustic, y= Latitude_acoustic)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(aes(fill = stat(log(count)))) +
    scale_fill_continuous(type = "viridis", limits = c(0, 7.5), oob = scales::squish) +
    theme_bw()
  plot(year_islands_i)
}
dev.off()



##### 
summary <- c("month", "count", "number_sharks")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]
write.csv(summary_sharks,'../results/acoustic/summary_sharks_no_dg.csv')
summary <- read.csv('../results/acoustic/summary_sharks_no_dg.csv')


################################## FINDING THE TAGS AT LIBERTY ####################




acoustic$Date <- substr(acoustic$Date, 0, 10)
#acoustic$Date <- as.POSIXct(acoustic$Date, format="%Y-%m-%d")

code_nest <- acoustic %>%
  nest(data= -Code)



###### For loop to find the tags at liberty for standaridsing 

adding = as.data.frame(matrix(nrow = 1, ncol = 4))
for (i in 1:length(code_nest$Code)){
  one <- code_nest$data[[i]]
  ID <- code_nest$Code[[i]]
  min <- min(one$Date, na.rm=T)
  max <- max(one$Date, na.rm=T)
  diff <- difftime(max, min)
  diff <- as.numeric(diff)
  toadd <- c(ID, min, max, diff)
  adding <- rbind(adding, toadd)
}

###### created dataframe with the minimum number and maximum as well as the number of days at liberty in tags 
adding <- adding[-1,]
cols <- c("Code", "min", "max", "days")
colnames(adding) <- cols  
write.csv(adding, "acoustic/standardising_tags.csv")  

summary_tags <- summary
summary_tags$month <- paste0(summary_tags$month, "-01")


summary_tags$count_tag <- 
  sapply(summary_tags$month, function(x)
    sum(as.Date(adding$min, "%Y-%m-%d") <= as.Date(x, "%Y-%m-%d") &
          as.Date(adding$max, "%Y-%m-%d") >= as.Date(x, "%Y-%m-%d")))



################################# FIRST STANDARDISATION FORM ###########################

setwd("/Users/emmadeeks/Dropbox/Overlap_data")
stations <- read.csv("Station_attributes_full.csv")
#station_plot <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
 # geom_point(data=stations, aes(x= x, y= y), colour = "Blue", shape = 7) +
  #ggtitle("Station") +
  #theme_bw()



setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
#acoustic <- read.csv("acoustic_no_DG.csv")
#BPV <- read.csv("BPV_no_DG.csv")



BPV_final = as.data.frame(matrix(nrow = 1, ncol = 5))
col <- c("X", "Date", "Longitude", "Latitude", "distance")
colnames(BPV_final) <- col

############ Finding stations 

for (i in 1:93){
  temp <- c()
  stations1 <- stations[i,]
  BPV$distance<-distHaversine(BPV[,3:4], stations1[,2:3], r=r.km)
  temp <- BPV[BPV$distance<sep.km,]
  different.names <- (!temp$Date %in% BPV_final$Date)
  not.in.a2 <- temp[different.names,]
  BPV_final <- rbind(BPV_final, not.in.a2)
}


BPV_final <- BPV_final[-1,]
write.csv(BPV_final, "standard/BPV_stations_10km.csv")

BPV_final$monthyear<- substr(BPV_final$Date, 0, 7)
summary_tags$monthyear<- substr(summary_tags$month, 0, 7)


table <- table(BPV_final$monthyear)
table <- as.data.frame(table)
cols <- c("monthyear", "Boat_station_freq")
colnames(table) <- cols

summary_tags <- merge(summary_tags, table, by = "monthyear")

############################## adding stations 

summary_tags$year <- substr(summary_tags$month, 0, 4)
summary_tags$stations <- ifelse(summary_tags$year == 2013, 29,
                                ifelse(summary_tags$year == 2014, 49, 
                                       ifelse(summary_tags$year == 2015, 64, 93)
                                )
)


############## Standardisation 1 ########
summary_tags$month <- substr(summary_tags$month, 6, 7)
summary_tags$standard1 <- (summary_tags$count / (summary_tags$Boat_station_freq * summary_tags$count_tag))
write.csv(summary_tags,'../results/acoustic/summary_tags_no_dg.csv')
summary_tags <- read.csv('../results/acoustic/summary_tags_no_dg.csv')
pdf("../results/acoustic/standardised/standardised_1_OVERLAP_overlaid.pdf")
ggplot(summary_tags, aes(x=month, y=standard1, fill=year, colour = year, group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to the potential (station~BPV)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

pdf("../results/acoustic/standardised/standardised_1_OVERLAP_plots_not_overlaid.pdf")
ggplot(summary_tags, aes(x=monthyear, y=standard1, group = 1)) + 
  geom_line() +
  geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to the potential (station~BPV)") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()


##################### Standardised 2 ###########################



summary_tags$standard2 <- (summary_tags$count / (744 * summary_tags$count_tag))

pdf("../results/acoustic/standardised/standardised_2_OVERLAP_overlaid.pdf")
  ggplot(summary_tags, aes(x=month, y=standard2, fill=year, colour = year, group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

pdf("../results/acoustic/standardised/standardised_2_OVERLAP_plots_not_overlaid.pdf")
  ggplot(summary_tags, aes(x=monthyear, y=standard2, group = 1)) + 
  geom_line() +
  geom_point() + 
  xlab("Month") +# for the x axis label
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  ylab("Proportion of successful overlaps with enforcement vessel and sharks") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

barplot(summary_tags$standard2, main ="Proportion of successful overlaps with enforcement vessel and sharks", ylab="Proportion of sharks overlapped", names.arg= summary_tags$monthyear, las=2) + stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red")

write.csv(summary_tags,'../results/acoustic/summary_tags_no_dg.csv')
######   
summary_tags <- read.csv('../results/acoustic/summary_tags_no_dg.csv')
#lm(standard~X, data = summary_tags)
#summary(lm(summary_tags$standard ~ poly(summary_tags$X, 3, raw = TRUE)))

lm(standard~X, data = summary_tags)
summary(lm(summary_tags$standard2 ~ poly(summary_tags$X, 2, raw = TRUE)))




######################## OVERLAYING BPV MONTHS #################
BPV$NewDate <- substr(BPV$Date, 0, 7)
BPV$year <- substr(BPV$Date, 0, 4)
BPV$month <- substr(BPV$Date, 6, 7)

nest_BPV <- BPV %>%
  nest(data= -month)

pdf("../results/acoustic/stat_each_month_BPV_plotted_compare.pdf")
for (i in 1:length(nest_BPV$month)){
  monthdata <- nest_BPV$data[[i]]
  month <- nest_BPV$month[[i]]
  year_plot_i <- ggplot(monthdata, aes(x=Longitude, y=Latitude, fill=year, colour = year, group=factor(year))) + 
    geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    geom_point(size = 1, shape=21) +
    ggtitle(month) +
    xlab("Month") +# for the x axis label
    ylab("Number of overlaps") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    theme_bw()
  plot(year_plot_i + stat_density_2d(aes(x=Longitude, y=Latitude, fill=year, color = year), geom="polygon", bins = 4, alpha=.1))
}

dev.off()



###################### Making barchart of effort through months #############

summary_tags <- read.csv('../results/acoustic/summary_tags_no_dg.csv')
summary_tags$year <- substr(summary_tags$monthyear, 0, 4)
barplot(summary_tags$Boat_station_freq)

counts <- table(summary_tags$Boat_station_freq, summary_tags$X)

pdf("../results/acoustic/barplot_hours_BPV_station_10km.pdf")
barplot(summary_tags$Boat_station_freq, main ="Number of hours BPV vessel was within 10km of a station within a month", ylim = c(0, 744), ylab="Hours in a month", names.arg= summary_tags$monthyear, las=2) 
dev.off()

summary_tags$proportion_sharks <- summary_tags$number_sharks / summary_tags$count_tag

pdf("../results/acoustic/barplot_proportion_sharks_overlapped_liberty.pdf")
barplot(summary_tags$proportion_sharks, main ="Proportion of sharks overlapped compared to total sharks at liberty per month", ylab="Proportion of sharks overlapped", names.arg= summary_tags$monthyear, las=2) 
dev.off()








