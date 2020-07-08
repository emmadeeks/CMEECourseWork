rm(list=ls()) #Clear global environment 
setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data/shape_files/")

library(glatos)
library(dplyr)
library(ggplot2)
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
GPS_acoustic <- read.csv("New_data_no_dg_hour/acoustic_GPS_no_elas.csv")
BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_no_dg.csv")

GPS_acoustic$NewDate <- substr(GPS_acoustic$Date, 0 , 7)

overlap <- read.csv("../results/acoustic_GPS/NO_REPEAT_Ac_GPS_all_10_overlap_no_DG.csv")

overlap_time <- overlap$Date
#high_month_BPV <- BPV[BPV$NewDate == '2017-09',]
#high_month_ac <- GPS_acoustic[GPS_acoustic$NewDate == '2017-09',]

#m1 <- merge(GPS_acoustic, BPV, by = "Date")
#m1 <- m1[!duplicated(m1[c('Date', 'Code')]),] 

trying_overlap <- GPS_acoustic[GPS_acoustic$Date %in% overlap_time,]

all_combined <- trying_overlap %>%
  nest(data= -Date) #

######### Priority is going through the data  set and for every hour finding the station with the highest station frequency of 
######## and then inputting them into a dataframe 
priority = as.data.frame(matrix(nrow = 1, ncol = 6))
rows <- colnames(monthdata)
colnames(priority) <- rows
for (i in 1:length(all_combined$Date)){
  monthdata <- all_combined$data[[i]]
  month <- all_combined$Date[[i]]
  variable <- names(sort(table(monthdata$station), decreasing=TRUE)[1])
  high_rows <- monthdata[monthdata$station == variable,]
  high_rows$NewDate <- month
  priority <- rbind(priority, high_rows)
}



priority <- priority[-1,]

priority$NewDate <- substr(priority$NewDate, 0, 7)

all_nestmonths <- priority %>%
  nest(data= -NewDate)






summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  toadd <- c(as.character(month), rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
}


summary <- c("NewDate", "count", "number_sharks")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]


summary_original <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')
#summary_original <- read.csv("../results/acoustic_GPS/new_summary_study_site_sharks.csv")
data_merge <- cbind(as.character(summary_original$monthyear), summary_original$month, summary_original$count_tag, summary_original$Boat_station_freq, summary_original$year, summary_original$stations, summary_original$actual_hours, summary_original$count)

data_merge <- as.data.frame(data_merge)  
names <- c("NewDate", "month", "count_tag", "Boat_station_freq", "year", "stations", "actual_hours", "ORIGINAL_count")
colnames(data_merge) <- names

potential <- merge(summary_sharks, data_merge, by = "NewDate", all = T)
summary_tags <- potential
summary_tags <- summary_tags[-1,]



summary_tags$actual_hours <- as.numeric(as.character(summary_tags$actual_hours))
summary_tags$ORIGINAL_count <- as.numeric(as.character(summary_tags$ORIGINAL_count))
summary_tags$count <- as.numeric(as.character(summary_tags$count))
summary_tags$unstandardised_potential <- (summary_tags$ORIGINAL_count) / (summary_tags$count)
summary_tags$unstandardised_potential <- as.numeric(as.character(summary_tags$unstandardised_potential))
summary_tags$count_tag <- as.numeric(as.character(summary_tags$count_tag))
summary_tags$Boat_station_freq <- as.numeric(as.character(summary_tags$Boat_station_freq))
summary_tags$number_sharks <- as.numeric(as.character(summary_tags$number_sharks))



summary_tags$standard1 <- (summary_tags$unstandardised_potential / (summary_tags$Boat_station_freq * summary_tags$count_tag))
summary_tags$standard3 <- (summary_tags$unstandardised_potential / (summary_tags$Boat_station_freq * summary_tags$number_sharks))
summary_tags$standard2 <- (summary_tags$unstandardised_potential / (summary_tags$actual_hours * summary_tags$count_tag))
summary_tags$standard4 <- (summary_tags$unstandardised_potential / (summary_tags$actual_hours * summary_tags$number_sharks))

summary_tags$std2_potential <- (summary_tags$count / (summary_tags$actual_hours * summary_tags$count_tag))
summary_tags$std2_original <- (summary_tags$ORIGINAL_count / (summary_tags$actual_hours * summary_tags$count_tag))
summary_tags$std2 <- (summary_tags$std2_original / summary_tags$std2_potential)


summary_tags$std5_stat <- (summary_tags$count / (summary_tags$actual_hours * summary_tags$count_tag * summary_tags$stations))




summary_tags <- summary_tags[-64,]
#pdf("../results/acoustic_GPS/AG_NO_REPEAT_standardised_2_OVERLAP_overlaid.pdf")
ggplot(summary_tags, aes(x=month, y=standard4, fill= factor(year), colour = factor(year), group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  scale_colour_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) + 
  scale_fill_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#dev.off()

ggplot(summary_tags, aes(x=NewDate, y=std2, group = 1), colour = '#B40F20') + 
  geom_line(size = 1) +
  #geom_point() + 
  #geom_line(summary_tags, mapping = aes(x = NewDate, y = standard4, group = 1), colour = 'dark green', size = 1) +
  #scale_colour_manual(values = c('#B40F20', 'dark green')) +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))






