rm(list=ls()) #Clear global environment 

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



setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
GPS_acoustic <- read.csv("New_data_no_dg_hour/acoustic_GPS_no_elas.csv")
BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_no_dg.csv")

GPS_acoustic$NewDate <- substr(GPS_acoustic$Date, 0 , 7)

#high_month_BPV <- BPV[BPV$NewDate == '2017-09',]
#high_month_ac <- GPS_acoustic[GPS_acoustic$NewDate == '2017-09',]

m1 <- merge(GPS_acoustic, BPV, by = "Date")
m1 <- m1[!duplicated(m1[c('Date', 'Code')]),] 

all_combined <- m1 %>%
  nest(data= -Date) #


priority = as.data.frame(matrix(nrow = 1, ncol = 10))
rows <- colnames(monthdata)
colnames(priority) <- rows
for (i in 1:length(all_combined$Date)){
  monthdata <- all_combined$data[[i]]
  month <- all_combined$Date[[i]]
  variable <- names(sort(table(monthdata$station), decreasing=TRUE)[1])
  high_rows <- monthdata[monthdata$station == variable,]
  high_rows$NewDate.y <- month
  priority <- rbind(priority, high_rows)
}

priority <- priority[-1,]
all_nestmonths <- priority %>%
  nest(data= -NewDate.x)


summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
for (i in 1:length(all_nestmonths$NewDate.x)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate.x[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  toadd <- c(as.character(month), rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
}


summary <- c("NewDate", "count", "number_sharks")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]
summary_sharks <- summary_sharks[,-3]

summary <- c("monthyear", "POTENTIAL_count")
colnames(summary_sharks) <- summary

summary_original <- read.csv("../results/acoustic_GPS/new_summary_study_site_sharks.csv")

potential <- merge(summary_original, summary_sharks, by = "monthyear", all = T)



potential$Potential_STAND <- (as.numeric(potential$count) / (as.numeric(potential$POTENTIAL_count) * as.numeric(potential$sharks_study) * as.numeric(potential$actual_hours)))

ggplot(potential, aes(x=monthyear, y=Potential_STAND, group = 1)) + 
  geom_line(potential, mapping = aes(x=monthyear, y=Potential_STAND, group = 1), colour="black", size = 0.8) +
  #geom_line(original_summary, mapping =aes(x=monthyear, y=standard2, group = 1), colour = "black", size = 1.2) +
  #geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps") +
  #geom_line(summary,mapping =  aes(x=acoustic_date, y=standard2, group = 1), colour="#000099") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


summary(lm(potential$Potential_STAND ~ poly(potential$X, 3, raw = TRUE)))



############################## draft 

data_merge <- cbind(as.character(summary_original$monthyear), summary_original$month, summary_original$sharks_study, summary_original$Boat_station_freq, summary_original$year, summary_original$stations, summary_original$actual_hours)




data_merge <- as.data.frame(data_merge)  
names <- c("NewDate", "month", "shark_study", "Boat_station_freq", "year", "stations", "actual_hours")
colnames(data_merge) <- names


summary_tags <- potential

summary_tags$standard1 <- (as.numeric(summary_tags$count) / (as.numeric(summary_tags$Boat_station_freq) * as.numeric(summary_tags$shark_study)))
summary_tags$count <- as.numeric(as.character(summary_tags$count))
summary_tags$shark_study <- as.numeric(as.character(summary_tags$shark_study))
summary_tags$actual_hours <- as.numeric(as.character(summary_tags$actual_hours))

summary_tags$standard2 <- summary_tags$count / (summary_tags$actual_hours * summary_tags$shark_study)



new_frame <- cbind(summary_tags$NewDate, summary_tags$standard2)
new_frame_2 <- cbind(as.character(summary_original$monthyear), summary_original$standard5)

names <- c("Month", "Original_overlap")
colnames(new_frame_2) <- names
names <- c("Month", "Potential_overlap")
colnames(new_frame) <- names

new_frame <- as.data.frame(new_frame)
new_frame_2 <- as.data.frame(new_frame_2)

final <- merge(new_frame_2, new_frame, by = "Month")







#final <- as.data.frame(final)
final$Potential_overlap <- as.numeric(as.character(final$Potential_overlap))
final$Original_overlap <- as.numeric(as.character(final$Original_overlap))
final$Month <- as.character(final$Month)
final$st7 <- (final$Original_overlap / final$Potential_overlap)
#final$actual_plot <- (final$Original_overlap / final$Potential_overlap)




