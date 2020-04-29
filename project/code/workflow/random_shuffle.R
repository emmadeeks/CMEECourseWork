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


acoustic <- read.csv("New_data_no_dg_hour/GPS_Ac_binded.csv")
BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_no_dg.csv")

acoustic$NewDate <- substr(acoustic$Date, 0, 7)
BPV$NewDate <- substr(BPV$Date, 0, 7)

df2 <- acoustic[sample(nrow(acoustic)),]
df3 <- BPV[sample(nrow(BPV)),]
df3 <- df3[,-1]
df2 <- df2[,-1]

df2 <- transform(df2, id=match(NewDate, unique(NewDate)))
df3 <- transform(df3, id=match(NewDate, unique(NewDate)))
df2$new <- substr(df2$Date, 8, 19)
df2$new <- paste(df2$id,df2$new, sep = "")

df3$new <- substr(df3$Date, 8, 19)
df3$new <- paste(df3$id,df3$new, sep = "")

m1 <- merge(df3, df2, by = "new")


all_overlap <- m1

#df.new <- as.numeric(levels(df.new$Longitude))[df.new$Longitude]
#list2$latitude.fix <- as.numeric(levels(list2$latitude))[list2$latitude]
######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
all_overlap$distance<-distHaversine(all_overlap[,3:4], all_overlap[,8:9], r=r.km)
all_10_overlap <- all_overlap[all_overlap$distance<sep.km,]

write.csv(all_10_overlap, "../results/acoustic_GPS/RANDOM_Shuffle_AG_all_10_overlap_no_DG.csv")
overlap <- read.csv("../results/acoustic_GPS/RANDOM_Shuffle_AG_all_10_overlap_no_DG.csv")
# Trying to create a forloop to iterate 


new_uniq <- overlap[!duplicated(overlap[c('new', 'Code')]),] 

write.csv(new_uniq, "../results/acoustic_GPS/RANDOM_Shuffle_NO_REPEAT_AG_all_10_overlap_no_DG.csv")
overlap <- read.csv("../results/acoustic_GPS/RANDOM_Shuffle_NO_REPEAT_AG_all_10_overlap_no_DG.csv")

all_nestmonths <- overlap %>%
  nest(data= -id.x) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 


summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
pdf("../results/acoustic_GPS/RANDOM_SHUFFLE_AG_NR_all_overlap_no_dg_NOREPEAT.pdf")
for (i in 1:length(all_nestmonths$id.x)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$id.x[[i]]
  #rows <- nrow(monthdata)
  #no_sharks <- unique(monthdata$Code)
  #no_sharks <- length(no_sharks)
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude.x, y= Latitude.x)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(aes(x=Longitude.x,y=Latitude.x)) +
    scale_fill_continuous(type = "viridis", limits = c(0, 100), oob = scales::squish) +
    theme_bw()
  #toadd <- c(month, rows, no_sharks)
  #summary_sharks <- rbind(summary_sharks, toadd)
  plot(year_islands_i)
}
dev.off()

summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 5))
for (i in 1:length(all_nestmonths$id.x)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$id.x[[i]]
  rows <- nrow(monthdata)
  date_ac <- as.character(substr(monthdata$Date.y, 0, 10))
  date_ac <- date_ac[1]
  date_BPV <- as.character(monthdata$NewDate.x[1])
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  toadd <- c(as.character(month), rows, no_sharks, date_ac, date_BPV)
  summary_sharks <- rbind(summary_sharks, toadd)
}

summary <- c("month", "count", "number_sharks", "acoustic_date", "BPV_date")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]

write.csv(summary_sharks,'../results/acoustic_GPS/RANDOM_Shuffle_AG_NR_summary_sharks_no_dg_NOREPEATS.csv')
summary <- read.csv('../results/acoustic_GPS/RANDOM_Shuffle_AG_NR_summary_sharks_no_dg_NOREPEATS.csv')



################################## FINDING THE TAGS AT LIBERTY ####################

tags_at_liberty <- read.csv("../results/acoustic_GPS/AG_standardising_tags.csv")  


summary_sharks$count_tag <- 
  sapply(summary_sharks$acoustic_date, function(x)
    sum(as.Date(tags_at_liberty$min, "%Y-%m-%d") <= as.Date(x, "%Y-%m-%d") &
          as.Date(tags_at_liberty$max, "%Y-%m-%d") >= as.Date(x, "%Y-%m-%d")))



################################# FIRST STANDARDISATION FORM ###########################
##################### Standardised 2 ###########################

summary_sharks$standard2 <- (as.numeric(summary_sharks$count) / (744 * as.numeric(summary_sharks$count_tag)))

pdf("../results/acoustic_GPS/RANDOM_Shuffle_AG_NO_REPEAT_standardised_2_OVERLAP_plots_not_overlaid.pdf")
ggplot(summary_sharks, aes(x=month, y=standard2, group = 1)) + 
  geom_line() +
  geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

######   

summary <- summary_sharks[order(as.Date(summary_sharks$acoustic_date, format="%Y-%m-%d")),]
summary$acoustic_date <- substr(summary$acoustic_date, 0, 7)

ggplot(summary, aes(x=acoustic_date, y=standard2, group = 1)) + 
  geom_line() +
  geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


########## reading in original data to overlay onto plot 

original_summary <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')

ggplot(original_summary, aes(x=monthyear, y=standard2, group = 1)) + 
  geom_line() +
  geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  #geom_line(summary,mapping =  aes(x=acoustic_date, y=standard2, group = 1), colour="#000099") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



############## making second random permute 



df5 <- acoustic[sample(nrow(acoustic)),]
df6 <- BPV[sample(nrow(BPV)),]
df5 <- df5[,-1]
df6 <- df6[,-1]

df5 <- transform(df5, id=match(NewDate, unique(NewDate)))
df6 <- transform(df6, id=match(NewDate, unique(NewDate)))
df6$new <- substr(df6$Date, 8, 19)
df6$new <- paste(df6$id,df6$new, sep = "")

df5$new <- substr(df5$Date, 8, 19)
df5$new <- paste(df5$id,df5$new, sep = "")

m1 <- merge(df5, df6, by = "new")


all_overlap <- m1

#df.new <- as.numeric(levels(df.new$Longitude))[df.new$Longitude]
#list2$latitude.fix <- as.numeric(levels(list2$latitude))[list2$latitude]
######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
all_overlap$distance<-distHaversine(all_overlap[,3:4], all_overlap[,9:10], r=r.km)
all_10_overlap <- all_overlap[all_overlap$distance<sep.km,]

#write.csv(all_10_overlap, "../results/acoustic_GPS/RANDOM_Shuffle_AG_all_10_overlap_no_DG.csv")
#overlap <- read.csv("../results/acoustic_GPS/RANDOM_Shuffle_AG_all_10_overlap_no_DG.csv")
# Trying to create a forloop to iterate 

overlap = all_10_overlap
new_uniq <- overlap[!duplicated(overlap[c('new', 'Code')]),] 

write.csv(new_uniq, "../results/acoustic_GPS/RANDOM_Shuffle_2_NO_REPEAT_AG_all_10_overlap_no_DG.csv")
overlap <- read.csv("../results/acoustic_GPS/RANDOM_Shuffle_2_NO_REPEAT_AG_all_10_overlap_no_DG.csv")

all_nestmonths <- overlap %>%
  nest(data= -id.x) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 



summary_sharks_2 = as.data.frame(matrix(nrow = 1, ncol = 5))
for (i in 1:length(all_nestmonths$id.x)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$id.x[[i]]
  rows <- nrow(monthdata)
  date_ac <- as.character(substr(monthdata$Date.y, 0, 10))
  date_ac <- date_ac[1]
  date_BPV <- as.character(monthdata$NewDate.x[1])
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  toadd <- c(as.character(month), rows, no_sharks, date_ac, date_BPV)
  summary_sharks_2 <- rbind(summary_sharks_2, toadd)
}

summary <- c("month", "count", "number_sharks", "acoustic_date", "BPV_date")
colnames(summary_sharks_2) <- summary
summary_sharks_2 <- summary_sharks_2[-1,]

write.csv(summary_sharks_2,'../results/acoustic_GPS/RANDOM_Shuffle_2_AG_NR_summary_sharks_no_dg_NOREPEATS.csv')
summary <- read.csv('../results/acoustic_GPS/RANDOM_Shuffle_2_AG_NR_summary_sharks_no_dg_NOREPEATS.csv')



################################## FINDING THE TAGS AT LIBERTY ####################

tags_at_liberty <- read.csv("../results/acoustic_GPS/AG_standardising_tags.csv")  


summary_sharks_2$count_tag <- 
  sapply(summary_sharks_2$acoustic_date, function(x)
    sum(as.Date(tags_at_liberty$min, "%Y-%m-%d") <= as.Date(x, "%Y-%m-%d") &
          as.Date(tags_at_liberty$max, "%Y-%m-%d") >= as.Date(x, "%Y-%m-%d")))



################################# FIRST STANDARDISATION FORM ###########################
##################### Standardised 2 ###########################

summary_sharks_2$standard2 <- (as.numeric(summary_sharks_2$count) / (744 * as.numeric(summary_sharks_2$count_tag)))
summary <- summary_sharks_2[order(as.Date(summary_sharks_2$acoustic_date, format="%Y-%m-%d")),]
summary$acoustic_date <- substr(summary$acoustic_date, 0, 7)

original_summary <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')

ggplot(original_summary, aes(x=monthyear, y=standard2, group = 1)) + 
  geom_line() +
  geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  geom_line(summary,mapping =  aes(x=acoustic_date, y=standard2, group = 1), colour="#000099") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))




