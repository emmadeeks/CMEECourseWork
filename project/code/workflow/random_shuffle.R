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


acoustic <- read.csv("New_data_no_dg_hour/acoustic_GPS_no_elas.csv")
BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_no_dg.csv")

acoustic$NewDate <- substr(acoustic$Date, 0, 7)
#BPV$NewDate <- substr(BPV$Date, 0, 7)

shuffle_acoustic <- acoustic[sample(nrow(acoustic)),] #shuffle rows 
shuffle_BPV <- BPV[sample(nrow(BPV)),] #shuffle rows 
shuffle_acoustic <- shuffle_acoustic[,-1]
shuffle_BPV <- shuffle_BPV[,-1]

shuffle_acoustic <- transform(shuffle_acoustic, id=match(NewDate, unique(NewDate))) ###### after shuffling rows you assign a unique id for each NewDate
shuffle_BPV <- transform(shuffle_BPV, id=match(NewDate, unique(NewDate)))

######## create a pretend date with the time, day and the ID as the month and then merge like you do with the overlap analysis 
shuffle_acoustic$new <- substr(shuffle_acoustic$Date, 8, 19)
shuffle_acoustic$new <- paste(shuffle_acoustic$id,shuffle_acoustic$new, sep = "")

shuffle_BPV$new <- substr(shuffle_BPV$Date, 8, 19)
shuffle_BPV$new <- paste(shuffle_BPV$id,shuffle_BPV$new, sep = "")

####### merge and match the made up dates 
m1 <- merge(shuffle_acoustic, shuffle_BPV, by = "new")


all_overlap <- m1

#df.new <- as.numeric(levels(df.new$Longitude))[df.new$Longitude]
#list2$latitude.fix <- as.numeric(levels(list2$latitude))[list2$latitude]
######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 20
all_overlap$distance<-distHaversine(all_overlap[,3:4], all_overlap[,10:11], r=r.km)
all_10_overlap <- all_overlap[all_overlap$distance<sep.km,]

#write.csv(all_10_overlap, "../results/acoustic_GPS/permute_test/permute_4_20.csv")
#overlap <- read.csv("../results/acoustic_GPS/permute_test/permute_4_20.csv")
# Trying to create a forloop to iterate 

overlap <- all_10_overlap
new_uniq <- overlap[!duplicated(overlap[c('new', 'Code')]),] 

write.csv(new_uniq, "../results/acoustic_GPS/permute_test/permute_5_20.csv")
overlap <- read.csv("../results/acoustic_GPS/permute_test/permute_5_20.csv")

all_nestmonths <- overlap %>%
  nest(data= -id.x) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 


#summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
#pdf("../results/acoustic_GPS/RANDOM_SHUFFLE_AG_NR_all_overlap_no_dg_NOREPEAT.pdf")
#for (i in 1:length(all_nestmonths$id.x)){
#  monthdata <- all_nestmonths$data[[i]]
#  month <- all_nestmonths$id.x[[i]]
  #rows <- nrow(monthdata)
  #no_sharks <- unique(monthdata$Code)
  #no_sharks <- length(no_sharks)
#  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude.x, y= Latitude.x)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
#    ggtitle(month) +
#    geom_hex(aes(x=Longitude.x,y=Latitude.x)) +
#    scale_fill_continuous(type = "viridis", limits = c(0, 100), oob = scales::squish) +
#    theme_bw()
  #toadd <- c(month, rows, no_sharks)
  #summary_sharks <- rbind(summary_sharks, toadd)
#  plot(year_islands_i)
#}
#dev.off()

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
summary_sharks <- summary_sharks[-2,]

write.csv(summary_sharks,'../results/acoustic_GPS/permute_test/summary_permute_5_20.csv')
summary <- read.csv('../results/acoustic_GPS/permute_test/summary_permute_5_20.csv')



################################## FINDING THE TAGS AT LIBERTY ####################

tags_at_liberty <- read.csv("../results/acoustic_GPS/AG_standardising_tags.csv")  


summary_sharks$count_tag <- 
  sapply(summary_sharks$acoustic_date, function(x)
    sum(as.Date(tags_at_liberty$min, "%Y-%m-%d") <= as.Date(x, "%Y-%m-%d") &
          as.Date(tags_at_liberty$max, "%Y-%m-%d") >= as.Date(x, "%Y-%m-%d")))



################################# FIRST STANDARDISATION FORM ###########################

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



BPV_stations <- read.csv("New_data_no_dg_hour/BPV_stations_20km.csv")

summary_tags <- summary_sharks
BPV_stations$monthyear<- substr(BPV_stations$Date, 0, 7)
summary_tags$monthyear<- substr(summary_tags$acoustic_date, 0, 7)


table <- table(BPV_stations$monthyear)
table <- as.data.frame(table)
cols <- c("monthyear", "Boat_station_freq")
colnames(table) <- cols

summary_tags <- merge(summary_tags, table, by = "monthyear")

############################## adding stations 

#summary_tags$year <- substr(summary_tags$month, 0, 4)
#summary_tags$stations <- ifelse(summary_tags$year == 2013, 29,
#                                ifelse(summary_tags$year == 2014, 49, 
#                                       ifelse(summary_tags$year == 2015, 64, 93)
#                                )
#)

summary_tags$year <- substr(summary_tags$monthyear, 0, 4)
summary_tags$stations <- ifelse(summary_tags$year == 2013, 29,
                                ifelse(summary_tags$year == 2014, 49, 
                                       ifelse(summary_tags$year == 2015, 64, 
                                              ifelse(summary_tags$year == 2016, 93,
                                                     ifelse(summary_tags$year == 2017, 89, 
                                                            ifelse(summary_tags$year == 2018, 56, 56)
                                                     )
                                              ))))




missing_dataframe = as.data.frame(matrix(nrow = 13, ncol = 11))
missing <- c("2016-07", "2016-08", "2016-09", "2016-10", "2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04", "2017-05", "2018-01", "2018-07")
missing_dataframe$monthyear <- missing
new_df <- missing_dataframe %>% select(monthyear, everything())

names <- c("monthyear", "month", "count", "number_sharks", "acoustic_date", "BPV_date", "count_tag", "Boat_station_freq", "year", "stations", "month2")
colnames(new_df) <- names

############## Standardisation 1 ########
summary_tags$month2 <- substr(summary_tags$monthyear, 6, 7)
new_df <- new_df[,-12]
#summary_tags$standard1 <- (summary_tags$count / (summary_tags$Boat_station_freq * summary_tags$count_tag * summary_tags$stations))
trying <- rbind(new_df, summary_tags)
trying$monthyear <- paste0(trying$monthyear, "-01")

summary_tags <- trying[order(as.Date(trying$monthyear, format="%Y-%m-%d")),]

#write.csv(summary_tags,'../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')

##################### Standardised 2 ###########################
summary_tags[is.na(summary_tags)] <- 0

summary_tags$standard2 <- (as.numeric(summary_tags$count) / (as.numeric(744) * as.numeric(summary_tags$count_tag) * as.numeric(summary_tags$stations)))
write.csv(summary_tags,'../results/acoustic_GPS/permute_test/summary_permute_5_20.csv')


#pdf("../results/acoustic_GPS/AG_NO_REPEAT_standardised_2_OVERLAP_overlaid.pdf")
ggplot(summary_tags, aes(x=monthyear, y=standard2, group = 1)) + 
  geom_line() +
  geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#dev.off()

######   


original_summary <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')
permute1 <- read.csv("../results/acoustic_GPS/permute_test/summary_permute_1_20.csv")
permute2 <- read.csv("../results/acoustic_GPS/permute_test/summary_permute_2_20.csv")
permute3 <- read.csv("../results/acoustic_GPS/permute_test/summary_permute_3_20.csv")
permute4 <- read.csv("../results/acoustic_GPS/permute_test/summary_permute_4_20.csv")
permute5 <- read.csv("../results/acoustic_GPS/permute_test/summary_permute_5_20.csv")

pdf("../results/acoustic_GPS/permute_test/permute_figure.pdf")
ggplot(original_summary, aes(x=monthyear, y=standard2, group = 1)) + 
  geom_line(original_summary, mapping =aes(x=monthyear, y=standard2, group = 1), colour = "black", size = 2) +
  geom_line(permute1,mapping =  aes(x=monthyear, y=standard2, group = 1), colour="orange", size = 1) +
  geom_line(permute2, mapping = aes(x=monthyear, y=standard2, group = 1), colour="red", size = 1) +
  geom_line(permute3, mapping = aes(x=monthyear, y=standard2, group = 1), colour="pink", size = 1) +
  geom_line(permute4, mapping = aes(x=monthyear, y=standard2, group = 1), colour="green", size = 1) +
  geom_line(permute5, mapping = aes(x=monthyear, y=standard2, group = 1), colour="blue", size = 1) +
  geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  #geom_line(summary,mapping =  aes(x=acoustic_date, y=standard2, group = 1), colour="#000099") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


dev.off()





############### notes 

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
pdf("../results/acoustic_GPS/permute_test/permute_figure.pdf")
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
sep.km   <- 20
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




