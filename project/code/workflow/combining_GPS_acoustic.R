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

BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_no_dg.csv")
GPS_acoustic <- read.csv("New_data_no_dg_hour/acoustic_GPS_no_elas.csv")

m1 <- merge(GPS_acoustic, BPV, by = "Date")
m1 <- m1[,-2]
m1 <- m1[,-6]

cols_10 <- c("Date", "Longitude_GPS", "Latitude_GPS","Code","station", "Longitude_BPV", "Latitude_BPV", "NewDate")

colnames(m1) = cols_10

okay <- data.table(m1)
okay$Longitude_GPS <- as.numeric(okay$Longitude_GPS)
okay$Latitude_GPS <- as.numeric(okay$Latitude_GPS)


all_combined <- okay %>%
  nest(data= -NewDate) #

############# PLOTTING INITIAL DATA TO HAVE A LOOK AT SPATIAL DISTRIBUTION #####
#GPS_acoustic <- read.csv("acoustic_no_elas.csv")
#GPS_acoustic <- GPS_acoustic[!duplicated(GPS_acoustic[c('Date', 'Code')]),] 
#pdf("../results/EVERY_acoustic_no_repeat.pdf")
#ggplot() + 
#  geom_point() +
#  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
#  ggtitle("All acoustic data") +
#  geom_hex(data=GPS_acoustic, aes(x= Longitude, y= Latitude, fill = stat(log(count)))) +
#  scale_fill_continuous(type = "viridis", limits = c(0, 11), oob = scales::squish) +
#  theme_bw() + 
  #geom_density2d() + stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon') +
#  coord_equal() +
#  ggsn::scalebar(Chagos_island,transform = T, dist = 50, dist_unit = "km", model = 'WGS84') +
  #scale_fill_gradient(low = "green", high = "red") +
#  scale_alpha(range = c(0.00, 0.25), guide = FALSE)
#dev.off()


pdf("../results/acoustic_GPS/EVERY_acoustic_GPS_BPV_nodg.pdf")
for (i in 1:length(all_combined$NewDate)){
  monthdata <- all_combined$data[[i]]
  month <- all_combined$NewDate[[i]]
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude_BPV, y= Latitude_BPV)) + 
    geom_point() +
    geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(data=monthdata, aes(x= Longitude_GPS, y= Latitude_GPS, fill = stat(log(count)))) +
    scale_fill_continuous(type = "viridis", limits = c(0, 10), oob = scales::squish) +
    theme_bw() + 
    geom_density2d() + stat_density2d(aes(fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon') +
    coord_equal() +
    ggsn::scalebar(Chagos_island,transform = T, dist = 50, dist_unit = "km", model = 'WGS84') +
    #scale_fill_gradient(low = "green", high = "red") +
    scale_alpha(range = c(0.00, 0.25), guide = FALSE)
  plot(year_islands_i)
}
dev.off()


#ggplot(data=monthdata2, aes(x= Longitude_GPS, y= Latitude_GPS)) +
 # geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  #xlim(70,74) +
  #ylim(-8.5, -4) +
  #geom_hex(aes(fill = stat(log(count)), xbins = 60)) +
  #scale_fill_continuous(type = "viridis", limits = c(0, 7), oob = scales::squish) +
  #coord_equal() +
  #ggsn::scalebar(Chagos_island,transform = T, dist = 25, dist_unit = "km", model = 'WGS84') 
  #geom_point(data=monthdata2, aes(x= Longitude_GPS, y= Latitude_GPS), shape = 23, fill = "orange", size = 3) +
  #geom_blank()


######################


all_overlap <- okay

#df.new <- as.numeric(levels(df.new$Longitude))[df.new$Longitude]
#list2$latitude.fix <- as.numeric(levels(list2$latitude))[list2$latitude]
######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 20
all_overlap$distance<-distHaversine(all_overlap[,2:3], all_overlap[,6:7], r=r.km)
all_10_overlap <- all_overlap[all_overlap$distance<sep.km,]

####### VERY IMPORTANT NOT TO INCLUDE REPEATING DATA
#write.csv(all_10_overlap, "../results/acoustic_GPS/Ac_GPS_all_10_overlap_no_DG.csv")
#overlap <- read.csv("../results/acoustic_GPS/Ac_GPS_all_10_overlap_no_DG.csv")
overlap <- all_10_overlap
overlap <- as.data.frame(overlap)

####### have decided to go with this method of subsetting as it also removes multiple station detections 
new_uniq <- overlap[!duplicated(overlap[c('Date', 'Code')]),] 
#new_uniq_stations <- overlap[!duplicated(overlap[c('Date', 'Code', 'station')]),] 
#station_dup <- new_uniq_stations[duplicated(new_uniq_stations[,c('Date', 'Code')]),]


write.csv(new_uniq, "../results/acoustic_GPS/NO_REPEAT_Ac_GPS_all_10_overlap_no_DG.csv")
overlap <- read.csv("../results/acoustic_GPS/NO_REPEAT_Ac_GPS_all_10_overlap_no_DG.csv")

#View(new_uniq) 

all_10_overlap = overlap

all_nestmonths <- all_10_overlap %>%
  nest(data= -NewDate) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 


summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
pdf("../results/acoustic_GPS/AG_NR_all_overlap_no_dg_NOREPEAT.pdf")
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude_GPS, y= Latitude_GPS)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(aes(x=Longitude_GPS,y=Latitude_GPS)) +
    scale_fill_continuous(type = "viridis", limits = c(0, 100), oob = scales::squish) +
    coord_equal() +
    ggsn::scalebar(Chagos_island,transform = T, dist = 50, dist_unit = "km", model = 'WGS84') +
    theme_bw()
  toadd <- c(month, rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
  plot(year_islands_i)
}
dev.off()

summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 4))
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  toadd <- c(as.character(month), rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
}



pdf("../results/acoustic_GPS/AG_NR_all_overlap_no_dg_NOREPEAT_LOG.pdf")
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude_GPS, y= Latitude_GPS)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    ggtitle(month) +
    geom_hex(aes(fill = stat(log(count)))) +
    scale_fill_continuous(type = "viridis", limits = c(0, 4), oob = scales::squish) +
    coord_equal() +
    ggsn::scalebar(Chagos_island,transform = T, dist = 25, dist_unit = "km", model = 'WGS84') +
    theme_bw()
  plot(year_islands_i)
}
dev.off()

##### 
summary <- c("month", "count", "number_sharks")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]
summary_sharks <- summary_sharks[,-4]
write.csv(summary_sharks,'../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')
summary <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')


################################## FINDING THE TAGS AT LIBERTY ####################




GPS_acoustic$Date <- substr(GPS_acoustic$Date, 0, 10)
#acoustic$Date <- as.POSIXct(acoustic$Date, format="%Y-%m-%d")

code_nest <- GPS_acoustic %>%
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
write.csv(adding, "../results/acoustic_GPS/AG_standardising_tags.csv")  

summary_tags <- summary
summary_tags$month <- paste0(summary_tags$month, "-15")



################################## FINDING THE TAGS AT LIBERTY ####################

tags_at_liberty <- read.csv("../results/acoustic_GPS/AG_standardising_tags.csv")  


summary_tags$count_tag <- 
  sapply(summary_tags$month, function(x)
    sum(as.Date(tags_at_liberty$min, "%Y-%m-%d") <= as.Date(x, "%Y-%m-%d") &
          as.Date(tags_at_liberty$max, "%Y-%m-%d") >= as.Date(x, "%Y-%m-%d")))

################# second way of doing this which i went with, this way is the same as far as I can tell but I add 15 to the day so it takes mid week 
#tags_2 <- tags_at_liberty
#tags_2$min <- substr(tags_2$min, 0, 7)
#tags_2$max <- substr(tags_2$max, 0, 7)
#sum_tag <- summary_tags
#sum_tag$month <- substr(sum_tag$month, 0, 7)
#sum_tag$month <- paste0(sum_tag$month, "-15")

#sum_tag$count_tag <- 
#  sapply(sum_tag$month, function(x)
#    sum(as.Date(tags_2$min, "%Y-%m-%d") <= as.Date(x, "%Y-%m-%d") &
#          as.Date(tags_2$max, "%Y-%m-%d") >= as.Date(x, "%Y-%m-%d")))

###### testing if this works- youd expect 5 This one is a test of tags at liberty 

#sum(as.Date(date, "%Y-%m-%d") <= as.Date(summary_tags$month[19], "%Y-%m-%d") &
#      as.Date(date2, "%Y-%m-%d") >= as.Date(summary_tags$month[19], "%Y-%m-%d"))

#date <- c("2014-03-01", "2014-01-01", "2013-03-04", "2018-01-01", "2015-07-01", "2015-08-01", "2015-01-01", "2014-01-01", "2015-01-01", "2015-08-01", "2015-01-01")
#date2 <- c("2018-02-01", "2014-05-01", "2015-08-01", "2017-02-01", "2016-02-01", "2015-06-01", "2015-04-01", "2016-04-01", "2016-04-01", "2015-09-01", "2015-04-01")



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

BPV_stations$monthyear<- substr(BPV_stations$Date, 0, 7)
summary_tags$monthyear<- substr(summary_tags$month, 0, 7)


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

summary_tags$year <- substr(summary_tags$month, 0, 4)
summary_tags$stations <- ifelse(summary_tags$year == 2013, 29,
                                ifelse(summary_tags$year == 2014, 49, 
                                       ifelse(summary_tags$year == 2015, 64, 
                                              ifelse(summary_tags$year == 2016, 93,
                                                     ifelse(summary_tags$year == 2017, 89, 
                                                            ifelse(summary_tags$year == 2018, 56, 56)
                                )
))))




missing_dataframe = as.data.frame(matrix(nrow = 13, ncol = 10))
missing <- c("2016-07", "2016-08", "2016-09", "2016-10", "2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04", "2017-05", "2018-01", "2018-07")
new_df <- cbind(missing, missing_dataframe)



names <- c("monthyear", "X", "month", "count", "number_sharks", "count_tag", "Boat_station_freq", "year", "stations", "standard1", 'standard3')
colnames(new_df) <- names

############## Standardisation 1 ########
#summary_tags <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')
summary_tags$month <- substr(summary_tags$monthyear, 6, 7)
summary_tags$standard1 <- (summary_tags$count / (summary_tags$Boat_station_freq * summary_tags$count_tag))
trying <- rbind(new_df, summary_tags)
trying$monthyear <- paste0(trying$monthyear, "-01")
summary_tags$standard3 <- (summary_tags$count / (summary_tags$Boat_station_freq * summary_tags$number_sharks))


summary_tags <- trying[order(as.Date(trying$monthyear, format="%Y-%m-%d")),]
summary_tags$monthyear <- substr(summary_tags$monthyear, 0 ,7)
#write.csv(summary_tags,'../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')

scale_fill_manual(name = "Area", labels = c("Other", "Hrs in DG", "Hrs patrolling"), values = c('#798E87','#C27D38','#CCC591', '#29211F', '#02401B', '#972D15')) 
pal <- RColorBrewer::brewer.pal(13, "wes_palette")[1:5]
RColorBrewer::brewer.pal(5, wes_palette("FantasticFox1"))

pdf("../results/acoustic_GPS/AG_NO_REPEAT_standardised_1_OVERLAP_overlaid.pdf")
ggplot(summary_tags, aes(x= as.factor(month), y=standard1, fill=factor(year), colour = factor(year), group=factor(year))) + geom_line(size=1.1) + 
  geom_point(size = 3) +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to the potential (station~BPV)") +
  #scale_shape_manual(values = c(0,1,3,0,1,3)) +
  scale_colour_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) + 
  scale_fill_manual(name = "Year", values = c('#B40F20', '#046C9A', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

pdf("../results/acoustic_GPS/AG_NO_REPEAT_standardised_1_OVERLAP_plots_not_overlaid.pdf")
ggplot(summary_tags, aes(x=monthyear, y=standard1, group = 1)) + 
  geom_line() +
  geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to the potential (station~BPV)") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

lm(standard~X, data = summary_tags)
summary(lm(summary_tags$standard3 ~ poly(summary_tags$X, 2, raw = TRUE)))



##################### Standardised 2 ###########################
#BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_no_dg.csv")

BPV$year <- substr(BPV$Date, 0, 4)
BPV$month <- substr(BPV$Date, 6, 7)

nest_BPV <- BPV %>%
  nest(data= -NewDate)

#pdf("../results/acoustic/stat_each_month_BPV_plotted_compare.pdf")

priority = as.data.frame(matrix(nrow = 1, ncol = 2))
rows <- c("monthyear", "actual_hours")
colnames(priority) <- rows
for (i in 1:length(nest_BPV$NewDate)){
  monthdata <- nest_BPV$data[[i]]
  month <- nest_BPV$NewDate[[i]]
  rows <- nrow(monthdata)
  new <- c(as.character(month), rows)
  priority <- rbind(priority, new)
}

priority <- priority[-1,]

summary_tags <- merge(summary_tags, priority, by = "monthyear", all.x = T)

summary_tags$actual_hours <- as.numeric(as.character(summary_tags$actual_hours))

summary_tags$standard2 <- (summary_tags$count / (summary_tags$actual_hours * summary_tags$count_tag))
summary_tags$standard4 <- (summary_tags$count / (summary_tags$actual_hours * summary_tags$number_sharks))


pdf("../results/acoustic_GPS/AG_NO_REPEAT_standardised_2_OVERLAP_overlaid.pdf")
ggplot(summary_tags, aes(x=month, y=standard4, fill= factor(year), colour = factor(year), group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  scale_colour_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) + 
  scale_fill_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

pdf("../results/acoustic_GPS/AG_NO_REPEAT_standardised_2_OVERLAP_plots_not_overlaid.pdf")
ggplot(summary_tags, aes(x=monthyear, y=standard4, group = 1)) + 
  geom_line() +
  geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

lm(standard~X, data = summary_tags)
summary(lm(summary_tags$standard4 ~ poly(summary_tags$X, 2, raw = TRUE)))


######   
###################### Making barchart of effort through months #############

write.csv(summary_tags, '../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')


summary_tags$year <- substr(summary_tags$monthyear, 0, 4)
#barplot(summary_tags$Boat_station_freq)

#counts <- table(summary_tags$Boat_station_freq, summary_tags$X)

pdf("../results/acoustic_GPS/AG_NR_barplot_hours_BPV_station_10km.pdf")
barplot(summary_tags$Boat_station_freq, main ="Number of hours BPV vessel was within 10km of a station within a month", ylim = c(0, 744), ylab="Hours in a month", names.arg= summary_tags$monthyear, las=2) 
dev.off()

summary_tags$proportion_sharks <- summary_tags$number_sharks / summary_tags$count_tag

pdf("../results/acoustic_GPS/AG_NR_barplot_proportion_sharks_overlapped_liberty.pdf")
barplot(summary_tags$proportion_sharks, main ="Proportion of sharks overlapped compared to total sharks at liberty per month", ylab="Proportion of sharks overlapped", names.arg= summary_tags$monthyear, las=2) 
dev.off()


##################### MAKING AN OVERAL DENSITY PLOT 
pdf("../results/acoustic_GPS/AG_NR_overall_density_overlap_no_dg.pdf")
ggplot(data=overlap, aes(x= Longitude_GPS, y= Latitude_GPS)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  ggtitle(month) +
  geom_hex(aes(x=Longitude_GPS,y=Latitude_GPS)) +
  scale_fill_continuous(type = "viridis", limits = c(0, 1000), oob = scales::squish) +
  theme_bw()
dev.off()


############ Overall plot 
IUU <- read.csv("IUU_Data_catches.csv", header = T)
IUU <- IUU[,1:4]

overlap <- read.csv("../results/acoustic_GPS/NO_REPEAT_Ac_GPS_all_10_overlap_no_DG.csv")
pal <- wes_palette("Zissou1", 100, type = "continuous")


pdf("../results/acoustic_GPS/scalebar_IUU_LOG_HEX_AG_NR_overall_density_overlap_no_dg.pdf")
ggplot(data=overlap, aes(x= Longitude_GPS, y= Latitude_GPS)) + 
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  xlim(70,74) +
  ylim(-8.5, -4) +
  geom_hex(aes(fill = stat(log(count)), xbins = 60)) +
  scale_fill_continuous(type = "viridis", limits = c(0, 7), oob = scales::squish) +
  #scale_fill_gradientn(colours = pal) +
  coord_equal() +
  ggsn::scalebar(Chagos_island,transform = T, dist = 50, dist_unit = "km", model = 'WGS84') +
  geom_point(data= IUU, aes(x= Longitude, y= Latitude), shape = 23, fill = "orange", size = 3) +
  theme_bw() 
dev.off()

pdf("../results/IUU_MPA.pdf")
ggplot() + 
  geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  #scale_fill_gradientn(colours = pal) +
  coord_equal() +
  ggsn::scalebar(Chagos_try,transform = T, dist = 100, dist_unit = "km", model = 'WGS84') +
  geom_point(data= IUU, aes(x= Longitude, y= Latitude), shape = 23, fill = "orange", size = 3) +
  theme_bw() 
dev.off()

new <- table(IUU$Year)
new <- as.data.frame(new)
new2 <- new[7:14,]

lm(new$X, data = new)
plot(new$X, new$Freq)
abline(lm(new$Freq ~ new$X))
summary(lm(new$Freq ~ poly(new$X, 2, raw = TRUE)))

new2 <- new2[-8,]
new2$X <- 1:nrow(new2)
plot(new2$X, new2$Freq)
abline(lm(new2$Freq ~ new2$X))
summary(lm(new2$Freq ~ poly(new2$X, 3, raw = TRUE)))




