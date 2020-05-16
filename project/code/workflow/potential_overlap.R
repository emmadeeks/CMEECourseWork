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


summary_original <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')

data_merge <- cbind(as.character(summary_original$monthyear), summary_original$month, summary_original$count_tag, summary_original$Boat_station_freq, summary_original$year, summary_original$stations)
data_merge <- as.data.frame(data_merge)  
names <- c("NewDate", "month", "count_tag", "Boat_station_freq", "year", "stations")
colnames(data_merge) <- names

potential <- merge(summary_sharks, data_merge, by = "NewDate", all = T)
summary_tags <- potential

#summary_tags$NewDate <- paste0(summary_tags$NewDate, "-01")

#summary_tags$NewDate <- as.Date(summary_tags$NewDate, format="%Y-%m-%d")
#summary_tags$NewDate <- substr(summary_tags$NewDate, 0 ,7)

#summary_tags <- data.table(summary_tags)
summary_tags$standard1 <- (as.numeric(summary_tags$count) / (as.numeric(summary_tags$Boat_station_freq) * as.numeric(summary_tags$count_tag)))
summary_tags$count <- as.numeric(as.character(summary_tags$count))
summary_tags$count_tag <- as.numeric(as.character(summary_tags$count_tag))

summary_tags$standard2 <- summary_tags$count / (744 * summary_tags$count_tag)



#pdf("../results/acoustic_GPS/AG_NO_REPEAT_standardised_2_OVERLAP_overlaid.pdf")
ggplot(summary_tags, aes(x=month, y=standard2, fill= factor(year), colour = factor(year), group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  scale_colour_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) + 
  scale_fill_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#dev.off()

#pdf("../results/acoustic_GPS/AG_NO_REPEAT_standardised_2_OVERLAP_plots_not_overlaid.pdf")
ggplot(summary_tags, aes(x=NewDate, y=standard2, group = 1), colour = '#B40F20') + 
  geom_line(size = 1) +
  #geom_point() + 
  geom_line(summary_original, mapping = aes(x = monthyear, y = standard2, group = 1), colour = 'dark green', size = 1) +
  #scale_colour_manual(values = c('#B40F20', 'dark green')) +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#pdf("../results/acoustic_GPS/actual_vs_potential_overlap_STANDARDISATION_2_no_repeats.pdf")
ggplot() + 
  geom_line(summary_tags, mapping = aes(x=NewDate, y=standard2, group = 1, colour = "Potential overlap"), size = 1) +
  geom_point() + 
  geom_line(summary_original, mapping = aes(x = monthyear, y = standard2, group = 1, colour = "Actual overlap"), size = 1) +
  scale_colour_manual("", breaks = c("Potential overlap", "Actual overlap"), values = c('black', 'darkred')) +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#dev.off()

summary(lm(summary_original$standard2 ~ poly(summary_original$X.2, 2, raw = TRUE)))
summary(lm(summary_tags$standard2 ~ poly(summary_tags$X, 2, raw = TRUE)))




IUU <- read.csv("IUU_Data_catches.csv", header = T)
IUU <- IUU[,1:4]

IUU$Year <- paste0(IUU$Year, "-")

IUU$Month <- paste0("0", IUU$Month)
for (i in 1:nrow(IUU)) {
  number <- IUU$Month[i]
  number <- nchar(number)
  if (number == 3) {
    IUU$Month[i] <- substr(IUU$Month[i], 2, 3)
  }
}  

IUU$Year <- paste(IUU$Year, IUU$Month, sep = "")
IUU <- IUU[37:79, ]
IUU_tab <- table(IUU$Year)
IUU_tab <- as.data.frame(IUU_tab)
names <- c("NewDate", "IUU_events")
colnames(IUU_tab) <- names



ggplot() + geom_bar(IUU_tab, mapping = aes(x=NewDate, y=IUU_events), stat="identity", colour = "black", alpha = 0.3) +
  ylab("Number of detections of acoustically tagged sharks standardised by tags at liberty") +
  #geom_line(summary_tags, mapping = aes(x=monthyear, y=standard2, group = 1)) +
  #geom_bar(alpha = 0.5) +
  #theme(axis.text.x = element_text(angle = 90)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), axis.text.x = element_text(angle = 90), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

summary_tags_2 <- merge(summary_tags, IUU_tab, by = "NewDate", all = T)
summary_tags_2$IUU_events[is.na(summary_tags_2$IUU_events)] <- 0

summary_original$std_2_plot <- summary_original$standard2 * 50
summary_tags_2$std_2_plot <- summary_tags_2$standard2 * 50

write_csv(summary_tags_2, '../results/acoustic_GPS/POTENIAL_summary_sharks_no_dg_NOREPEATS.csv')

Fig_3 <- cbind(as.character(summary_original$monthyear), summary_original$std_2_plot)
Fig_3_2 <- cbind(as.character(summary_tags_2$NewDate), summary_tags_2$std_2_plot, summary_tags_2$IUU_events)

names <- c("Month", "Original_overlap")
colnames(Fig_3) <- names
names <- c("Month", "Potential_overlap", "IUU_events")
colnames(Fig_3_2) <- names
Fig_3 <- as.data.frame(Fig_3)
Fig_3_2 <- as.data.frame(Fig_3_2)

final <- merge(Fig_3, Fig_3_2, by = "Month")

#names <- c("Month", "Original_overlap", "Potential_overlap", "IUU_events")
#colnames(final) <- names
final <- as.data.frame(final)
final$Potential_overlap <- as.numeric(as.character(final$Potential_overlap))
final$Original_overlap <- as.numeric(as.character(final$Original_overlap))
final$Month <- as.character(final$Month)
final$to_plot <- (final$Original_overlap / final$Potential_overlap) * 10
final$actual_plot <- (final$Original_overlap / final$Potential_overlap)

final$to_plot <- as.numeric(as.character(final$to_plot))
final$IUU_events <- as.numeric(as.character(final$IUU_events))

write.csv(final, '../results/acoustic_GPS/IUU_POTENIAL_ORIGINAL_summary_sharks_no_dg_NOREPEATS.csv')

pdf("../results/acoustic_GPS/Potential_propotion_IUU_15_may.pdf")
ggplot(final, mapping =  aes(x=Month, y=to_plot, group = 1)) + 
  geom_line(size = 1) +
  #geom_point(final, mapping = aes(x=Month, y=as.numeric(IUU_events))) + 
  #ylim(0, 1) +
  #geom_line(summary_original, mapping = aes(x = monthyear, y = std_2_plot, group = 1, colour = "Actual overlap"), size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~. / 9, "Overlap score (between 0 and 1), Potential being location with most sharks")) +
  geom_bar(final, mapping = aes(x=Month, y=IUU_events), stat="identity", colour = "black", alpha = 0.3) +
  scale_x_discrete() +
  #geom_bar(Fig_3, mapping = aes(x=Month, y=IUU_events), stat="identity", colour = "black", alpha = 0.3) +
  #geom_line(summary_tags_2, mapping = aes(x=NewDate, y=standard2, group = 1, colour = "Potential overlap"), size = 1) +
  #scale_colour_manual("", breaks = c("Potential overlap", "Actual overlap"), values = c('black', 'darkred')) +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Date") +# for the x axis label
  ylab("Number of IUU interceptions") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


dev.off()
final$X <- 1:nrow(final)

summary(lm(final$actual_plot ~ poly(final$X, 2, raw = TRUE)))
#summary(lm(summary_tags$standard2 ~ poly(summary_tags$X, 2, raw = TRUE)))

final$year <- substr(final$Month, 0, 4)
final$month_plot <- substr(final$Month, 6, 7)

pdf("../results/acoustic_GPS/Potential_propotion_OVERLAID_15_may.pdf")
ggplot(final, aes(x=month_plot, y=actual_plot, fill= factor(year), colour = factor(year), group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  scale_colour_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) + 
  scale_fill_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()
