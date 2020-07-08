rm(list=ls()) #Clear global environment 
#setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data/shape_files/")

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
BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_no_dg.csv")

BPV$year <- substr(BPV$Date, 0, 4)
BPV$month <- substr(BPV$Date, 6, 7)

nest_BPV <- BPV %>%
  nest(data= -NewDate)

#pdf("../results/acoustic/stat_each_month_BPV_plotted_compare.pdf")

#priority = as.data.frame(matrix(nrow = 1, ncol = 2))
#rows <- c("monthyear", "actual_hours")
#colnames(priority) <- rows
#for (i in 1:length(nest_BPV$NewDate)){
#monthdata <- nest_BPV$data[[i]]
#month <- nest_BPV$NewDate[[i]]
#rows <- nrow(monthdata)
#new <- c(as.character(month), rows)
#priority <- rbind(priority, new)
#}

summary_tags <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')
#final <- read.csv('../results/acoustic_GPS/IUU_POTENIAL_ORIGINAL_summary_sharks_no_dg_NOREPEATS.csv')

#summary_tags <- merge(summary_tags, priority, by = "monthyear")


GPS_acoustic <- read.csv("New_data_no_dg_hour/acoustic_GPS_no_elas.csv")
BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_no_dg.csv")

GPS_acoustic$NewDate <- substr(GPS_acoustic$Date, 0 , 7)


overlap_20km <- read.csv("../results/acoustic_GPS/NO_REPEAT_Ac_GPS_all_10_overlap_no_DG.csv")
#high_month_BPV <- BPV[BPV$NewDate == '2017-09',]
#high_month_ac <- GPS_acoustic[GPS_acoustic$NewDate == '2017-09',]

#m1 <- merge(GPS_acoustic, BPV, by = "Date")
#m1 <- m1[!duplicated(m1[c('Date', 'Code')]),] 

all_combined <- overlap_20km %>%
  nest(data= -Date) #

########## This dataframe now has the number of sharks detected in 
priority = as.data.frame(matrix(nrow = 1, ncol = 2))
names <- c("monthyear", "unique_total_sharks")
colnames(priority) <- names
for (i in 1:length(all_combined$Date)){
  monthdata <- all_combined$data[[i]]
  month <- all_combined$Date[[i]]
  variable <- unique(monthdata$Code)
  high_rows <- length(variable)
  date <- monthdata$NewDate.x[1]
  bind <- c(as.character(date), high_rows)
  priority <- rbind(priority, bind)
}

priority <- priority[-1,]
all_nestmonths <- priority %>%
  nest(data= -monthyear)


summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 2))
for (i in 1:length(all_nestmonths$monthyear)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$monthyear[[i]]
  no_sharks <- sum(as.numeric(monthdata$unique_total_sharks))
  toadd <- c(as.character(month), no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
}


summary <- c("monthyear", "total_number_sharks")
colnames(summary_sharks) <- summary
summary_sharks <- summary_sharks[-1,]

summary_tags <- merge(summary_tags, summary_sharks, by = "monthyear")
summary_tags$total_number_sharks <- as.numeric(as.character(summary_tags$total_number_sharks))
summary_tags$count_tag <- as.numeric(as.character(summary_tags$count_tag))
summary_tags$actual_hours <- as.numeric(as.character(summary_tags$actual_hours))

summary_tags$O <- (summary_tags$count/(summary_tags$actual_hours * summary_tags$count_tag))
summary_tags$P <- (summary_tags$total_number_sharks/ (summary_tags$actual_hours * summary_tags$count_tag))
summary_tags$potential <- summary_tags$O / summary_tags$P


  ggplot(summary_tags, aes(x=month, y=potential, fill= factor(year), colour = factor(year), group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 2, shape=21) +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  ggtitle("Standardisation 2: Hours of months") +
  scale_colour_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) + 
  scale_fill_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  
    ggplot(summary_tags, aes(x=monthyear, y=potential, group = 1)) + 
    geom_line() +
    geom_point() + 
    ggtitle("Standardisation 3: incl stations") +
    stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
    xlab("Month") +# for the x axis label
    ylab("Proportion of successful overlaps") +
    theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

    summary(lm(summary_tags$potential ~ poly(summary_tags$X, 2, raw = TRUE)))  

    
    
  summary_tags$standard2 <- summary_tags$count / (summary_tags$actual_hours * summary_tags$count_tag)
    
  ggplot(summary_tags, aes(x=month, y=standard2, fill= factor(year), colour = factor(year), group=factor(year))) + geom_line(size=0.8) + 
    geom_point(size = 2, shape=21) +
    xlab("Month") +# for the x axis label
    ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
    ggtitle("Standardisation 2: Hours of months") +
    scale_colour_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) + 
    scale_fill_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  
  ggplot(summary_tags, aes(x=monthyear, y=standard2, group = 1)) + 
    geom_line() +
    geom_point() + 
    ggtitle("Standardisation 3: incl stations") +
    stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
    xlab("Month") +# for the x axis label
    ylab("Proportion of successful overlaps") +
    theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  summary(lm(summary_tags$standard2 ~ poly(summary_tags$X, 2, raw = TRUE)))  
  
write.csv(summary_tags, "../results/acoustic_GPS/updated_AG_NR_summary_sharks_no_dg_NOREPEATS.csv")  
  
      