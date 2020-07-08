rm(list=ls()) #Clear global environment 

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

library(geosphere)
library(dplyr)


setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_INCLUDE_dg.csv")


#BPV$Longitude <- as.numeric(levels(BPV$Longitude))[BPV$Longitude]
#BPV$Latitude <- as.numeric(levels(BPV$Latitude))[BPV$Latitude]

BPV$Latitude <- as.numeric(as.character(BPV$Latitude))
BPV$Longitude <- as.numeric(as.character(BPV$Longitude))

BPV$day <- substr(BPV$Date, 0, 10)
BPV$NewDate <- substr(BPV$Date, 0, 7)


BPV_loop <- BPV %>%
  nest(data= -day)

mydata <- BPV_loop$data[[1]]


#mydata[(order(as.Date(mydata$Date, format = "%y-%m-%d %H:%M:%S"))),]
mydata <- arrange(mydata, Date)

mydata <- mutate(mydata, 
       Distance = distHaversine(cbind(Longitude, Latitude),
                                cbind(lag(Longitude), lag(Latitude))))

mydata$Distance <- mydata$Distance / 1000

priority = as.data.frame(matrix(nrow = 1, ncol = 2))
rows <- c("day", "km_travelled")
colnames(priority) <- rows
for (i in 1:length(BPV_loop$day)){
  mydata <- BPV_loop$data[[i]]
  mydata <- arrange(mydata, Date)
  mydata <- mutate(mydata, 
                   Distance = distHaversine(cbind(Longitude, Latitude),
                                            cbind(lag(Longitude), lag(Latitude))))
  
  mydata$Distance <- mydata$Distance / 1000
  sum_km <- sum(mydata$Distance, na.rm = T)
  date <- substr(mydata$Date[1], 0 , 10)
  toadd <- c(date, sum_km)
  priority <- rbind(priority, toadd)
}

priority <- priority[-1,]
priority$km_travelled <- as.numeric(as.character(priority$km_travelled))

priority$NewDate <- substr(priority$day, 0, 7)
BPV_AV <- priority %>%
  nest(data= -NewDate)

mean_table = as.data.frame(matrix(nrow = 1, ncol = 2))
rows <- c("monthyear", "average")
colnames(mean_table) <- rows
for (i in 2:length(BPV_AV$NewDate)){
  mydata <- BPV_AV$data[[i]]
  mydata <- as.data.frame(mydata)
  a <- as.numeric(mydata$km_travelled)
  date <- BPV_AV$NewDate[[i]]
  mean <- mean(a)
  toadd <- c(date, mean)
  mean_table <- rbind(mean_table, toadd)
}

mean_table <- mean_table[-1,]

mean_table$average <- as.numeric(as.character(mean_table$average))


ggplot(mean_table, aes(x=monthyear, y=average, group = 1)) + 
  geom_line() +
  geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
mean_table$X <- 1:nrow(mean_table)

lm(average~X, data = summary_tags)
summary(lm(mean_table$average ~ poly(mean_table$X, 1, raw = TRUE)))


p<- ggplot(priority, aes(x=NewDate, y=km_travelled))
p=p+geom_point()
p=p+geom_boxplot()
p

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

pdf("../results/Thesis_figures/figure_3_panel.pdf")
ggplot(priority, aes(x=NewDate, y=km_travelled)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4, colour = "black", fill = "lightblue") + 
  scale_x_discrete(breaks = every_nth(n = 3)) +
  ylab("Kilometers travelled by patrol vessel") + 
  xlab("Month") +
  coord_cartesian(ylim = c(0, 1000)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(),
                                              axis.line = element_line(colour = "black"), panel.background = element_rect(fill='grey96', colour='black')) 

dev.off()


a <- ggplot(priority, aes(x=NewDate, y=km_travelled)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4, colour = "black", fill = "lightblue") + 
  scale_x_discrete(breaks = every_nth(n = 3)) +
  ylab("Kilometers travelled") + 
  xlab("Month") +
  coord_cartesian(ylim = c(0, 1000)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(),
                                              axis.line = element_line(colour = "black"), panel.background = element_rect(fill='grey96', colour='black')) 






summed <- aggregate(km_travelled~NewDate, priority, FUN=sum) 



b <- ggplot(summed, aes(x=NewDate, y=km_travelled, group = 1)) + 
  geom_line(size = 0.9) +
  #geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="blue2", size = 0.8) +
  xlab("Month") +# for the x axis label
  scale_x_discrete(breaks = every_nth(n = 3)) +
  ylab("Total number of kilometers travelled") +
  theme_bw() +  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(),
                        axis.line = element_line(colour = "black"), panel.background = element_rect(fill='grey96', colour='black')) 




n <- plot_grid(a, b, ncol = 1)

pdf("../results/Thesis_figures/figure_3_panel_km.pdf")
n
dev.off()

summed$X <- 1:nrow(summed)

lm(km_travelled~X, data = summed)
summary(lm(summed$km_travelled ~ poly(summed$X, 2, raw = TRUE)))





########## rough 
boxplot(km_travelled~NewDate,
        data=priority,
        ylim = c(0, 1000), 
        xlab="Month",
        ylab="Kilometers travelled by patrol vessel",
        col="lightblue",
        border="black") 


boxplot(km_travelled~NewDate,
        data=priority,
        ylim = c(0, 1000), 
        xlab="Month",
        ylab="Kilometers travelled by patrol vessel",
        col="lightblue",
        border="black") 


