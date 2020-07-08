rm(list=ls()) #Clear global environment 

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


acoustic <- read.table("/Users/emmadeeks/Dropbox/Overlap_data/Chagos_ALL_acoustic_2019.txt", header = TRUE, sep = ",", dec = ".") #read in the data 

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data/sat_tag_stuff")

code_needed <- acoustic[acoustic$code == '19505', ]

unique(code_needed$receiver)



new <- data.frame()
sort_data <- function(code_needed) {code_needed$date <- substr(code_needed$detect_date, 0, 10)
code_needed$time <- substr(code_needed$detect_date, 12, 19)
code_needed <- code_needed[!duplicated(code_needed[c('date', 'station')]),] 
new <- cbind(code_needed$date, code_needed$time, code_needed$receiver_lat, code_needed$receiver_lon)
new <- as.data.frame(new)
new$V1 <- format(as.Date(new$V1, format = "%Y-%m-%d"), "%d-%b-%Y")
name <- code_needed$code[1]
write.table(new, file = paste(name, "GPSsat.txt", sep = "_"), col.names = F, row.names = F, quote = FALSE)
#print(new)
return(new)
}

sort_data(code_needed)






