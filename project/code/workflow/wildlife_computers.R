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


GPS <- read.csv("/Users/emmadeeks/Dropbox/Emma_sat_tag/122507-1-GPE3_emma.csv") #read in the data

SST <- GPS[GPS$Observation.Type == 'SST', ]

light <- GPS[GPS$Observation.Type == ' Light - Dawn', ]





