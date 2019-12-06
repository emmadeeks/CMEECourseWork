#!/usr/bin/env Rscript 

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: DataWrangTidy.R
#Desc: Script illustrating how to 'wrangle' data using the 'dplyr' and 'tidyr' package and other functions in R	
#Arguments: No manual input required but uses the PoundHillData.csv and PoundHillMetaData.csv from data
#Outputs: Both inputted datasets are in an improved format and under variable names 'MyData' and 'MyMetaData' respectively
#Date: Oct 2019  
################################################################
################## Wrangling the Pound Hill Dataset ############
################################################################

############# Load the dataset ###############
# header = false because the raw data don't have real headers
MyData <- as.matrix(read.csv("../data/PoundHillData.csv",header = F))

# header = true because we do have metadata headers
MyMetaData <- read.csv("../data/PoundHillMetaData.csv",header = T, sep=";", stringsAsFactors = F)

############# Inspect the dataset ###############
require(dplyr) #Instead of reshape its the dplyr and tidyr packages 
require(tidyr)

glimpse(MyData) # different ways to look at data using diplyr
tbl_df(MyData) # different ways to look at data using diplyr
dim(MyData)
fix(MyData) #you can also do this
fix(MyMetaData)

############# Transpose ###############
# To get those species into columns and treatments into rows
MyData <- t(MyData)
head(MyData)
dim(MyData)

############# Replace species absences with zeros ###############
MyData[MyData == ""] = 0

############# Convert raw matrix to data frame ###############

TempData <- as.data.frame(MyData[-1,],stringsAsFactors = F)#stringsAsFactors = F is important!
colnames(TempData) <- MyData[1,] # assign column names from original data

############# Convert from wide to long format  ###############

require(dplyr)
require(tidyr)
#This is using gather to convert data from wide to long format
# instead of melt from reshape2 gather is used to convert data from wide to long format 
MyWrangledData <- TempData %>% gather(Species, Count, -Cultivation, -Block, -Plot, -Quadrat)

MyWrangledData[, "Cultivation"] <- as.factor(MyWrangledData[, "Cultivation"])
MyWrangledData[, "Block"] <- as.factor(MyWrangledData[, "Block"])
MyWrangledData[, "Plot"] <- as.factor(MyWrangledData[, "Plot"])
MyWrangledData[, "Quadrat"] <- as.factor(MyWrangledData[, "Quadrat"])
MyWrangledData[, "Count"] <- as.integer(MyWrangledData[, "Count"])

glimpse(MyWrangledData) #some more diplr and tidyr functions 
tbl_df(MyWrangledData)
dim(MyWrangledData)


