#!/usr/bin/env Rscript

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: TAutoCorr.R
#Desc: An exercise in correlation coefficients and P-values. Calculates the correlation between n-1 pairs of years in temperature, script loads the KeyWestAnnualMeanTemperature data 
#using load and computes the coefficient for this data before randomly shuffling the data 10000 times to randomly permute the time series and then recalculate the correlation 
#coefficient for each randomly permuted year sequence and storing it.
#Arguments: No manual input but uses the KeyWestAnnualMeanTemperature data	
#Outputs: TThe fraction of the correlation coefficients from the previous step were greater that that from step 1. Also out outputs a Latex file interpreting results. Also outputs pdf of graph for lattice.
#Date: Oct 2019  

#an excercise in correlation coefficients and P-values.
#Calculates the correlation between n-1 pairs of years in temperature,
#script loads the KeyWestAnnualMeanTemperature data using load and computes the coefficient for this data
#Then randomly shuffles the data 10000 times to randomly permute the time series and then recalculate the correlation coefficient for each randonly permuted year sequence and storing it.
#Output: The fraction of the correlation coefficients from the previous step were greater that that from step 1.
#Also out outputs a Latex file interpreting results.

load("../data/KeyWestAnnualMeanTemperature.RData")
plot(ats)

#Adding new column
ats$sucyear <- ats$Temp
#Permuting function to shift columns
shift2 <- function(x, n) `length<-`(tail(x, -n), length(x))
#Shifting columns by applying function
ats <- transform(ats, sucyear = shift2(sucyear, 1))
#Removing final row
ats1 <- ats[-nrow(ats),]
#This is removing the first column as its not needed for loop
#Working out correlation and storing to variable
correlation <- cor(ats1$Temp, ats1$sucyear, method = "pearson")

#creating variable with number for for loop- this is mainly for trouble shooting
replicate <- 10000
#Creates empty dataframe
df <- as.data.frame(matrix(nrow = 100, ncol = 2))
#Creates empty vector
add_cor <- c()
#for loop for 1 to the number in vector
#Adds puts column from ats into first row then shuffles it in second row
#Runs correlation
#Shift rows up using re defined function
#removes final line
#Runs correlation and appends to empty vector
for (i in 1:replicate){ #from one to 1000 (replicate)
  df$V1 <- sample(ats$Temp) 
  df$V2 <- df$V1
  df <- transform(df, V2 = shift2(V2, 1))
  ats_df <- df[-nrow(df),]
  add_cor <- append(add_cor, cor(ats_df$V1, ats_df$V2, method = "pearson"))
}

#counts vaues that is higher than the first correlation coeffcient
count = 0
for (x in add_cor){
  if (x > correlation){
    count = count-1
  }
}

#Calculates fraction that is above the first coefficient
fraction <- count/replicate
print(fraction)

#Makes a pdf of results for the Latex summary
pdf("../results/TAutoCorr.pdf")
plot(add_cor, col= "black", ylim = c(-0.4, 0.4), pch = 16, ylab= "Correlation coeffiient") +
  points(correlation, col = "red", pch = 16)
dev.off()



