
#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: preparing_data.py
#Desc:  Prepared data for the model fitting script in R 
#Arguments: Required Pawar, 2012 CRat.csv dataset 
#Date: March 2020



# Some imports to explore the datasets
# Import relevant packages for data manipulation
import pandas as pd # For manipulating data 
import numpy as np # For replacing NA values in dataframe 
#Read in data 
data = pd.read_csv("../data/CRat.csv")
# Make a new dataframe with the relevant columns of interest 
newdata = data[['ID', 'ResDensity', 'N_TraitValue', 'Habitat', 'Res_MovementDimensionality', 'Con_MovementDimensionality', 'ConTaxa', 'ResTaxa']]
#drop any NA values from the data 
newdata2 = newdata.dropna()
#Count the number of IDs to see if there are any sets of data with less than 5 data points as well as group them 
newdata2.groupby('ID').ID.count()
#Create a threshold of datapoints for the modelling- this is so any curves with less than 5 data points are dropped from the model in order to 
# fit models more precisely and avoid any bad fits from lack of data points 
threshold = 5
#Create a variable called ValueCounts that counts then number of occurances 
# Of each ID
ValueCounts = newdata2['ID'].value_counts()
# Apply the threshold value to the ValueCounts to remove any IDs with less than 5 data points
toremove = ValueCounts[ValueCounts <= threshold].index
#Replace these values with NaNs
newdata2 = newdata2.replace(to_replace = toremove, value = np.nan)
# Inspect data to see if the function worked 
newdata2['ID'].value_counts()

#Drop NaN values from data frame
newdata_no_missing = newdata2.dropna()
#Check that it has worked 
newdata_no_missing.isnull().sum()

# save the data to a new csv with no NA values and all functional response curves with less than 5 points are removed. 
# Data is entitled modified_CRat.csv and is imported through to the R script 
newdata_no_missing.to_csv('../data/modified_CRat.csv')
print("Data has been prepared, moving on to RScript.")