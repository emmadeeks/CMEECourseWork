# Some imports to explore the datasets
# Import relevant packages for data manipulation
import pandas as pd
import scipy as sc
import matplotlib.pylab as pl
import seaborn as sns # You might need to install this (e.g., sudo pip install seaborn)
import numpy as np
#Read in data 
data = pd.read_csv("../data/CRat.csv")
# Make a new dataframe with the relevant columns of interest 
newdata = data[['ID', 'ResDensity', 'N_TraitValue', 'Habitat', 'Res_MovementDimensionality', 'Con_MovementDimensionality', 'ConTaxa', 'ResTaxa']]
newdata2 = newdata.dropna()
#Count the number of IDs to see if there are any sets of data with less than 5 data points
newdata2.groupby('ID').ID.count()
#Create a threshold of datapoints for the modelling
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
#Check to see if there are any NaN values
#newdata2.isnull().sum()
#Drop NaN values from data frame
newdata_no_missing = newdata2.dropna()
#Check that it has worked 
newdata_no_missing.isnull().sum()
#newdata_no_missing['ConTaxa'] = newdata_no_missing['ConTaxa'].str.split().str[0]
#newdata_no_missing['ResTaxa'] = newdata_no_missing['ResTaxa'].str.split().str[0]
newdata_no_missing.to_csv('../data/modified_CRat.csv')