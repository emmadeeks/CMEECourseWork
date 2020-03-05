#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: plotting_script.py
#Desc:  script that takes the two tables produced by the fitting_script.R and creates three plots as well as does statistical analysis on the data 
#Arguments: Two tables, one entitled 'optiisedtable.csv' and the other 'MergedOptTable.csv' from the data directory of the miniproject folder 
#Date: March 2020 

# Some imports to explore the datasets
# Import relevant packages for data manipulation
import pandas as pd #For data wrangling
import seaborn as sns # For plotting
import matplotlib.pyplot as plt #for plotting 
import researchpy as rp #for linear models and statistical analysis 
import statsmodels.api as sm # for anovas 
from statsmodels.formula.api import ols #for anovas 
import pingouin as pg # for statistical analysis 
from scipy.stats import chisquare # for chi square analysis 


#Read in data 

data = pd.read_csv("../data/MergedOptTable.csv")
datacon = pd.read_csv("../data/optimisedtable.csv")


########### COMPARING MECHANISTIC AND PHENMENOLOGICAL MODELS ###################
# conduct a chi squared test 
counts = datacon['mecphe'].value_counts()
chisquare([counts[0], counts[1]])   

#one way anova for comparing all the models overall 
aov = pg.rm_anova(dv='AIC', within='Model',
                 subject='ID.', data=data, detailed=False)



### HABITAT ANALYSIS ###

#Two way anova using piguoin- mixed model with between subject factor 
# Between subject: Habitat, within subject: Model 
aov2 = pg.mixed_anova(data=data, dv='AIC', between='Habitat', within='Model',
                     subject='ID.', correction= False)


### Post hoc analysis 
Habitattukey = pg.pairwise_tukey(data = data, dv = 'AIC', between=['Habitat'])


####### Plotting boxplot of habitat 

bp = sns.boxplot(y = 'minAIC', x = 'Habitat', data = datacon, palette = "husl", hue = "AIC")
plt.legend(loc = 'upper left', ncol = 1) #legend 
plt.ylabel("Minimum AIC") #label 
plt.savefig('../results/HabitatBoxplot.pdf') #save as pdf 
plt.close()

### 3D consumer  dimension ANALYSIS ###
#Two way anova using piguoin- mixed model with between subject factor 
# Between subject: Consumer dimension, within subject: Model 
aov3 = pg.mixed_anova(data=data, dv='AIC', between='ConDimension', within='Model',
                     subject='ID.', correction=False)

#Tukey post hoc analysis 
tukey = pg.pairwise_tukey(data = data, dv = 'AIC', between=['ConDimension'])


####### Plotting boxplot of resource 

bps2 = sns.boxplot(y = 'minAIC', x = 'Con_Dim', data = datacon, palette = "BrBG", hue = "AIC") #making boxplot 
plt.legend(loc = 'upper left', ncol = 1) #positioning key 
plt.ylabel("Minimum AIC") #making y label 
plt.xlabel("Consumer Dimension") #making x label 
plt.savefig('../results/Res_Dim_Boxplot.pdf') #saving plot 
plt.close()


####### Plotting bar chart of phenomological and mechanistic datasets 

ax = sns.countplot(x="mecphe", data=datacon, palette="GnBu_d") #making a bar plot based on counts of data 
plt.ylabel("Count") #adding labels 
plt.xlabel("Model Type")
plt.savefig('../results/mecphe.pdf') #saving plot in results 
plt.close()