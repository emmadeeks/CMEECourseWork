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


# Summarise AIC by model and habitat
#rp.summary_cont(data.groupby(['Habitat','Model']))['AIC']
# Run a two-way ANOVA on the AIC values by Habitat and Model
#model = ols('AIC ~ C(Model)*C(Habitat)', data).fit()

# Check if the overall model is significant
#print(f"Overall model F({model.df_model: .0f},{model.df_resid: .0f}) = {model.fvalue: .3f}, p = {model.f_pvalue: .4f}")
# Check assumptions of two-way ANOVA are met 
    #Durban-Watson detects the presence of autocorrelation
    #Jarque-Bera tests assumption of normality
    #Omnibus tests the assumption of homogeneity of variance
    #Condition Number assess multicollinearity
        #NB: values over 20 are indicative of multicollinearity. 
#model.summary()
# View ANOVA table
#res = sm.stats.anova_lm(model, typ=2)

aov2 = pg.mixed_anova(data=data, dv='AIC', between='Habitat', within='Model',
                     subject='ID.', correction= False)




#aov = pg.anova(dv='AIC', between='Habitat', data=data,
#          detailed=True)

#aov = pg.mixed_anova(data=data, between='Habitat', within='Model',
 #                    subject='AIC')       

#ModelANOVA = pg.anova(data = data, dv = 'AIC', between = 'Model', detailed = True)


### 
Habitattukey = pg.pairwise_tukey(data = data, dv = 'AIC', between=['Habitat'])


####### Plotting boxplot of habitat 

bp = sns.boxplot(y = 'minAIC', x = 'Habitat', data = datacon, palette = "husl", hue = "AIC")
plt.legend(loc = 'upper left', ncol = 1)
plt.ylabel("Minimum AIC")
plt.savefig('../results/HabitatBoxplot.pdf')


### 3D resource dimension ANALYSIS ###

# Summarise AIC by model and habitat
#rp.summary_cont(data.groupby(['ResDimension','Model']))['AIC']
# Run a two-way ANOVA on the AIC values by Habitat and Model
#model = ols('AIC ~ C(Model)*C(ResDimension)', data).fit()

# Check if the overall model is significant
#print(f"Overall model F({model.df_model: .0f},{model.df_resid: .0f}) = {model.fvalue: .3f}, p = {model.f_pvalue: .4f}")
# Check assumptions of two-way ANOVA are met 
    #Durban-Watson detects the presence of autocorrelation
    #Jarque-Bera tests assumption of normality
    #Omnibus tests the assumption of homogeneity of variance
    #Condition Number assess multicollinearity
        #NB: values over 20 are indicative of multicollinearity. 
#model.summary()
# View ANOVA table
#res = sm.stats.anova_lm(model, typ=2)
aov3 = pg.mixed_anova(data=data, dv='AIC', between='ResDimension', within='Model',
                     subject='ID.', correction=False)

#Tukey post hoc analysis 
tukey = pg.pairwise_tukey(data = data, dv = 'AIC', between=['ResDimension'])


####### Plotting boxplot of resource 

bps2 = sns.boxplot(y = 'minAIC', x = 'Res_Dim', data = datacon, palette = "BrBG", hue = "AIC") #making boxplot 
plt.legend(loc = 'upper left', ncol = 1) #positioning key 
plt.ylabel("Minimum AIC") #making y label 
plt.xlabel("Resource Dimension")
plt.savefig('../results/Res_Dim_Boxplot.pdf') #saving plot 



####### Plotting bar chart of phenomological and mechanistic datasets 

ax = sns.countplot(x="mecphe", data=datacon, palette="GnBu_d") #making a bar plot based on counts of data 
plt.ylabel("Count") #adding labels 
plt.xlabel("Model Type")
plt.savefig('../results/mecphe.pdf') #saving plot in results 