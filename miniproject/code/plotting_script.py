# Some imports to explore the datasets
# Import relevant packages for data manipulation
import pandas as pd
import scipy as sc
import scipy.stats as stats
import matplotlib.pylab as pl
import seaborn as sns # You might need to install this (e.g., sudo pip install seaborn)
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import researchpy as rp
import statsmodels.api as sm
from statsmodels.formula.api import ols
import pingouin as pg


#Read in data 

data = pd.read_csv("../data/MergedOptTable.csv")
datacon = pd.read_csv("../data/optimisedtable.csv")


### HABITAT ANALYSIS ###


# Summarise AIC by model and habitat
rp.summary_cont(data.groupby(['Habitat','Model']))['AIC']
# Run a two-way ANOVA on the AIC values by Habitat and Model
model = ols('AIC ~ C(Model)*C(Habitat)', data).fit()

# Check if the overall model is significant
print(f"Overall model F({model.df_model: .0f},{model.df_resid: .0f}) = {model.fvalue: .3f}, p = {model.f_pvalue: .4f}")
# Check assumptions of two-way ANOVA are met 
    #Durban-Watson detects the presence of autocorrelation
    #Jarque-Bera tests assumption of normality
    #Omnibus tests the assumption of homogeneity of variance
    #Condition Number assess multicollinearity
        #NB: values over 20 are indicative of multicollinearity. 
model.summary()
# View ANOVA table
res = sm.stats.anova_lm(model, typ=2)
res

####### Plotting boxplot of habitat 

bp = sns.boxplot(y = 'minAIC', x = 'Habitat', data = datacon, palette = "coolwarm", hue = "AIC")
handles, labels = bp.get_legend_handles_labels()
plt.legend(loc = 'upper right', bbox_to_anchor = (1.25, 1), ncol = 1)
#bp.set_ylabels("Freshwater", "Marine", "Terrestrial")
plt.savefig('../results/HabitatCompare')


### 3D resource ANALYSIS ###


# Summarise AIC by model and habitat
rp.summary_cont(data.groupby(['ResDimension','Model']))['AIC']
# Run a two-way ANOVA on the AIC values by Habitat and Model
model = ols('AIC ~ C(Model)*C(ResDimension)', data).fit()

# Check if the overall model is significant
print(f"Overall model F({model.df_model: .0f},{model.df_resid: .0f}) = {model.fvalue: .3f}, p = {model.f_pvalue: .4f}")
# Check assumptions of two-way ANOVA are met 
    #Durban-Watson detects the presence of autocorrelation
    #Jarque-Bera tests assumption of normality
    #Omnibus tests the assumption of homogeneity of variance
    #Condition Number assess multicollinearity
        #NB: values over 20 are indicative of multicollinearity. 
model.summary()
# View ANOVA table
res = sm.stats.anova_lm(model, typ=2)
res

####### Plotting boxplot of habitat 

bp = sns.boxplot(y = 'minAIC', x = 'Res_Dim', data = datacon, palette = "coolwarm", hue = "AIC")
handles, labels = bp.get_legend_handles_labels()
plt.legend(loc = 'upper right', bbox_to_anchor = (1.25, 1), ncol = 1)
#bp.set_ylabels("Freshwater", "Marine", "Terrestrial")
plt.savefig('../results/Res_Dim_Compare')



####### Plotting bar chart of phenomological and mechanistic datasets 

datacon['mecphe'].value_counts().plot(kind='bar')
