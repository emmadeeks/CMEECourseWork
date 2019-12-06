#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: LV3.py
#Desc:  Extra credit of LV2 and is a discrete time version of the Lv model 
# using a time series and is also profiled
#Arguments: Has default arguments but can input LV model parameters manually: r: is intrinsic growth rate of resource population
#a: per-capita "search rate", i.e. the rate at which consumers find resources units: area per time unit
#z: mortality rate - units are 'per time'
#e" consumer's efficiency (rate in turning mass into its own mass). no units.
#Outputs: Plots two models; consumer-resource model and LV model and saves to results 
#Date: Oct 2019 

import numpy as np
import scipy as sc
import sys
import matplotlib.pylab as p
import scipy.integrate as integrate

""" Similar to LV1 but requires inputs into the parameters but does use default values """

"""
Extra credit of LV2 and is a discrete time version of the Lv model 

This script is codifying the Lotka-Volterra model for predator-prey system
in two-dimensional space (e.g. on land) and is using a discrete time series 

A key difference between LV1 and LV2 is LV2 uses a
modified equation with constant K which is the carrying capacity 
Another key difference is that is takes system arguments 

dR/dt = rR - aCR
dC/dt = -zC + eaCR
C is consumer population abundance, R is resource population abundance
units: number per area unit
r is intrinsic growth rate of resource population
a is per-capita "search rate", i.e. the rate at which consumers find resources
units: area per time unit
z is the mortality rate - units are 'per time'
e is the consumer's efficiency (rate in turning mass into its own mass). no units. """


K = 30

#define time vector integrating from time point 0 to 30 using 1000 sub-divisions of time
#The time series 
t = sc.linspace(1, 15, 100)

#length of the time ereis 
rows = len(t)
# Setting the initial conditions for the two populations (10 resources and 5 consumers per unit area),
#  and convert the two into an array (because our function takes an array as input)
R0 = 10
C0 = 5
RC0 = sc.array([R0, C0])
#Fills array with zeros 
RC = np.zeros([rows,2])
RC[:1] = RC0

#dets default parameters 
r = 1.
a = 0.1 
z = 1.5
e = 0.75

#goes through the length of the time eries 
for i in range(0, len(t)-1):
    RC[i+1][0] = RC[i][0] * (1 + (r * (1 - RC[i][0] / K)) - a * RC[i][1]) #This is the discrete time eries model for resource
    RC[i+1][1] = RC[i][1] * (1 - z + e * a * RC[i][0]) # this is the discrete time series model for the consumers 

#output filled array with the consumer resource at each time series 

fig, (ax1, ax2) = p.subplots(1,2)
# plots against time series and conpares the consumer resource density 
ax1.plot(t, RC[:,0], 'g-', label='Resource density') # Plot
ax1.plot(t, RC[:,1]  , 'b-', label='Consumer density')
ax1.legend(loc='best')
ax1.set(xlabel = 'Time', ylabel = 'Population density')
p.ylabel('Population density')
ax1.set_title('Consumer-Resource population dynamics')
p.show()# To display the figure

#f1.savefig('../results/LV_model_LV2.pdf') #Save figure
textstr = ' '.join(("r =", (str(r))))
textstr1 = ' '.join(("a =", (str(a))))
textstr2 = ' '.join(("z =", (str(z))))
textstr3 = ' '.join(("e =", (str(e))))
final = '\n'.join((textstr, textstr1, textstr2, textstr3))
props = dict(boxstyle='round', facecolor='wheat', alpha=0.5)
ax2.text(0.05, 0.95, final, transform=ax2.transAxes, fontsize=9,
        verticalalignment='top', bbox=props)
props = dict(boxstyle='round', facecolor='wheat', alpha=0.5)
ax2.plot(RC[:,1], RC[:,0]  , 'r-') 
p.legend(loc='best')
p.xlabel('Resource density')
p.ylabel('Consumer density')
p.show()# To display the figure

fig.savefig('../results/consumer_resource_model_LV3.pdf')

