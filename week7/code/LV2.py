#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: LV2.py
#Desc:  Plots Consumer-resource population dybamics and runs the 
# Lotka-Volterra model for predator prey systems. This script is codifying the Lotka-Volterra model 
# for predator-prey system in two-dimensional space (e.g. on land). 
#Arguments: Has default arguments but can input LV model parameters manually: r: is intrinsic growth rate of resource population
#a: per-capita "search rate", i.e. the rate at which consumers find resources units: area per time unit
#z: mortality rate - units are 'per time'
#e" consumer's efficiency (rate in turning mass into its own mass). no units.
#Outputs: Plots two models; consumer-resource model and LV model into one subplot called consumer_resource_model_LV2.pdf
#Date: Oct 2019 

import numpy as np
import scipy as sc
import sys
import matplotlib.pylab as p
import scipy.integrate as integrate

""" Similar to LV1 but requires inputs into the parameters but does use default values """

"""
This script is codifying the Lotka-Volterra model for predator-prey system
in two-dimensional space (e.g. on land).

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

def dCR_dt(pops, t=0):
    """ Defines the Lotka volterra model with carry capacity of population e.g. two dimensional space """
    R = pops[0]
    C = pops[1]
    dRdt = r * R * (1- (R / K)) - a * R * C 
    dCdt = -z * C + e * a * R * C
    
    return sc.array([dRdt, dCdt])

# takes systemarguments as floats so decimals are allowed but if they are not supplied e.g. if there are less than 
# 5 system arguments inputted then it takes default values 
if len(sys.argv)== 5:
    r = float(sys.argv[1])
    a = float(sys.argv[2])
    z = float(sys.argv[3])
    e = float(sys.argv[4])
else: 
    print("Using default arguments:")
    r = 1
    a = 0.1 
    z = 1.5
    e = 0.75

#K used 
K = 30

# an array of consumer resource is created to be put into the function 
R0 = 10
C0 = 5 
RC0 = sc.array([R0, C0])

#time series 
t = sc.linspace(0, 15, 1000)
#Model is run with array of consumer resorce 
pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)

# two plots are opened next to each other 
fig, (ax1, ax2) = p.subplots(1,2)
# plots are indexes so both pltos are plotted ont he same time series e.g. consumer and resource compared along 
#the same time series 
ax1.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
ax1.plot(t, pops[:,1]  , 'b-', label='Consumer density')
ax1.legend(loc='best') # adds legend 
ax1.set(xlabel = 'Time', ylabel = 'Population density') # x and y labels are put in 
p.ylabel('Population density')
ax1.set_title('Consumer-Resource population dynamics')
p.show()# To display the figure

# want the input variables to be displayed on the pltos 
textstr = ' '.join(("r =", (str(r)))) #
textstr1 = ' '.join(("a =", (str(a))))
textstr2 = ' '.join(("z =", (str(z))))
textstr3 = ' '.join(("e =", (str(e))))
final = '\n'.join((textstr, textstr1, textstr2, textstr3)) # append strings into one file 
props = dict(boxstyle='round', facecolor='wheat', alpha=0.5) #make box 
ax2.text(0.05, 0.95, final, transform=ax2.transAxes, fontsize=9,
        verticalalignment='top', bbox=props) #position box 
props = dict(boxstyle='round', facecolor='wheat', alpha=0.5)
ax2.plot(pops[:,1], pops[:,0]  , 'r-')
p.legend(loc='best')
p.xlabel('Resource density')
p.ylabel('Consumer density')
p.show()# To display the figure

fig.savefig('../results/consumer_resource_model_LV2.pdf') #save figure 

