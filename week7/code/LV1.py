#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: LV1.py
#Desc:  Plots Consumer-resource population dybamics and runs the Lotka-Volterra model for predator prey systems 
#Arguments: no input 
#Outputs: Plots two models; consumer-resource model and LV model
#Date: Oct 2019 

import scipy as sc
import matplotlib.pylab as p
import scipy.integrate as integrate

""" Plots Consumer-Resource population dynamics. Runs the Lotka-Volterra model for predator-prey system
in two-dimensional space
C is consumer population abundance, 
R is resource population abundance
units: number per area unit
r is intrinsic growth rate of resource population
a is per-capita "search rate", i.e. the rate at which consumers find resources
units: area per time unit
z is the mortality rate - units are 'per time'
e is the consumer's efficiency (rate in turning mass into its own mass). no units. """

def dCR_dt(pops, t=0):
    """ Defines function to run the Lotka Volterra model on populations
    of consumers and resources """ 
    R = pops[0]
    C = pops[1]
    dRdt = r * R - a * R * C 
    dCdt = -z * C + e * a * R * C
    
    return sc.array([dRdt, dCdt])

# sets variables 
#r is intrinsic growth rate of resource population
#a is per-capita "search rate", i.e. the rate at which consumers find resources
#units: area per time unit
#z is the mortality rate - units are 'per time'
#e is the consumer's efficiency (rate in turning mass into its own mass). no units

r = 1.
a = 0.1 
z = 1.5
e = 0.75

R0 = 10
C0 = 5 
RC0 = sc.array([R0, C0])

#Time series 
t = sc.linspace(0, 15, 1000)
# Runs functon 
pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)

f1 = p.figure()
#plots figure 
p.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
p.plot(t, pops[:,1]  , 'b-', label='Consumer density')
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population density')
p.title('Consumer-Resource population dynamics')
p.show()# To display the figure

f1.savefig('../results/LV_model.pdf') #Save figure

f2 = p.figure()

p.plot(pops[:,1], pops[:,0]  , 'r-')
p.grid()
p.legend(loc='best')
p.xlabel('Resource density')
p.ylabel('Consumer density')
p.title('Consumer-Resource population dynamics')
p.show()# To display the figure

f2.savefig('../results/consumer_resource_model.pdf')




