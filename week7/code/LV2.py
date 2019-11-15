import numpy as np
import scipy as sc
import sys
import matplotlib.pylab as p
import scipy.integrate as integrate

def dCR_dt(pops, t=0):

    R = pops[0]
    C = pops[1]
    dRdt = r * R * (1- R * K) - a * R * C 
    dCdt = -z * C + e * a * R * C
    
    return sc.array([dRdt, dCdt])

r = float(sys.argv[1])
a = float(sys.argv[2])
z = float(sys.argv[3])
e = float(sys.argv[4])

K = 100

R0 = 10
C0 = 5 
RC0 = sc.array([R0, C0])


t = sc.linspace(0, 15, 1000)
pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)

f1 = p.figure()

p.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
p.plot(t, pops[:,1]  , 'b-', label='Consumer density')
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population density')
p.title('Consumer-Resource population dynamics')
p.show()# To display the figure

#f1.savefig('../results/LV_model_LV2.pdf') #Save figure

f2 = p.figure()

p.plot(pops[:,1], pops[:,0]  , 'r-', label = ('r=',r, 'a=',a, 'z=',z, 'e=',e))
p.grid()
p.legend(loc='best')
p.xlabel('Resource density')
p.ylabel('Consumer density')
p.title('Consumer-Resource population dynamics')
p.show()# To display the figure

#f2.savefig('../results/consumer_resource_model_LV2.pdf')