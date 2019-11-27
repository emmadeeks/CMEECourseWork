import numpy as np
import scipy as sc
import sys
import matplotlib.pylab as p
import scipy.integrate as integrate

""" Similar to LV1 but requires inputs into the parameters but does use default values """

def dCR_dt(pops, t=0):

    R = pops[0]
    C = pops[1]
    dRdt = r * R * (1- (R / K)) - a * R * C 
    dCdt = -z * C + e * a * R * C
    
    return sc.array([dRdt, dCdt])

if len(sys.argv)== 5:
    r = float(sys.argv[1])
    a = float(sys.argv[2])
    z = float(sys.argv[3])
    e = float(sys.argv[4])
else: 
    print("Using default arguments:")
    r = 1.
    a = 0.1 
    z = 1.5
    e = 0.75

K = 35

R0 = 10
C0 = 5 
RC0 = sc.array([R0, C0])


t = sc.linspace(0, 15, 1000)
pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)

fig, (ax1, ax2) = p.subplots(1,2)

ax1.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
ax1.plot(t, pops[:,1]  , 'b-', label='Consumer density')
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
ax2.plot(pops[:,1], pops[:,0]  , 'r-')
p.legend(loc='best')
p.xlabel('Resource density')
p.ylabel('Consumer density')
p.show()# To display the figure

fig.savefig('../results/consumer_resource_model_LV2.pdf')

