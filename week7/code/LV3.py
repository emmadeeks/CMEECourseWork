import numpy as np
import scipy as sc
import sys
import matplotlib.pylab as p
import scipy.integrate as integrate

""" Similar to LV1 but requires inputs into the parameters but does use default values """
K = 35

#define time vector integrating from time point 0 to 30 using 1000 sub-divisions of time

t = sc.linspace(1, 30, 10)
rows = len(t)
# Setting the initial conditions for the two populations (10 resources and 5 consumers per unit area), and convert the two into an array (because our function takes an array as input)
R0 = 10
C0 = 5
RC0 = sc.array([R0, C0])
RC = np.zeros([rows,2])
RC[:1] = RC0

r = 1.
a = 0.1 
z = 1.5
e = 0.75

for i in range(0, len(t)-1):
    RC[i+1][0] = RC[i][0] * (1 + (r * (1 - RC[i][0] / K)) - a * RC[i][1])
    RC[i+1][1] = RC[i][1] * (1 - z + e * a * RC[i][0])

fig, (ax1, ax2) = p.subplots(1,2)

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

