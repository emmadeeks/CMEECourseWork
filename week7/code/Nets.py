""" Extension practical of Python week 7 """

import networkx as nx
import csv 
import pandas
import scipy as sc
import matplotlib.pylab as p
import numpy as np

edges = pandas.read_csv('../data/QMEE_Net_Mat_edges.csv', header=0)
nodes = pandas.read_csv('../data/QMEE_Net_Mat_nodes.csv', header=0)

edges.index = ["ICL", "UoR", "CEH", "ZSL", "CEFAS", "NonAc"]

edges = edges.stack().reset_index()
edges = edges[(edges != 0).all(1)]
edges1 = sc.array(edges)
foredge = np.delete(edges1, 2, 1)  

Sps = sc.unique(foredge)

pos = nx.circular_layout(Sps)
G = nx.Graph()
G.add_nodes_from(Sps)
G.add_edges_from(tuple(foredge))
colours = ['lime', 'lime', 'blue', 'red', 'blue', 'lime']
f3 = p.figure()
nx.draw_networkx(G, pos, node_size = 3500, node_color = colours)
# Save file
f3.savefig('../results/Nets.svg')


G = nx.erdos_renyi_graph(20,0.1)
color_map = []
for id in nodes:
    if id == 'University':
        color_map.append('blue')
    else: color_map.append('green')      
nx.draw(G,node_color = color_map, with_labels = True)
plt.show()