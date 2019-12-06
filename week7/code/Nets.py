#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: Nets.py
#Desc:  Extension practical of Python week 7  which uses networkx to visualise the QMEE 
#CDT collaboration network, coloruing the nodes by the type of node 
#(organisation type: University, Histing partner or non-histing partner)
#Arguments: Takes as input the QMEE_Net_Mat_edges.csv and QMEE_Net_Mat_nodes.csv from data directory
#Outputs: network plot in pdf called nets.svg in the results directory
#Date: Oct 2019 

""" Extension practical of Python week 7  which uses networkx to visualise the QMEE 
CDT collaboration network, coloruing the nodes by the type of node 
(organisation type: University, Histing partner or non-histing partner) """


import networkx as nx ### Import package for network 
import csv 
import pandas
import scipy as sc
import matplotlib.pylab as p ## Import 
import numpy as np

#reads in csv fies and saves them to a variable 
#uses pandas 
edges = pandas.read_csv('../data/QMEE_Net_Mat_edges.csv', header=0)
nodes = pandas.read_csv('../data/QMEE_Net_Mat_nodes.csv', header=0)

#Changes the row numbers to the university so the edges are easier to match and the nodes when plotted are the same
edges.index = ["ICL", "UoR", "CEH", "ZSL", "CEFAS", "NonAc"]
#Stack the edges continuency table so that all data is side by side like in DrawFW AdjL 
edges = edges.stack().reset_index()
edges = edges[(edges != 0).all(1)] #Remove any rows with zero in them as that is not a node connection
edges1 = sc.array(edges) #make into an array with just the two columns showing the connections
foredge = np.delete(edges1, 2, 1) # delete the third row as that is not needed 

Sps = sc.unique(foredge) #find unique universities to get the nodes 

pos = nx.circular_layout(Sps) #plots nodes as circular 
G = nx.Graph() #open graph 
G.add_nodes_from(Sps) #add the nodes in which are the unique universiies 
G.add_edges_from(tuple(foredge)) #add the edges or connecting parts of the nodes 
colours = ['lime', 'lime', 'blue', 'red', 'blue', 'lime'] #make vector with colour for nodes 
f3 = p.figure()
nx.draw_networkx(G, pos, node_size = 3500, node_color = colours) # draw network graph 
# Save file
f3.savefig('../results/Nets.svg') # save the figure 

## SOME ROUGH WORK I DIDNT GET TO WORK 
#G = nx.erdos_renyi_graph(20,0.1)
#color_map = []
#for id in nodes:
#    if id == 'University':
#        color_map.append('blue')
#    else: color_map.append('green')      
#nx.draw(G,node_color = color_map, with_labels = True)
#plt.show()