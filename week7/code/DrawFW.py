#!/usr/bin/env python3

#Author: Emma Deeks ead19@imperial.ac.uk
#Script: DrawFW.py
#Desc:  This script outputs figure of a food web network generated by a random 
# adjacency table and saves it as a pdf. uses packages netwrokx
#Arguments: no input 
#Outputs: network plot in pdf called network.pdf in the data directory
#Date: Oct 2019 

import networkx as nx
import scipy as sc
import matplotlib.pyplot as p

""" This script outputs figure of a food web network and saves it as a pdf """

def GenRdmAdjList(N = 2, C = 0.5):
    """ 
    Generates a random adjacency list to be used for the example network graph
    this adjacency table will be used as the connections for the nodes 
    """
    Ids = range(N)
    ALst = []
    for i in Ids:
        if sc.random.uniform(0,1,1) < C:
            Lnk = sc.random.choice(Ids,2).tolist()
            if Lnk[0] != Lnk[1]: #avoid self (e.g., cannibalistic) loops
                ALst.append(Lnk)
    return ALst

MaxN = 30
C = 0.75

# Generates random adjacency table using function defined 
AdjL = sc.array(GenRdmAdjList(MaxN, C))
AdjL

#select unique numbers which is equivalent to unique species 
Sps = sc.unique(AdjL) # get species ids

SizRan = ([-10,10]) #use log10 scale
Sizs = sc.random.uniform(SizRan[0],SizRan[1],MaxN)
Sizs

# adds the nodes which are individual numbers
pos = nx.circular_layout(Sps) # cicular nodes 
G = nx.Graph() #open graph 
G.add_nodes_from(Sps)
#adds edges using the tuple adjl table which has all of the links in it 
G.add_edges_from(tuple(AdjL))

# calculates the node sizes 
NodSizs= 1000 * (Sizs-min(Sizs))/(max(Sizs)-min(Sizs)) 

f3 = p.figure()
#plots figure
nx.draw_networkx(G, pos, node_size = NodSizs)
#saves as pdf
f3.savefig('../results/network.pdf')





