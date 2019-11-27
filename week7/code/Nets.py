""" Extension practical of Python week 7 """

import networkx as nx
import csv 

import csv
 
with open('../data/QMEE_Net_Mat_edges.csv', newline='') as edges:  
    reader = csv.reader(edges)
    for row in reader:
        print(row)

with open('../data/QMEE_Net_Mat_nodes.csv', newline='') as nodes:  
    reader = csv.reader(nodes)
    for row in reader:
        print(row)

pos = nx.circular_layout(Sps)


pos = nx.circular_layout(Sps)
G = nx.Graph()
G.add_nodes_from(Sps)
G.add_edges_from(tuple(AdjL))

NodSizs= 1000 * (Sizs-min(Sizs))/(max(Sizs)-min(Sizs)) 

f3 = p.figure()

nx.draw_networkx(G, pos, node_size = NodSizs)

f3.savefig('../results/network_extra_cred.pdf')