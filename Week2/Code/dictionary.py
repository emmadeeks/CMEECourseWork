taxa = [ ('Myotis lucifugus','Chiroptera'),
         ('Gerbillus henleyi','Rodentia',),
         ('Peromyscus crinitus', 'Rodentia'),
         ('Mus domesticus', 'Rodentia'),
         ('Cleithrionomys rutilus', 'Rodentia'),
         ('Microgale dobsoni', 'Afrosoricida'),
         ('Microgale talazaci', 'Afrosoricida'),
         ('Lyacon pictus', 'Carnivora'),
         ('Arctocephalus gazella', 'Carnivora'),
         ('Canis lupus', 'Carnivora'),
        ]

""" Populates a dictionary to map order names to sets of taxa """

taxa_dic = {} #Empty dictionary 
set_order= set() #empty set 
for w in taxa: 
        set_order.add(w[1]) #Adds orders to set, as sets cant repeat words only four orders 

for y in set_order: 
        set_add = set()
        for i in taxa:
                if i[1]==y: #Goes through taxa list and if orders match output the species into dictionary 
                        set_add.add(i[0])
        taxa_dic[y]=set_add

print("Completed dictionary:", taxa_dic) #Prints completed dictionary 






        






