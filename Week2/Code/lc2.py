# Average UK Rainfall (mm) for 1910 by month
# http://www.metoffice.gov.uk/climate/uk/datasets
rainfall = (('JAN',111.4),
            ('FEB',126.1),
            ('MAR', 49.9),
            ('APR', 95.3),
            ('MAY', 71.8),
            ('JUN', 70.2),
            ('JUL', 97.1),
            ('AUG',140.2),
            ('SEP', 27.0),
            ('OCT', 89.4),
            ('NOV',128.4),
            ('DEC',142.2),
           )

# (1) Use a list comprehension to create a list of month,rainfall tuples where
# the amount of rain was greater than 100 mm.

""" Uses list comprehensions and for loops to create list of month, rainfall tuples when the
amount of rain was: 
Greater than 100mm 
Less than 50mm """

#List comprehension that returns months of rainfall when it was over or 
# equal to 100mm
x = set([i for i in rainfall if (i[1]) >= 100])
print(x)

# List comprehension that returns months of rainfall when it was under 50mm
x = set([i for i in rainfall if (i[1]) <= 50])
print(x)

# For loop that returns months of rainfall when it was over or 
# equal to 100mm
List_of_rain = []
for w in rainfall:
    if (w[1]) >= 100:
        List_of_rain.append(w)
print(List_of_rain)

# For loop that returns months of rainfall when it was under 50mm
List_of_rain = []
for w in rainfall:
    if (w[1]) <= 50:
        List_of_rain.append(w)
for a in List_of_rain:
    print(a)