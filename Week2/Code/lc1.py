birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
         )

""" Uses List comprehensions to create three lists containing
latin names, common names and mean body masses for each species of birds """

#List comprehension for list with latin names 
first_words = {w[0] for w in birds}  #Comprehension that indexes the first line of the list
print(first_words)

#List comprehension for list with common names 
first_words = {w[1] for w in birds}
print(first_words)


#List comprehension for list with body mass 
first_words = {w[2] for w in birds}
print(first_words)


#For loop indexing a list of latin names from a list of birds
first_words = set()
for w in birds: 
    first_words.add(w[0])
print(first_words)

#For loop indexing a list of common names from a list of birds
first_words = set()
for w in birds: 
    first_words.add(w[1])
print(first_words)


#For loop indexing a list of body mass from a list of birds
first_words = set()
for w in birds: 
    first_words.add(w[2])
print(first_words)