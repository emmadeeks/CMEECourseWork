# CMEE 2019 HPC excercises R code HPC run code proforma

rm(list=ls()) # good practice 
source("HPC.R")
# it should take a faction of a second to source your file
# if it takes longer you're using the main file to do actual simulations
# it should be used only for defining functions that will be useful for your cluster run and which will be marked automatically

# do what you like here to test your functions (this won't be marked)
# for example
species_richness(c(1,4,4,5,1,6,1))
# should return 4 when you've written the function correctly for question 1

# you may also like to use this file for playing around and debugging
# but please make sure it's all tidied up by the time it's made its way into the main.R file or other files.

community <- c(10,14,6,7,8)
init_community_max(7)
init_community_min(4)
choose_two(4)
neutral_step(c(10,5,13))
neutral_generation(c(10,5,13))
neutral_time_series(community = init_community_max(7), duration = 20)
question_8()
neutral_step_speciation(c(10,5,13), 0.8)
neutral_generation_speciation(community, 0.2)
neutral_time_series_speciation(community, 0.2, duration = 200)

