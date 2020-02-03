# CMEE 2019 HPC excercises R code HPC run code proforma

rm(list=ls()) # good practice 
graphics.off()
source('HPC.R')



# do what you like here to test your functions (this won't be marked)
# for example
#Question 1
species_richness(c(1,4,4,5,1,6,1))
# should return 4 when you've written the function correctly for question 1

#Question 2
init_community_max(7)

# Question 3
init_community_min(4)

# Question 4
choose_two(4)

# Question 5
neutral_step(c(10,5,13))

# Question 6
neutral_generation(c(10,5,13))

# Question 7 
neutral_time_series(community = init_community_max(7), duration = 20)

# Question 8
question_8()

# Question 9 
neutral_step_speciation(c(10,5,13), 0.8)

# Question 10 
community <- c(1,4,4,5,1,6,1)
neutral_generation_speciation(community, 0.2)

# Question 11
neutral_time_series_speciation(community, 0.2, duration = 200)

#Question 12

question_12()

# Question 13

species_abundance(c(1,5,3,6,5,1,1))

#Question 14

octaves(c(100,64,63,5,4,3,2,2,1,1,1,1))

# Question 15

sum_vect(c(1,3), c(1,0,5,2))

# Question 16

question_16()

#Question 17
cluster_run(speciation_rate = 0.1, size = 100, wall_time = 1, interval_rich = 1, interval_oct = 10, burn_in_generations = 200, output_file_name = "my_test_file_1.rda")

# Question 20 
process_cluster_results()

################### FRACTALS ####################
# Question 21
question_21()

# Question 22

question_22()

# Question 23
chaos_game()

#Question 24
graphics.off()
plot(-50:50,-50:50, type = "n")
turtle(c(2,2), 1, 10)

# Question 25
graphics.off()
plot(-50:50,-50:50, type = "n")
elbow(c(2,2), 1, 10)

# Question 26
graphics.off()
plot(-50:50,-50:50, type = "n")
spiral(c(2,2), 1, 10)



#Question 27
draw_spiral()

#Question 28 
plot(-50:50,-50:50, type = "n")
tree(c(0,0), 1, 10)

draw_tree()

# Question 29
graphics.off()
plot(-50:50,-50:50, type = "n")
fern(c(0,0), 0, 10)

draw_fern()

# Question 30
graphics.off()
plot(-50:50,-50:50, type = "n")
fern2(c(0,0), 0, 10, -1)

draw_fern2()


#Challenge A
Challenge_A()

Challenge_B()

Challenge_C()

Challenge_D()

Challenge_E()

Challenge_F()


