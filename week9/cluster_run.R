rm(list=ls()) # good practice 
graphics.off()
<<<<<<< HEAD
# Question 1
#Each number in a community is a individual but the different numbers 
# represents different species- species richness outputs the number 
#of different numbers/species
species_richness <- function(community){
  length(unique(community)) #Returns unique numbers in the list of numbers or 'community' 
}


# Question 2
# This gives the maximum number of different specis in a community which
# is specified but the 'size' within the function- gives sequence
init_community_max <- function(size){
  seq(size) # lists the maximal number of different species in a community by listing all numbers until size
}

# Question 3
# effect of initial condition- generates an alternative initial state
# for your simulation of a certain size with the mimum possible number of species
# Gives total number of individuals for given size 
init_community_min <- function(size){
  c(rep(1, size)) # Lowest possible number of possible species- just one for size
}

# Question 4
# Chooses random number according to uniform distribution between i and max vallue
# chooses second random number and returns vector of length
choose_two <- function(max_value){
  sample(1:max_value, 2) #chooses 2 random numbers between 1 and the inputted value
=======
source("HPC.R")
species_richness <- function(community){
  length(unique(community))
}

init_community_min <- function(size){
  c(rep(1, size))
}

choose_two <- function(max_value){
  sample(1:max_value, 2)
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
}

# Question 5
# performs a single step of a simple neutral model simulation WITHOUT SPECIATION
# on a commmunity vector. picks idividual to die and replaces with reproduction
# Are not same individual but could be of the same species. returns community states with equal probability
<<<<<<< HEAD
neutral_step <- function(community){ 
  first <- choose_two(length(community)) # Uses choose_two function to choose random numbers from normal distribution
  community[first[1]] <- community[first[2]] #replaces dead invidivual with new individual
=======
neutral_step <- function(community){
  first <- choose_two(length(community))
  community[first[1]] <- community[first[2]]
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
  return(community)
}


# Question 6
# simulates several neutral_steps on a community so generation has passed
# A generation is however many indivduals there are divided by two (community/2)
# makes x into a even number randomly 
# Generation is the amount of time expected between bird and reproduction
# (not the time bwteen birth and death)
# e.g. 10 indivudals = 5 neutral steps correspond to 5 births and 5 deaths 
# Funcion outputs vector giving community state after a generation has passed
neutral_generation <- function(community){
<<<<<<< HEAD
  steps <- round(jitter(length(community)/2, amount = 0.1)) #sets a jitter round the length of communty/2 by 0.1
  for (i in 1:steps){ #compute neutral_step function on community with jittered rounding
    community <- neutral_step(community) #going through community and replacing the old community as you loop through the steps
=======
  steps <- round(jitter(length(community)/2, amount = 0.1))
  for (i in 1:steps){
    community <- neutral_step(community)
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
  }
  return(community)
}


# Question 7
# rns the neutral theory simulation and return time series of species richness
# inputs community and number of generations 
# gives species richness at each generation of simulation
neutral_time_series <- function(community, duration){
<<<<<<< HEAD
  richness <- c() # empty vector to store richness in 
  for (i in 1:duration){ # number of generations
    community <- neutral_generation(community) # computes the neutral generation of that community
    p <- species_richness(community) #then the richness
    richness <- c(richness, p) #inputs richness into vector as it runs iteraively rhought the duration 
  }
  return(richness) # returns time series of species richness
=======
  richness <- c()
  for (i in 1:duration){
    community <- neutral_generation(community)
    p <- species_richness(community)
    richness <- c(richness, p)
  }
  return(richness)
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
}


# Question 8
# Plots time series of neutral time series from initial conidition of maximum diversity
# not inputs 
question_8 <- function(){
<<<<<<< HEAD
  x <- c() #initialised vectors 
  duration = 200 #defines the generation time 
  richness <- neutral_time_series(init_community_max(100), duration) #runs neutral time series for maximal diversity
  x <- 1:duration # sets the x axis to the generation length 
  plot(x,richness) #plots the generations against the richness 
=======
  x <- c()
  y <- c()
  duration = 200
  richness <- neutral_time_series(init_community_max(100), duration)
  for (i in 1:duration){
    x <- c(x, i)
    y <- c(y, richness[i])
  }
  plot(x,y)
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
  return("type your written answer here")
}

# Question 9
# performs a step of a neutral model but WITH SPECIATION 
# speciation replaces dead indidivual with new species witht he probably of the given speciation rate 
# if probability not met the dead indiivual is replaced with the offspring of another inidividual like neutral step
# speciation rate is an input parameter 
<<<<<<< HEAD
neutral_step_speciation <- function(community,speciation_rate){ 
  newspecies <- runif(1, min = 0, max = 1) #selects 'new species between 1 and 0 to use
  if(newspecies > speciation_rate){ # if that new species is greater than the speciation rate
    community <- neutral_step(community) #a neutal step is done on exsiting community and no species introduced
  } else { # if not the new species is introduced into population
    choices <- choose_two(length(community))
    community[choices[1]] <- max(community)+1 # another species is added to community 
  }
  return(community) #returns commmunity 
}

# Question 10
# uses a simulation with speciation but acts as nuetral_generation with the gitter aspect
# finds the number of steps by dividing the length of community by two and rounding either way by 0.1 
neutral_generation_speciation <- function(community,speciation_rate)  {
  steps <- round(jitter(length(community)/2, amount = 0.1))
  for (i in 1:steps){
    community <- neutral_step_speciation(community, speciation_rate) # applies new function which also includes speciation
  }
  return(community) 
=======
neutral_step_speciation <- function(community,speciation_rate){
  newspecies <- runif(1, min = 0, max = 1)
  if(newspecies > speciation_rate){
    community <- neutral_step(community)
  } else {
    choices <- choose_two(length(community))
    community[choices[1]] <- max(community)+1
  }
  return(community)
}

# Question 10
neutral_generation_speciation <- function(community,speciation_rate)  {
  steps <- round(jitter(length(community)/2, amount = 0.1))
  for (i in 1:steps){
    community <- neutral_step_speciation(community, speciation_rate)
  }
  return(community)
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
}


# Question 11
<<<<<<< HEAD
# important to notes at the generations go through the newly generation community 
# produced from neutral_generation_speciaton is then fed back into the function 
neutral_time_series_speciation <- function(community,speciation_rate,duration){
  richness <- species_richness(community) #calculates the species richness of the community
  for (i in 1:as.integer(duration)){ # for the length of the generations 
    community <- neutral_generation_speciation(community, speciation_rate) #calculate the community according to speciation rate
    p <- species_richness(community) #calculate the species richness of these updates communities 
    richness <- c(richness, p) # new richness is appended to a vector of species richnesses throughout the generations 
=======
neutral_time_series_speciation <- function(community,speciation_rate,duration){
  richness <- c()
  for (i in 1:as.integer(duration)){
    community <- neutral_generation_speciation(community, speciation_rate)
    p <- species_richness(community)
    richness <- c(richness, p)
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
  }
  return(richness)
}


# Question 12
<<<<<<< HEAD
# This function performs a newutral theory simulation with speciation and plots the species richness
# against time, two time series are plotted, one times series with the maximum diversity at initial state 
# Another time series with the mininmal diversity at inital state
question_12 <- function(){
  graphics.off()
  x <- c() #initalise vectors 
  duration = 200 #sets generations as 200
  speciation_rate = 0.1
  richness <- neutral_time_series_speciation(init_community_max(100), speciation_rate, duration) #performs neutral theory simulation on maximal diversity
  richness2 <- neutral_time_series_speciation(init_community_min(100), speciation_rate, duration) #Performs neutral theory simulations on minimal diversity
  x = 1:201 #set x axis as generations plus 1 to see end of plot clearly 
  plot(y = richness, x = x, ylim = c(0,100), col = 'red', type = 'l') #plot richness against generations
  lines(richness2, col = 'blue') # add the second line with is richness at minimal diversity 
=======

question_12 <- function(){
  graphics.off()
  x <- c()
  y <- c()
  y2 <- c()
  duration = 200
  speciation_rate = 0.1
  richness <- neutral_time_series_speciation(init_community_max(100), speciation_rate, duration)
  richness2 <- neutral_time_series_speciation(init_community_min(100), speciation_rate, duration)
  x <- c(duration)
  y <- c(richness)
  y2 <- c(richness2)
  plot(y, col = 'red', type = 'l')
  lines(y2, col = 'blue')
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
  return("type your written answer here")
}


# Question 13
<<<<<<< HEAD
# This calculates species abundance by calculating how requently each number or species in the 
#inputted community appears 
species_abundance <- function(community)  {
  w = table(community) # puts community into a frequency table of each number in community
  sort(w, decreasing = TRUE) #sorts the table from highest to lowest
}

# Question 14
# This function bins the abundances of species into octave classes 
# first value of vector tells you how many species have an abundance of only 1, second how many species 
# have an abundance of 2 or 3  and the nth valye tells you how many specie shave 
# an abundance greater than or equal to 2(n-1)
octaves <- function(abundance_vector) { #abundance vector returned by species_abundance
  tabulate(floor(log2(abundance_vector))+1) 
}

# Question 15
# accepts two vectors and returns their sum after filling shorter vector with zeros 
# to bring it up to the correct ength 
sum_vect <- function(x, y) {
  diff <- length(x)-length(y) #finds the difference in length of the two vectors 
  if (diff > 0) { # if the difference is greater than 0, fill y with zeros to rbing up to length of difference
    y <- c(y, rep(0, abs(diff))) }
  if (diff < 0) { # if difference is less than 0 fill x vector with zeros to bring up to difference
    x <- c(x, rep(0, abs(diff))) }
  vector_sum <- x + y # than add the two vectors and return the summed vector
=======
species_abundance <- function(community)  {
  w = table(community)
  sort(w, decreasing = TRUE)
}

# Question 14
octaves <- function(abundance_vector) {
  tabulate(floor(log2(abundance_vector))+1)
}

# Question 15
sum_vect <- function(x, y) {
  diff <- length(x)-length(y)
  if (diff > 0) {
    y <- c(y, rep(0, abs(diff))) }
  if (diff < 0) {
    x <- c(x, rep(0, abs(diff))) }
  vector_sum <- x + y
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
  return(vector_sum)
}

# Question 16 
<<<<<<< HEAD
# runs a neutral model simualtion using the same parameters as question 12 for a burn in period 
# of 200 generations and records species abundance in a octave vector 
# continue simulation from where it left off for 2000 generations and record species abundance 
# octaves every 20 generations. barplot shows average species abundance octave vector 
question_16 <- function(){
  graphics.off() #graphics off 
  richnessmax <- c() #initialise vectors 
  richnessmin <- c()
  octave_to_max <- c()
  octave_to_min <- c()
  duration = 200 #burn in period of generations
  generation = 2000 # generations after burn in period 
  speciation_rate = 0.1
  richnessmax <- init_community_max(100) # inital community at max diversity 
  richnessmin <- init_community_min(100) # initial community at min diversity 
  for (i in 1:as.integer(duration)) { #length of burn in period
    richnessmax <- neutral_generation_speciation(richnessmax, speciation_rate) #find the species richness for burn in generations
    richnessmin <- neutral_generation_speciation(richnessmin, speciation_rate)
  } #note each newly produced species richness is fed back into the neutral_generation_speciation function
  counter <- 0  #sets counterr as zero so the number of times the for loop goes through generations is the numbers that 
  for (i in 1:as.integer(generation)) { # octaves are divided by to get the average 
    richnessmax <- neutral_generation_speciation(richnessmax, speciation_rate) #Takes the richness after burn in period 
    richnessmin <- neutral_generation_speciation(richnessmin, speciation_rate) #inputs into the for loop for generations after burn in period
    if (i %% 20==0){ # if i in generations divides by 20 with zero remainders take the octaves
      counter<- counter + 1 #add one to counter to divide octaves by
      octave_max <- octaves(species_abundance(richnessmax)) #calculate octaves 
      octave_min <- octaves(species_abundance(richnessmin))
      octave_to_max <- sum_vect(octave_to_max, octave_max) #add any new octaves onto the exsiting octaves using sum_vect
      octave_to_min <- sum_vect(octave_to_min, octave_min)
    }
  }
  averagemax <- octave_to_max / counter #divide octaves by the counter to get the avergae 
  averagemin <- octave_to_min / counter
  par(mfrow=c(1,2)) #set up two plots
  barplot(averagemax)
  barplot(averagemin) #plot the average 
  return("does the initial condition of the system matter? why is this?")
=======
question_16 <- function(){
  graphics.off()
  richnessmax <- c()
  richnessmin <- c()
  octave_to_max <- c()
  octave_to_min <- c()
  duration = 200
  generation = 2000
  speciation_rate = 0.1
  richnessmax <- init_community_max(100)
  richnessmin <- init_community_min(100)
  for (i in 1:as.integer(duration)) {
    richnessmax <- neutral_generation_speciation(richnessmax, speciation_rate)
    richnessmin <- neutral_generation_speciation(richnessmin, speciation_rate)
  }
  octave_max <- octaves(species_abundance(richnessmax))
  octave_min <- octaves(species_abundance(richnessmin))
  counter <- 0 
  for (i in 1:as.integer(generation)) {
    richnessmax <- neutral_generation_speciation(richnessmax, speciation_rate)
    richnessmin <- neutral_generation_speciation(richnessmin, speciation_rate)
    if (i %% 20==0){
      counter<- counter + 1
      octave_max <- octaves(species_abundance(richnessmax))
      octave_min <- octaves(species_abundance(richnessmin))
      octave_to_max <- sum_vect(octave_to_max, octave_max)
      octave_to_min <- sum_vect(octave_to_min, octave_min)
    }
  }
  averagemax <- octave_to_max / counter
  averagemin <- octave_to_min / counter
  par(mfrow=c(1,2))
  barplot(averagemax)
  barplot(averagemin)
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
}



# Question 17
<<<<<<< HEAD
# this is similar to question 16 but starts a simulation off with only minimal community size 
# and has many more input parameters. this functions applies the neutral generation speciation function 
# for a predefined amount of walltime. then store species richness as intervals of interval_rich
# but only during the burn in period. then after burn in period stop recording species richness 
# for whole simualtion record species richness as octaves each interval_oct 
# save simulation in file with various parameters names. 
cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct,
                        burn_in_generations, output_file_name)  {
  community <- init_community_min(size) # initialise community with minimum diversity 
  start_time <- (proc.time()[3]) #initialise start time
  counter <- 0 # initialise counter 
  octave_data <- list() # make a list of octave data 
  count_octave <- 0 # intialise second counter for recording octave dara 
  richness <- c() # intiialise vector 
  run_time <- (proc.time()[3] - start_time)/60 # run time has to be in minutes and is the current time
  while (run_time < wall_time) { #while this time is less than wall_time do this:
    community <- neutral_generation_speciation(community, speciation_rate) # run neutral generation speciation on newly generated communities
    counter <- counter + 1 # for each iteration add 1 to counter
    run_time <- (proc.time()[3] - start_time)/60 # update run time as your go through generations 
    if (counter %% interval_rich == 0 && counter <= burn_in_generations) { #if counter is directly divisble by interval_rich and less than of equal to burn in generations 
      richness <- c(richness, species_richness(community)) #calculate and append species richness to vecctor
    }
    if (counter %% interval_oct == 0) { # if counter is directly divisible by interval_oct
      count_octave <- count_octave + 1 # add to octave counter 
      octave1 <- octaves(species_abundance(community)) # put the current community into octaves
      octave_data[[count_octave]] <- (octave1) #append new octaves to the octave_data list
    }
  }
  time_end <- (proc.time()[3] - start_time) # calculate ending time 
  save(richness, octave_data, community, time_end, speciation_rate,
       size, wall_time, interval_rich, interval_oct, burn_in_generations, file = output_file_name)
} #save all the elements to a file 

################################ QUESTION 18 #####################

iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX")) #Sets iter 
set.seed(iter) #sets iter as the seed 

# function that picks each size 
whichsize <- function(iter) {
  if (iter %% 4 == 0){ # if iter is divisble by 4 exactly then set size to 500
    size = 500
  }
  if (iter %% 4 == 1){ # if iter has remainder one than set size to 1000
=======
cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct,
                        burn_in_generations, output_file_name)  {
  community <- init_community_min(size)
  start_time <- (proc.time()[3])
  counter <- 0
  octave_data <- list()
  count_octave <- 0
  richness <- c()
  run_time <- (proc.time()[3] - start_time)/60
  while (run_time < wall_time) {
    community <- neutral_generation_speciation(community, speciation_rate)
    counter <- counter + 1
    run_time <- (proc.time()[3] - start_time)/60
    if (counter %% interval_rich == 0 && counter <= burn_in_generations) {
      richness <- c(richness, species_richness(community))
    }
    if (counter %% interval_oct == 0) {
      count_octave <- count_octave + 1
      octave1 <- octaves(species_abundance(community))
      octave_data[[count_octave]] <- (octave1)
    }
  }
  time_end <- (proc.time()[3] - start_time)
  save(richness, octave_data, community, time_end, speciation_rate,
       size, wall_time, interval_rich, interval_oct, burn_in_generations, file = output_file_name)
}


iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
set.seed(iter)

whichsize <- function(iter) {
  if (iter %% 4 == 0){
    size = 500
  }
  if (iter %% 4 == 1){
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
    size = 1000
  }
  if (iter %% 4 == 2){
    size = 2500
  }
  if (iter %% 4 == 3){
    size = 5000
  }
  return(size)
}


<<<<<<< HEAD
newiter <- whichsize(iter) # calculate which size to use as input to cluster run 
myfilename <- paste("EDIteration:", iter, ".rda", sep = "") #set the filename with iteration number in it 
octpar = newiter/10 # set the interval oct time 
burngen = 8*newiter # set the burn in generations 

# run the cluster run 
cluster_run(speciation_rate = 0.003517, size = newiter, wall_time = 690, interval_rich = 1, interval_oct = octpar, burn_in_generations = burngen, output_file_name = myfilename)
=======
newiter <- whichsize(iter)
myfilename <- paste("EDIteration:", iter, ".rda", sep = "")

cluster_run(speciation_rate = 0.003517, size = newiter, wall_time = 10, interval_rich = 1, interval_oct = 10, burn_in_generations = 200, output_file_name = myfilename)
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7




