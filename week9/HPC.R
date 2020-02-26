# CMEE 2019 HPC excercises R code main proforma
# you don't HAVE to use this but it will be very helpful.  If you opt to write everything yourself from scratch please ensure you use EXACTLY the same function and parameter names and beware that you may loose marks if it doesn't work properly because of not using the proforma.

name <- "Emma Deeks"
preferred_name <- "Emma"
email <- "ead19@imperial.ac.uk"
username <- "edeeks"
personal_speciation_rate <- 0.003517 # will be assigned to each person individually in class and should be between 0.002 and 0.007

# Question 1
#Each number in a community is a individual but the different numbers 
# represents different species- species richness outputs the number 
#of different numbers/species
species_richness <- function(community){
<<<<<<< HEAD
  length(unique(community)) #Returns unique numbers in the list of numbers or 'community' 
=======
  length(unique(community))
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
}


# Question 2
# This gives the maximum number of different specis in a community which
# is specified but the 'size' within the function- gives sequence
init_community_max <- function(size){
<<<<<<< HEAD
  seq(size) # lists the maximal number of different species in a community by listing all numbers until size
=======
  seq(size)
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
}

# Question 3
# effect of initial condition- generates an alternative initial state
# for your simulation of a certain size with the mimum possible number of species
# Gives total number of individuals for given size 
init_community_min <- function(size){
<<<<<<< HEAD
  c(rep(1, size)) # Lowest possible number of possible species- just one for size
=======
  c(rep(1, size))
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
}

# Question 4
# Chooses random number according to uniform distribution between i and max vallue
# chooses second random number and returns vector of length
choose_two <- function(max_value){
<<<<<<< HEAD
  sample(1:max_value, 2) #chooses 2 random numbers between 1 and the inputted value
=======
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
  graphics.off()
  x <- c() #initialised vectors 
  duration = 200 #defines the generation time 
  richness <- neutral_time_series(init_community_max(100), duration) #runs neutral time series for maximal diversity
  x <- 1:duration # sets the x axis to the generation length 
  plot(x,richness, xlab = "Generations", ylab = "Species richness", cex.main = 0.9, main = "Time series of a neutral model simulation", cex = 1, pch = 20, cex.axis = 0.95, col = "dark red") #plots the generations against the richness 
  return("After approximately 70 generations of the neutral_time_series simulation the species richness decreases to 1 and remains. In the neutral_generation function the inputted community is halved each step and so during the neutral_time_series this inputted community is repeatededly halved until it reaches one, there are also no new inputs into the system, e.g. speciation.")
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
  return("type your written answer here")
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
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
  plot(y = richness, x = x, ylim = c(0,100), main = "Neutral theory simulation with speciation timeseries", cex.main = 0.9, ylab = "Species richness", xlab = "Generations", lwd = 2, cex.axis = 0.8, col = 'red', type = 'l') #plot richness against generations
  legend("topright",
         legend = c("Minimum initial state of diversity", "Maximum initial state of diversity"),
         col = c("blue", "red"),
         pch = c(20,20),
         bty = "n",
         pt.cex = 1,
         cex = 0.75,
         text.col = "black",
         horiz = F)
  lines(richness2, col = 'blue', lwd = 2) # add the second line with is richness at minimal diversity 
  return("Despite the two initial conditions being maximal diversity and minimal diversity both plots reached the same species richness after a relatively short amount of generations. This is likely because selection pressures and competition within that environment will drive populations to the same level of richness despite the starting conditions as the carrying capacity of that system is not affected by intial communities")
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
  return("type your written answer here")
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
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
  barplot(averagemax, xlab = "Octaves", ylab = "Species abundance in octave", names.arg = 1:length(averagemax), cex.lab = 1.2, cex.axis = 1, col = "dark red")
  title(main = "Maximum initial community", line=0.5, cex.main=0.9)
  barplot(averagemin, xlab = "Octaves", ylab = "Species abundance in octave", names.arg = 1:length(averagemin), cex.lab = 1.2, cex.axis = 1, col = "dark green")
  title(main = "Minimum initial community", line=0.5, cex.main=0.9)
  mtext("Species abundance octaves through 2000 generations with 2 different initial starting communities", outer=TRUE,  cex=1.5, line=-2)#plot the average 
  return("Like in question 12, the initial condition of the system does not appear to matter, after a certain amount of generations, or, the burn in generations, species richness will be equal irrelvant of the initial community ")
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

# Questions 18 and 19 involve writing code elsewhere to run your simulations on the cluster

# Question 20 
# This function processes the output files from the cluster run on the cluster of the 100 iterations 
# reads in the output files and plots four multipanel graphs for each simulation size 
# of the mean species abundance octave only using data after the burn in period 
# also outputs a list of four vectors corresponding to the octave outputs that plot te four bar graphs- 
#the vectors  appear in the list in increasing community order 

process_cluster_results <- function()  {
  graphics.off()
  counter500 <- 0 # set counters to calculate averages 
  counter1000 <- 0
  counter2500 <- 0
  counter5000 <- 0
  newvect500 <- c() # set empty vectors 
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

# Questions 18 and 19 involve writing code elsewhere to run your simulations on the cluster



# Question 20 
process_cluster_results <- function()  {
  graphics.off()
  counter500 <- 0
  counter1000 <- 0
  counter2500 <- 0
  counter5000 <- 0
  newvect500 <- c()
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
  newvect1000 <- c()
  newvect2500 <- c()
  newvect5000 <- c()
  average500 <- c()
  average1000 <- c()
  average2500 <- c()
  average5000 <- c()
<<<<<<< HEAD
  combined_results <- list() # set the list of the vectors of octaves 
  for (i in 1:100){ # the amount of iterations 
    load(file = gsub(" ", "", paste("EDIteration:", i, ".rda"))) # load any file starting with ED and with number of iteration in 
    toremove <- burn_in_generations/interval_oct #remove the burn in generations from this list 
    datatouse <- octave_data[-c(1:toremove)] # set data to use as data with no burn in generations 
    sumocataves <- c()
    for (p in c(1:length(datatouse))){
      sumocataves <- sum_vect(sumocataves, datatouse[[p]])
    }
    # go through all the data in iterations and sum them 
    if (size == 500){
      counter500 <- counter500 + length(datatouse) # if the size if 500 then add to the counter the length of the sum
      newvect500 <- sum_vect(newvect500, sumocataves) # add the sum octaves to the new vector 
    }
    if (size == 1000){
      counter1000 <- counter1000 + length(datatouse)
      newvect1000 <- sum_vect(newvect1000, sumocataves)
    }
    if (size == 2500){
      counter2500 <- counter2500 + length(datatouse)
      newvect2500 <- sum_vect(newvect2500, sumocataves)
    }
    if (size == 5000){
      counter5000 <- counter5000 + length(datatouse)
      newvect5000 <- sum_vect(newvect5000, sumocataves)
    }
  }
  average500 <- newvect500 / counter500 #average by dividing the the new vector of octaves for that size and divide by the counter
  average1000 <- newvect1000 / counter1000
  average2500 <- newvect2500 / counter2500
  average5000 <- newvect5000 / counter5000
  par(mfrow=c(2,2))
  barplot(average500, xlab = "Species abundance octaves", ylab = "Average species abundance", names.arg = 1:length(average500), cex.lab = 1.1, cex.axis = 1, col = "dark red") # plot the avergaed octaves 
  title(main = "Initial community size: 500", line=0.5, cex.main=0.9)
  barplot(average1000, xlab = "Species abundance octaves", ylab = "Average species abundance", names.arg = 1:length(average1000), cex.lab = 1.1, cex.axis = 1, col = "dark green")
  title(main = "Initial community size: 1000", line=0.5, cex.main=0.9)
  barplot(average2500, xlab = "Species abundance octaves", ylab = "Average species abundance", names.arg = 1:length(average2500), cex.lab = 1.1, cex.axis = 1, col = "blue")
  title(main = "Initial community size: 2500", line=0.5, cex.main=0.9)
  barplot(average5000, xlab = "Species abundance octaves", ylab = "Average species abundance", names.arg = 1:length(average5000), cex.lab = 1.1, cex.axis = 1, col = "yellow")
  title(main = "Initial community size: 5000", line=0.5, cex.main=0.9)
  mtext("Species abundance octaves in 4 different initial community sizes", outer=TRUE,  cex=1, line=-1.5)
  combined_results <- list(average500, average1000, average2500, average5000) #create your list output here to return
  return(combined_results) #return the list 
}


########### FRACTALS ############

# Question 21
# This function returns the calculated fractal dimension of an object and a string explaining the workings. 
=======
  combined_results <- list()
  for (i in 1:8){
    load(file = gsub(" ", "", paste("Thursday_test_2_", i, ".rda")))
    toremove <- burn_in_generations/interval_oct
    datatouse <- head(octave_data, -toremove)
    sumocataves <- c()
    for (p in length(datatouse)){
      sumocataves <- sum_vect(sumocataves, datatouse[[p]])
    }
    if (size == 500){
        counter500 <- counter500 + length(sumocataves)
        newvect500 <- sum_vect(newvect500, sumocataves)
    }
    if (size == 1000){
        counter1000 <- counter500 + length(sumocataves)
        newvect1000 <- sum_vect(newvect1000, sumocataves)
    }
    if (size == 2500){
        counter2500 <- counter2500 + length(sumocataves)
        newvect2500 <- sum_vect(newvect2500, sumocataves)
    }
      if (size == 5000){
        counter5000 <- counter5000 + length(sumocataves)
        newvect5000 <- sum_vect(newvect5000, sumocataves)
      }
    }
    average500 <- newvect500 / counter500
    average1000 <- newvect1000 / counter1000
    average2500 <- newvect2500 / counter2500
    average5000 <- newvect5000 / counter5000
    par(mfrow=c(2,2))
    barplot(average500)
    barplot(average1000)
    barplot(average2500)
    barplot(average5000)
    # clear any existing graphs and plot your graph within the R window
    combined_results <- list(average500, average1000, average2500, average5000) #create your list output here to return
    return(combined_results)
  }

########### FRACTALS ############
# Question 21
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
question_21 <- function()  {
  a <- list('1.89', "Using the Koch Curve equation, I set the the Width of the square at 3, because it is three 'squares' long
            and the size as 8 as there are 8 squares of the subunit. I then divided log of size (8) by the log of width (3) to get 1.89")
  return(a)
}

# Question 22
<<<<<<< HEAD
# This question outputs the fractal dimension of a object and a string explaining how I worked it out 
=======
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
question_22 <- function()  {
  a <- list("2.73", "Using the box method for working out the dimension, the number 
            of hypercubes was set as 20 and the hypercube length set as a 1/3 as that was 
            the fraction where the 'hole' in the centre of the cube was excluded. the equation was log(20)/log(1/3) and log(1)")
  return(a)
}

# Question 23
<<<<<<< HEAD
# Function that plots a fractal triangle 
chaos_game <- function()  {
  graphics.off()
  A = c(0,0) # storing the three coordinates of a graph to a variable
  B = c(3,4)
  C = c(4,1)
  X = c(0,0) # setting X as coordinate A
  points <- list(A, B, C) #making a list of the coordinates 
  plot(x = X[1], y = X[2], ylim = c(-1, 5), xlim = c(-0.5, 4), cex = 0.5, xlab = "X Coordinates", ylab = "Y Coordinates", cex.main = 1, main = "Fractal Triangle", pch = 20, cex.axis = 0.8) #Plotting X coordinates onto a graph
  for (i in 1:10000){ #runs through a loop 10000 times 
    new <- sample(points, 1) #chooses one of the three points at random
    newnum <- c(as.numeric(new[[1]][1]), as.numeric(new[[1]][2])) # changes the coordinates to two numeric numbers and out of a list
    Xnew <- ((newnum - X)/2) # calculare the difference between the coordinate and X then divide by two to get half way
    X <- Xnew + X #add this half value to X so you are then moving X half way towards the points
    points(x = X[1], y = X[2], cex = 0.5) #add points 
  }
  return("This makes a fractal triangle, the more iterations you run through the more points are plotted, points are overlaid ontop of the old points so the triangle appears more filled out")
}



# Question 24
# Function that draws a line from a given point, defined as a vector, and in a given directio
turtle <- function(start_position, direction, length, colour= "black"){ #default colour of line is given but can be changed
  newx <- cos(direction) * length  #calculate x value by multiplying cos(direction) by length
  newy <- sin(direction) * length # calculate y value by multiplying sin(direction) by length 
  newpoints <- c(newx,newy) #put into a vector of new points
  newpositions <- c(newpoints + start_position) # create vector of new points added to the startposition 
  segments(start_position[1], start_position[2], newpositions[1], newpositions[2], col = colour, lwd = 1) #plot the start posiiton and then the new poisitions using segments to create a line 
=======
chaos_game <- function()  {
  A = c(0,0)
  B = c(3,4)
  C = c(4,1)
  X = c(0,0)
  points <- list(A, B, C)
  plot(x = X[1], y = X[2], cex = 0.5)
  for (i in 1:10000){
    new <- sample(points, 1)
    newnum <- c(as.numeric(new[[1]][1]), as.numeric(new[[1]][2]))
    Xnew <- ((newnum - X)/2)
    X <- Xnew + X
    points(x = X[1], y = X[2], cex = 0.5)
  }
  return("This makes a fractal triangle")
}

# Question 24

turtle <- function(start_position, direction, length){
  newx <- cos(direction) * length 
  newy <- sin(direction) * length
  newpoints <- c(newx,newy)
  newpositions <- c(newpoints + start_position)
  segments(start_position[1], start_position[2], newpositions[1], newpositions[2])
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
  return(newpositions) # you should return your endpoint here.
}

# Question 25
<<<<<<< HEAD
# Function that calls turtle function twice and plots two lines that join together with an angle between them 
elbow <- function(start_position, direction, length, colour = "black"){
  first <- turtle(start_position, direction, length, colour) #calls turtle function once and saves output to variable
  turtle(first, (direction - pi/4), length*0.95, colour) #uses the starting positions given by turtle and puts them into turtle again so line is 
} #is drawn from the end positions of the first turtle function

# Question 26
# function that plots a spiral/spider web by calling the turtle function on itself again
# and again until a limit is reached, lines plotted each time get smaller each time and are plotted at certain angles to eachother 
spiral <- function(start_position, direction, length, colour = "black"){
  first <- turtle(start_position, direction, length, colour) #first call turtle function
  limit = 0.1 #sets limit so there are no errors 
  if (length > limit) {
  spiral(first, (direction - pi/4), length*0.95, colour) #plots lines at at pi/4 or 45o angle between them
  }
  return("A spiral is plotted, by calling the turtle function on itself again and again until a limit is reached, lines plotted each time get smaller each time and are plotted at certain angles to eachother, this angle is what creates the web like shape")
}

# Question 27
# Function that opens a new plot and calls th spiral function, takes as input the spiral inputs 
draw_spiral <- function(){
  graphics.off()
  plot(-10:30,-20:20, type = "n", cex.main = 1, main = "Spiral function output", xlab = "",ylab = "", axes = 0) # initalise empty new plot
  spiral(c(0,0), 1, 10) #call spiral function 
=======
elbow <- function(start_position, direction, length){
  first <- turtle(start_position, direction, length)
  turtle(first, (direction - pi/4), length*0.95)
}

# Question 26
spiral <- function(start_position, direction, length, limit){
  first <- turtle(start_position, direction, length)
  if (length > limit) {
  spiral(first, (direction - pi/4), length*0.95, limit)
  }
  return("type your written answer here")
}

# Question 27
draw_spiral <- function(start_position, direction, length, limit = 0.1)  {
  first <- turtle(start_position, direction, length)
  if (length > limit) {
  spiral(first, (direction - pi/4), length*0.95, limit)
  }
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
}



# Question 28
<<<<<<< HEAD
# function that, instead of calling itself once like spiral, calls its self twice
# plots a tree in a pre-exsisting plot
tree <- function(start_position, direction, length, colour = "black"){
  first <- turtle(start_position, direction, length, colour) # calls the turtle function and saves to a variable
  limit = 0.1
  if (length > limit) { #having a limit stops function crashing 
    tree(first, (direction - pi/4), length*0.65, colour) # calls function twice is different directions 
    tree(first, (direction + pi/4), length*0.65, colour)
=======
tree <- function(start_position, direction, length, limit = 0.1){
  first <- turtle(start_position, direction, length)
  if (length > limit) {
    tree(first, (direction - pi/4), length*0.65, limit)
    tree(first, (direction + pi/4), length*0.65, limit)
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
  }
  return("type your written answer here")
}

<<<<<<< HEAD
# Function that calls the tree function and initalises an empty plot 
draw_tree <- function()  {
  graphics.off()
  plot(0:30,0:30, type = "n", xlab = "",ylab = "", axes = FALSE, cex.main = 1, main = "Tree function output") # initalise empty new plot
  tree(c(5,15), 0, 8) #call tree function 
}


# Question 29
# Function that draws a fern- similar to tree function but one branch goes 
# 45 degrees to the left and the other branch goes stright. the length of the 
# branches are also different from tree 
fern <- function(start_position, direction, length, colour = "dark green"){
  first <- turtle(start_position, direction, length, colour)
  limit = 0.1
  if (length > limit) {
    fern(first, (direction - pi/4), length*0.38, colour) #this branch is going 45 degrees to the left 
    fern(first, direction, length*0.87, colour) #this branch is going stright on 
  }
=======
start_position2 <- c(2,2)
graphics.off()
plot(-50:50,-50:50, type = "n")
tree(start_position2, 0, 10)

draw_tree <- function()  {
  # clear any existing graphs and plot your graph within the R window
}

# Question 29
fern <- function(start_position, direction, length, limit = 0.1){
  first <- turtle(start_position, direction, length)
  if (length > limit) {
    fern(first, (direction - pi/4), length*0.38, limit)
    fern(first, direction, length*0.87, limit)
  }
  return("type your written answer here")
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
}


draw_fern <- function()  {
<<<<<<< HEAD
  graphics.off()
  plot(0:65,-35:30, type = "n", xlab = "",ylab = "", axes = 0, cex.main = 1, main = "Fern function output") # initalise empty new plot
  fern(c(0,0), 0, 8) #call tree function 
}

# Question 30
# Function that plots a fern on both sides! 
# does this using the new 'dir' input which decides whether the side branch goes to the 
# left or the right. 
fern2 <- function(start_position, direction, length, dir, colour = "dark green"){
  first <- turtle(start_position, direction, length, colour)
  limit = 0.1
  if (length > limit) {
    fern2(first, (direction + (dir * pi/4)), length*0.38, (dir*1)) #by multiplying dir by 1 it will alternate between plus and minus state
    fern2(first, direction, length*0.87, (dir*-1)) # this also changes changes
=======
  # clear any existing graphs and plot your graph within the R window
}

# Question 30

fern2 <- function(start_position, direction, length, dir, limit = 0.1){
  first <- turtle(start_position, direction, length)
  if (length > limit) {
    fern2(first, (direction + (dir * pi/4)), length*0.38, (dir*1), limit)
    fern2(first, direction, length*0.87, (dir*-1), limit)
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
  }
  return("type your written answer here")
}

<<<<<<< HEAD
draw_fern2 <- function()  {
  graphics.off()
  plot(-50:50,-5:95, type = "n", xlab = "",ylab = "", axes = 0, cex.main = 1, main = "Fern2 function output") 
  fern2(c(0,0), pi/2, 10, -1)
=======
plot(0:100,-50:50, type = "n")
fern(start_position2, 0, 10)

fern2(start_position2, 0, 10, 1)

fern2(start_position2, pi/2, 10, -1)

draw_fern2 <- function()  {
  # clear any existing graphs and plot your graph within the R window
>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
}

# Challenge questions - these are optional, substantially harder, and a maximum of 16% is available for doing them.  

# Challenge question A
<<<<<<< HEAD
# Function that plots the mean species richness as a function of time (measured in generations)
# across a large number of repeat simulations using the same parameters as in question 16
# the averages are then plotted with a 97.2% confidence interval on the species richness at each point in time 
# this is repeated for both high and low initial diversity and the number of generations needed for the system to 
#reach dynamic equilibrium is estimated and marked on the graph. 

Challenge_A <- function(){
  graphics.off()
  y <- c() #initialising vectors 
  y2 <- c()
  upper <- c()
  lower <- c()
  upper2 <- c()
  lower2 <- c()
  sim = 10 # setting up simulation size 
  duration = 2000 # setting up the number of generations 
  speciation_rate = 0.1 # setting the speciation rate
  community_max <- init_community_max(100) #initialising starting communities and low and high initial diversity 
  community_min <- init_community_min(100)
  for (i in 1:sim){ # loop that goes through the number of simulations 
    richness <- neutral_time_series_speciation(community_max, speciation_rate, duration)
    richness2 <- neutral_time_series_speciation(community_min, speciation_rate, duration)
    y <- sum_vect(y, richness) #adding vectors of richness as i go through simulations in order to average 
    y2 <- sum_vect(y2, richness2)
  }
  yav <- y/sim #averaging the two richnesses at their different initial community states by dividing by simulation size
  y2av <- y2/sim #these are the y axis for the plot
  error <- qnorm(.028,lower.tail=FALSE) #calculating the error using qnorm 
  upper <- (yav + error) #adding the error to the averaged y values to set upper limit 
  lower <- (yav - error) # subtracting the error to the averaged y values to set lower limit 
  upper2 <- (y2av + error)
  lower2 <- (y2av - error)
  xpol <- c(seq(0, duration), seq(duration, 0)) #setting the upper and lower x axis limits for the polygon
  ypol <- c(c(upper), rev(c(lower))) #setting the lower and upper limits of the polygon to plot the confidence intervals 
  ypol2 <- c(c(upper2), rev(c(lower2)))
  par(mfrow=c(2, 1)) #calling multiplot figure
  plot(yav, col = 'black', ylim = c(5,70), xlab = "Generations", ylab = "Species richness", cex.axis = 0.8, type = 'l')
  title(main = "Maximum initial community richness", line=0.5, cex.main=0.9) #adding title on top of plot
  polygon(xpol, ypol, col = adjustcolor("tomato",alpha.f=0.5), border = FALSE)#plotting polygon, making it transparent 
  lines(yav, col = 'black', lwd =0.5) #plotting the black line over the top of the polygon so it can be visualised 
  abline(v = 80, col = "red", lwd = 2)
  legend("topright",
         legend = c("Species richness", "97.2% confidence interval", "Suggested burn-in period:80"),
         col = c("black", "tomato", "red"),
         pch = c(20,20),
         bty = "n",
         pt.cex = 1,
         cex = 0.75,
         text.col = "black",
         horiz = F) #adding in a figure legend 
  plot(y2av, col = 'black', ylim = c(5,70), xlab = "Generations", ylab = "Species richness", cex.axis = 0.8, type = 'l')#plotting second line
  title(main = "Minimum initial community richness", line=0.5, cex.main=0.9) #adding title on top of it
  polygon(xpol, ypol2,col = adjustcolor("skyblue1", alpha.f = 0.5), border = FALSE) #adding polygon with transluscent colours so not overpowering
  lines(y2av, col = 'black', lwd = 0.5)#adding black line over top of polygon 
  abline(v = 80, col = "dark blue", lwd = 2)
  mtext("Neutral theory applied to a simulation of species richness in a community through time", outer=TRUE,  cex=1, line=-2) #adding title page
  legend("topright",
         legend = c("Species richness", "97.2% confidence interval", "Suggested burn-in period:80"),
         col = c("black", "skyblue1", "dark blue"),
         pch = c(20,20),
         bty = "n",
         pt.cex = 1,
         cex = 0.75,
         text.col = "black",
         horiz = F) #adding second legend 
}


########### CHALLENGE B
# Challenge question B
# Challenge B is a series of functions that intend to a plot a graph showing many averaged time series for a whole
# range of different initial species richnesses. 

# This defines the function that calculates the initial starting community 
#INITALISING COMMUNITY 
init_community<- function(size, richness){
  a <- seq(richness) #makes a sequence of the given richness
  community <- rep(a, size/richness) #repeats this sequence however many times size is given then divides it by total richness
  return(community) #returns the new community to be inputted into the function below 
}

#MAKING THE PLOT
#This function plots the averaged richness
functionforplot <- function(community){
  graphics.off()
  x <- c() #initalise empty x and y vectors 
  y <- c()
  sim = 10 #set the simulation size 
  duration = 500 # set the generations 
  speciation_rate = 0.1
  for (i in 1:sim){
    richness <- neutral_time_series_speciation(community, speciation_rate, duration) #calculate richness with given parameters
    y <- sum_vect(y, richness) #sum the richness of all simulations into the y vector to be averaged later
  }
  yav <- y/sim #divide the y vector by simulation size to average it 
  return(yav)
}

#Challenge_B Overall function 
#this function plots the multiple communities using a for loop to run through a pre-defined richness 
Challenge_B <- function() {
  size = 100 #set the size 
  richnesses <- c(1, 1:10*10) #create richness vector 
  richnessvect <- c() #initialise empty richness vector 
  for (x in richnesses){ #for each element in richnesses vector 
    community <- init_community(size, x) #use that element as the richness of that specific communnity and calculate the starting community from it
    a <- functionforplot(community) #save elements to plot into a variable called 'a' to be added to the vector for plotting
    richnessvect <- cbind(richnessvect, a) #bind these richnesses to a vector for later plotting
  }
  plot(x = 1:nrow(richnessvect), y = richnessvect[,1], type = "l", col = 'black', ylim = c(0,100), xlab = "Generations", ylab = "Species richness", cex.axis = 0.8) #plot lines
  title(main = "Species richness through generations across multiple averaged communities", line=0.5, cex.main=0.9)
  for (i in 2:ncol(richnessvect)){ #for loop specifying that for each coloum except colum 1 as that has already be plotted, add lines to preexisiting plot
    lines(x = 1:nrow(richnessvect), y = richnessvect[,i], col = i) #colour of each line corresponds to iteration number i 
  }
}

# Challenge question C
# function that plots the mean species richness against simulation generation 
# This is to inform us on how long each different size category would have needed as a burn in period. 
Challenge_C <- function() {
  graphics.off()
  vector500 = c() #initalise empty vectors 
  vector1000 = c()
  vector2500 <- c()
  vector5000 <- c()
  av500 <- c()
  av1000 <- c()
  av2500 <- c()
  av5000 <- c()
  counter500 <- 0
  counter1000 <- 0
  counter2500 <- 0
  counter5000 <- 0
  for (i in 1:100) { # go through each file of the simulation data
    load(file = gsub(" ", "", paste("EDIteration:", i, ".rda"))) #load it according to iteration number assuming it is in the same directory
    if(size == 500) { #if statements on size to sum vectors of richness
      vector500 <- sum_vect(vector500, richness)
      counter500 <- (counter500 + 1) #add to counter so it can later be averaged
    }
    if(size == 1000) {
      vector1000 <- sum_vect(vector1000, richness)
      counter1000 <- (counter1000 + 1)
    }
    if(size == 2500) {
      vector2500 <- sum_vect(vector2500, richness)
      counter2500 <- (counter2500 + 1)
    }
    if(size == 5000) {
      vector5000 <- sum_vect(vector5000, richness)
      counter5000 <- (counter5000 + 1)
    }
  }
  av500 <- vector500 / counter500  #average each vector by dividing by the counter
  av1000 <- vector1000 / counter1000
  av2500 <- vector2500 / counter2500
  av5000 <- vector5000 / counter5000
  par(mfrow=c(2,2)) #initialise four plots
  plot(x = 1:length(av500), y = av500, type = "l", col = 'black', xlab = "Generations", ylab = "Species richness", cex.axis = 0.8)
  title(main = "Size 500", line=0.5, cex.main=0.9) #add title 
  abline(v = 380, col = "dark blue", lwd = 2) #put line on plot 
  legend("bottomright",
         legend = c("Species richness", "Suggested burn-in period:380"),
         col = c("black", "dark blue"),
         pch = c(20,20),
         bty = "n",
         pt.cex = 1,
         cex = 0.75,
         text.col = "black",
         horiz = F) #initiate legend 
  mtext("Mean species richnesses against generations for four different values of size", outer = TRUE, cex.main=0.9, line=-2) #main title 
  plot(x = 1:length(av1000), y = av1000, type = "l", col = 'black', xlab = "Generations", ylab = "Species richness", cex.axis = 0.8)
  title(main = "Size 1000", line=0.5, cex.main=0.9)
  abline(v = 800, col = "dark blue", lwd = 2)
  legend("bottomright",
         legend = c("Species richness", "Suggested burn-in period:800"),
         col = c("black", "dark blue"),
         pch = c(20,20),
         bty = "n",
         pt.cex = 1,
         cex = 0.75,
         text.col = "black",
         horiz = F)
  plot(x = 1:length(av2500), y = av2500, type = "l", col = 'black', xlab = "Generations", ylab = "Species richness", cex.axis = 0.8)
  title(main = "Size 2500", line=0.5, cex.main=0.9)
  abline(v = 1000, col = "dark blue", lwd = 2)
  legend("bottomright",
         legend = c("Species richness", "Suggested burn-in period:1000"),
         col = c("black", "dark blue"),
         pch = c(20,20),
         bty = "n",
         pt.cex = 1,
         cex = 0.75,
         text.col = "black",
         horiz = F)
  plot(x = 1:length(av5000), y = av5000, type = "l", col = 'black', xlab = "Generations", ylab = "Species richness", cex.axis = 0.8)
  title(main = "Size 5000", line=0.5, cex.main=0.9)
  abline(v = 2000, col = "dark blue", lwd = 2)
  legend("bottomright",
         legend = c("Species richness", "Suggested burn-in period:2000"),
         col = c("black", "dark blue"),
         pch = c(20,20),
         bty = "n",
         pt.cex = 1,
         cex = 0.75,
         text.col = "black",
         horiz = F)
}


# Challenge question D
#J = 5
#v = 2
#Challenge_D <- function() {
#  lineages <- c(rep(1, length(1:J)))
#  abundances <- c()
#  N = J
#  zero <- v * ((J-1)/(1-v))
#  while (N > 1){
#  j = sample(lineages, 1)
#  randum <- runif(1, 0, 1)
#  if (randum < (zero/ (zero + N -1))){
#    abundances <- c(abundances, j)
#  }
#  if (randum >= (zero/ (zero + N -1))){
#    i = sample(1, lineages)
#    lineages[i] = lineages[i] + lineages[j]
#  }
#  lineages <- lineages[-lineages[j]]
#  N - 1
#  }
#  abundances <- c(abundances, lineages)
#  # clear any existing graphs and plot your graph within the R window
#  return(abundances)
#}

# Challenge question E

Challenge_E <- function() {
  graphics.off()
  A = c(-3,2) #coordinates of equilateral triangle 
  B = c(1,-2)
  C = c(3,4)
  X = c(-3,2)
  points <- list(A, B, C) #put ocordinates into a list 
  plot(x = X[1], y = X[2], ylim = c(-3,6), xlim = c(-4, 4), col = "green", cex = 0.5, xlab = "X coordinates", ylab = "Y coordinates", main = "Sierpinski Gasket", cex.main = 0.9, pch = 20, cex.axis = 0.8) #plot the cinitial coordinate X
  for (i in 1:10000){ #lots of iterations 
    new <- sample(points, 1) #randomly sample points
    newnum <- c(as.numeric(new[[1]][1]), as.numeric(new[[1]][2])) #change list to numeric vector 
    Xnew <- ((newnum - X)/2) #divide newnum by 2 to get the half way point and subtract X to get the distance
    X <- Xnew + X #add X coordinate and cordinates of half the distance to x so that it moves half way to the coordinate A, B or C
    if (newnum == A){
      points(x = X[1], y = X[2], col = "blue", cex = 0.25) #if statements to say that if the randomly selected number is A, B or C the point should be coloured differently 
    } 
    if (newnum == B) {
      points(x = X[1], y = X[2], col = "dark green", cex = 0.25)
    }
    if (newnum == C) {
      points(x = X[1], y = X[2], col = "brown", cex = 0.25)
    } 
  }
  return("Nothing appeared to change when I changed the starting point of x except in the direction the points filled out in. One interesting observation from colouring each of the three cordinates a different colour is that the three coordinates act completely independantely of one another and the traingle is plotted by the coordinates independantly plotting their own triangle to join at the end")
}

# Challenge question F
F_challenge1 <- function(start_position, direction, length, colour){
  first <- turtle(start_position, direction, length, colour)
  limit = 0.1
  if (length > limit) {
    F_challenge1(first, (direction - pi/2), length*0.65, colour = "pink")
    F_challenge1(first, (direction + pi/2), length*0.65, colour = "red")
  }
  return("type your written answer here")
}


F_challenge2 <- function(start_position, direction, length, colour = "dark green"){
  first <- turtle(start_position, direction, length, colour)
  limit = 0.1
  if (length > limit) {
    F_challenge2(first, (direction - pi/4), length*0.85, colour = "purple") #this branch is going 45 degrees to the left 
    F_challenge2(first, (direction + pi/2), length*0.35, colour)#this branch is going stright on 
  }
}


F_challenge3 <- function(start_position, direction, length, colour = "dark green"){
  first <- turtle(start_position, direction, length, colour)
  limit = 0.1
  if (length > limit) {
    F_challenge3(first, (direction + pi/13), length*0.9, colour) #this branch is going 45 degrees to the left 
    F_challenge3(first, direction, length*0.37, colour) #this branch is going stright on 
  }
}


F_challenge4 <- function(start_position, direction, length, colour){
  first <- turtle(start_position, direction, length, colour = "dark green")
  limit = 0.1
  if (length > limit) {
    F_challenge4(first, (direction + pi/8), length*0.85, colour = "red")
    tree(first, (direction - pi/2), length*0.35, colour = "dark red")
  }
  return("type your written answer here")
}

Challenge_F <- function(){
graphics.off()
par(mfrow=c(2,2))
plot(-15:15,-5:25, type = "n", ann = FALSE, axes = 0)
F_challenge1(c(0,0), pi/2, 10, "green")
plot(-10:30, -10:30, type = "n", axes = 0, ann = FALSE)
F_challenge2(c(0,0), pi/2, 10, "pink")
plot(-30:10, 0:40, type = "n", axes = 0, ann = FALSE)
F_challenge4(c(0,0), pi/2, 10, "green")
plot(-40:40, -40:40, type = "n", axes = 0, ann = FALSE)
for(i in 1:6){
  F_challenge3(c(0,0), i, 7, colour = i)
}
}

=======
Challenge_A <- function() {
  # clear any existing graphs and plot your graph within the R window
}

# Challenge question B
Challenge_B <- function() {
  # clear any existing graphs and plot your graph within the R window
}

# Challenge question C
Challenge_C <- function() {
  # clear any existing graphs and plot your graph within the R window
}

# Challenge question D
Challenge_D <- function() {
  # clear any existing graphs and plot your graph within the R window
  return("type your written answer here")
}

# Challenge question E
Challenge_E <- function() {
  # clear any existing graphs and plot your graph within the R window
  return("type your written answer here")
}

# Challenge question F
Challenge_F <- function() {
  # clear any existing graphs and plot your graph within the R window
  return("type your written answer here")
}

>>>>>>> 0d7590ae85f1493548f944ace5922e4c47068ab7
# Challenge question G should be written in a separate file that has no dependencies on any functions here.

