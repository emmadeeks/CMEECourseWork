# CMEE 2019 HPC excercises R code main proforma
# you don't HAVE to use this but it will be very helpful.  If you opt to write everything yourself from scratch please ensure you use EXACTLY the same function and parameter names and beware that you may loose marks if it doesn't work properly because of not using the proforma.

name <- "Emma Deeks"
preferred_name <- "Emma"
email <- "ead19@imperial.ac.uk"
username <- "edeeks"
personal_speciation_rate <- 0.002 # will be assigned to each person individually in class and should be between 0.002 and 0.007

# Question 1
#Each number in a community is a individual but the different numbers 
# represents different species- species richness outputs the number 
#of different numbers/species
species_richness <- function(community){
  length(unique(community))
}


# Question 2
# This gives the maximum number of different specis in a community which
# is specified but the 'size' within the function- gives sequence
init_community_max <- function(size){
  seq(size)
}

# Question 3
# effect of initial condition- generates an alternative initial state
# for your simulation of a certain size with the mimum possible number of species
# Gives total number of individuals for given size 
init_community_min <- function(size){
  c(rep(1, size))
}

# Question 4
# Chooses random number according to uniform distribution between i and max vallue
# chooses second random number and returns vector of length
choose_two <- function(max_value){
  sample(1:max_value, 2)
}

# Question 5
# performs a single step of a simple neutral model simulation WITHOUT SPECIATION
# on a commmunity vector. picks idividual to die and replaces with reproduction
# Are not same individual but could be of the same species. returns community states with equal probability
neutral_step <- function(community){
  first <- choose_two(length(community))
  community[first[1]] <- community[first[2]]
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
  steps <- round(jitter(length(community)/2, amount = 0.1))
  for (i in 1:steps){
    community <- neutral_step(community)
  }
  return(community)
}


# Question 7
# rns the neutral theory simulation and return time series of species richness
# inputs community and number of generations 
# gives species richness at each generation of simulation
neutral_time_series <- function(community, duration){
  richness <- c()
  for (i in 1:duration){
    community <- neutral_generation(community)
    p <- species_richness(community)
    richness <- c(richness, p)
  }
  return(richness)
}


# Question 8
# Plots time series of neutral time series from initial conidition of maximum diversity
# not inputs 
question_8 <- function(){
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
}

# Question 9
# performs a step of a neutral model but WITH SPECIATION 
# speciation replaces dead indidivual with new species witht he probably of the given speciation rate 
# if probability not met the dead indiivual is replaced with the offspring of another inidividual like neutral step
# speciation rate is an input parameter 
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
}


# Question 11
neutral_time_series_speciation <- function(community,speciation_rate,duration){
  richness <- c()
  for (i in 1:as.integer(duration)){
    community <- neutral_generation_speciation(community, speciation_rate)
    p <- species_richness(community)
    richness <- c(richness, p)
  }
  return(richness)
}


# Question 12

question_12 <- function(){
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
}


# Question 13
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
  if (length(x) < length(y)){
    x <- c(x, rep(0, length(y)- length(x)))
  } else if (length(x) > length(y)) {
    y <- c(x, rep(0, length(x)-length(y)))
  }
 x + y
}

# Question 16 
question_16 <- function()  {
  duration = 200
  speciation_rate = 0.1
  vector <- species_richness(community)
  for (x in 1:duration){
    richness_community <- neutral_generation_speciation(init_community_max(100), speciation_rate)
  }
  for (x in 1:duration){
    richness_min_community <- neutral_generation_speciation(init_community_min(100), speciation_rate)
  }
  octave_max <- octaves(species_abundance(richness_community))
  octave_min <- octaves(species_abundance(richness_min_community))
  for (x in 1:2000){
    richness_community_2000 <- neutral_generation_speciation(richness_community, speciation_rate)
  }
  for (x in 1:2000){
    richness_min_community_2000 <- neutral_generation_speciation(richness_min_community, speciation_rate)
  }
  return("type your written answer here")
}

richness <- neutral_time_series_speciation(init_community_max(100), speciation_rate, duration)
richness2 <- neutral_time_series_speciation(init_community_min(100), speciation_rate, duration)
x <- c()
y <- c()
y2 <- c()
# Question 17
cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name)  {
  
}

# Questions 18 and 19 involve writing code elsewhere to run your simulations on the cluster

# Question 20 
process_cluster_results <- function()  {
  # clear any existing graphs and plot your graph within the R window
  combined_results <- list() #create your list output here to return
  return(combined_results)
}

# Question 21
question_21 <- function()  {
  return("type your written answer here")
}

# Question 22
question_22 <- function()  {
  return("type your written answer here")
}

# Question 23
chaos_game <- function()  {
  # clear any existing graphs and plot your graph within the R window
  return("type your written answer here")
}

# Question 24
turtle <- function(start_position, direction, length)  {
  
  return() # you should return your endpoint here.
}

# Question 25
elbow <- function(start_position, direction, length)  {
  
}

# Question 26
spiral <- function(start_position, direction, length)  {
  return("type your written answer here")
}

# Question 27
draw_spiral <- function()  {
  # clear any existing graphs and plot your graph within the R window
  
}

# Question 28
tree <- function(start_position, direction, length)  {
  
}
draw_tree <- function()  {
  # clear any existing graphs and plot your graph within the R window
}

# Question 29
fern <- function(start_position, direction, length)  {
  
}
draw_fern <- function()  {
  # clear any existing graphs and plot your graph within the R window
}

# Question 30
fern2 <- function(start_position, direction, length)  {
  
}
draw_fern2 <- function()  {
  # clear any existing graphs and plot your graph within the R window
}

# Challenge questions - these are optional, substantially harder, and a maximum of 16% is available for doing them.  

# Challenge question A
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

# Challenge question G should be written in a separate file that has no dependencies on any functions here.

