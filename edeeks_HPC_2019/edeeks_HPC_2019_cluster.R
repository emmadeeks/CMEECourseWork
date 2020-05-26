rm(list=ls()) # good practice 
graphics.off()

species_richness <- function(community){
  length(unique(community))
}

init_community_min <- function(size){
  c(rep(1, size))
}

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
  diff <- length(x)-length(y)
  if (diff > 0) {
    y <- c(y, rep(0, abs(diff))) }
  if (diff < 0) {
    x <- c(x, rep(0, abs(diff))) }
  vector_sum <- x + y
  return(vector_sum)
}

# Question 16 
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
}



# Question 17
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


newiter <- whichsize(iter)
myfilename <- paste("EDIteration:", iter, ".rda", sep = "")
octpar = newiter/10
burngen = 8*newiter

cluster_run(speciation_rate = 0.003517, size = newiter, wall_time = 690, interval_rich = 1, interval_oct = octpar, burn_in_generations = burngen, output_file_name = myfilename)




