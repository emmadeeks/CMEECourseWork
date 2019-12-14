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

# Questions 18 and 19 involve writing code elsewhere to run your simulations on the cluster



# Question 20 
process_cluster_results <- function()  {
  graphics.off()
  counter500 <- 0
  counter1000 <- 0
  counter2500 <- 0
  counter5000 <- 0
  newvect500 <- c()
  newvect1000 <- c()
  newvect2500 <- c()
  newvect5000 <- c()
  average500 <- c()
  average1000 <- c()
  average2500 <- c()
  average5000 <- c()
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
question_21 <- function()  {
  a <- list('1.89', "Using the Koch Curve equation, I set the the Width of the square at 3, because it is three 'squares' long
            and the size as 8 as there are 8 squares of the subunit. I then divided log of size (8) by the log of width (3) to get 1.89")
  return(a)
}

# Question 22
question_22 <- function()  {
  a <- list("2.73", "Using the box method for working out the dimension, the number 
            of hypercubes was set as 20 and the hypercube length set as a 1/3 as that was 
            the fraction where the 'hole' in the centre of the cube was excluded. the equation was log(20)/log(1/3) and log(1)")
  return(a)
}

# Question 23
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
  return(newpositions) # you should return your endpoint here.
}

# Question 25
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
}



# Question 28
tree <- function(start_position, direction, length, limit = 0.1){
  first <- turtle(start_position, direction, length)
  if (length > limit) {
    tree(first, (direction - pi/4), length*0.65, limit)
    tree(first, (direction + pi/4), length*0.65, limit)
  }
  return("type your written answer here")
}

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
}


draw_fern <- function()  {
  # clear any existing graphs and plot your graph within the R window
}

# Question 30

fern2 <- function(start_position, direction, length, dir, limit = 0.1){
  first <- turtle(start_position, direction, length)
  if (length > limit) {
    fern2(first, (direction + (dir * pi/4)), length*0.38, (dir*1), limit)
    fern2(first, direction, length*0.87, (dir*-1), limit)
  }
  return("type your written answer here")
}

plot(0:100,-50:50, type = "n")
fern(start_position2, 0, 10)

fern2(start_position2, 0, 10, 1)

fern2(start_position2, pi/2, 10, -1)

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

