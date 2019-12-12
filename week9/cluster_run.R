rm(list=ls()) # good practice 
graphics.off()
source("HPC.R")



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

cluster_run(speciation_rate = 0.003517, size = newiter, wall_time = 10, interval_rich = 1, interval_oct = 10, burn_in_generations = 200, output_file_name = myfilename)




