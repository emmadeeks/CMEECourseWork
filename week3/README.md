# CMEE Coursework Repository- README.md 

##  Week3: 

Focuses on the basics of R as well as data exploration, management and visualisation; Topics covered include:
- Basic R syntax and programming conventions assuming you have never set your eyes on R
- Principles of data processing and exploration (including visualization) using R
- Principles of clean and efficient programming using R
- To generate publication quality graphics in R
- To develop reproducible data analysis "work flows" so you (or anybody else) can run and re-run your analyses, graphics outputs and all, in R
- To make R simulations more efficient using vectorization
- To find and fix errors in R code using debugging
- To make data wrangling and analyses more efficient and convenient using custom tools such as tidyr
- Some additional tools and topics in R (accessing databases, building your own packages, etc.).

### Code: Bold scripts are part of the practical coursework for week 3

| Script       | Function     | Input     | Output    |
| :------------- | :----------: | -----------: |-----------: |
|  `apply1.R` |  Builds a random matrix  |  No input  |  Calculates mean of each row in matrix  |
|  `apply2.R` |  Function of if statement that states if a number is lower than 0 multiple it by 100  |  Manual input not essential but can put number/matrix for function to be applied over  |  Matrix of random numbers function has been applied over  |
|  `AutoCorr.tex` |  Latex document produced as a result of the Auto Correlation practical in weather  |  No input  |  Completed Latex document  |
|  `basic_io.R` |  Script to illustrate the different ways to write in a file and different parameters  |  No manual input required but uses trees.csv data from data directory  |  csv file names MyData in global environment  |
|  `boilerplate.R` |  Illustrates how R functions work  | Two arguments defined within script   |  Class of inputs  |
|  `break.R` |  Script showing how to break out of loop  |  No input   |  Depending on if value is equal to 10 will output what number is equal to  |
|  `browse.R` |  Function that shows a different way of debugging by running a simulation of exponential growth and returning a vector of generations before plotting it. Shows how to use break to run through the code line by line and debug.  |  Once run you can input code to navigate the break function   |  first iteration of the for loop and the console will enter the browser mode then allows manual debugging  |
|  `control_flow.R` |  Illustrates the use of if statements, while loops and for loops.  |  No input  |  The results from each loop illustrated  |
|  `DataWrang.R` |  Script illustrating how to 'wrangle' data using the reshape2 package and other functions in R  |  No manual input required but uses the PoundHillData.csv and PoundHillMetaData.csv from data  |  Both inputted datasets are in an improved format and under variable names 'MyData' and 'MyMetaData' respectively  |
|  **`DataWrangTidy.R`** |  Script illustrating how to 'wrangle' data using the 'dplyr' and 'tidyr' package and other functions in R  |  No manual input required but uses the PoundHillData.csv and PoundHillMetaData.csv from data  |  Both inputted datasets are in an improved format and under variable names 'MyData' and 'MyMetaData' respectively  |
|  `get_Tree_Height.py` |  Function that calculates heights of trees given distance of each tree from its base and angle to its top, using  the trigonometric formula |  Degrees and distance but not required manually as there is a default of 'trees.csv' from data  |  Heights of the tree, same units as distance  |
|  `get_Tree_Height.R` |  Function that calculates heights of trees given distance of each tree from its base and angle to its top, using  the trigonometric formula  |  Degrees and distance but not required manually as there is a default of 'trees.csv' from data  |  Heights of the tree, same units as distance  |
|  `Girko.R` |  Function that returns an eclipse  |  No input  |  Girko.pdf in results with eclipse |
|  `GPDD_Data.R` |  Loads and plots the species abundance worldwide using the maps package and saves it as a pdf  |  Uses GPDDFiltered.RData in data  |  Species abundance worldwide  |
|  `MyBars.R` |  Script that builds a plot and saves as a pdf  |  Text file of Resuts.txt from data directory  |  pdf called 'MyBars.pdf' in results directory  |
|  `next.R` |  A for loop that runs through 1-10 and prints each number  |  No input  |  i in for loop  |
|  `plotLin.R` |  A script that plots data and adds linear regression line  |  No input  |  pdf called 'MyLinReg.pdf' in results  |
|  **`PP_Lattice.R`** |  Script that makes three pdf lattices of prey mass, predator mass and the size ratio of prey mass over predator mass. Script also calculates the mean, median and log of these for prey mass, predator mass and size ratio into a csv file. Script calculates this for the data subsetted by feeding type  |  EcolArchives-E089-51-D1.csv from data- no manual input  |  3 pdf and a csv file with values  |
|  **`PP_Regress_loc.R`** |  Script that  calculates the regression of data when its been subsetted three times and outputs the results to a table in csv format |  EcolArchives-E089-51-D1.csv from data- no manual input   |  csv file with the linear results; 'PP_Regress_loc_Results.csv' in results directory   |
|  **`PP_Regress.R`** |  Script that creates and saves a graph as a pdf file that exactly replicates a graph and also calculates the regression of the data when its been subsetted two times and outputs the results to a table  |   EcolArchives-E089-51-D1.csv from data- no manual input   |  PDF file of replicate graph; 'PP_Regress.pdf' and also csv file with the linear results; 'PP_Regress_Results.csv' in results directory  | 
|  `preallocate.R` | Illustrating the speed of allocation with one loop having preallocated values and one not |  No input  |  Speeds of two for loops   |
|  `Ricker.R` |  A vectorization challenge that runs a simulation of the Ricker model and returns a vector of length generations  |  No input  |  Plots Ricker model  |
|  `sample.R` |  Example script of using vectorization involving lapply and sapply. Also times the different functions under a vectorized and looped approach to lapply and sapply.  |  No input  |  The result of the functions using loops and vectorisation as well as the time taken to run each function with apply, vectorization and preallocation  |
|  `SQLinR.R` |  Script to demonstrate how data can be used to access update and manage databases easily  |  Has default data entry  |  No output as dataframes removed from global environment after script is run  |
|  **`TAutoCorr.R`** |  An exercise in correlation coefficients and P-values. Calculates the correlation between n-1 pairs of years in temperature, script loads the KeyWestAnnualMeanTemperature data using load and computes the coefficient for this data before randomly shuffling the data 10000 times to randomly permute the time series and then recalculate the correlation coefficient for each randomly permuted year sequence and storing it.  |  No manual input but uses the KeyWestAnnualMeanTemperature data   |  The fraction of the correlation coefficients from the previous step were greater that that from step 1. Also out outputs a Latex file interpreting results. Also outputs pdf of graph for lattice. |
|  **`TreeHeight.R`** |  Calculates heights of trees given distance of each tree from its base and angle to its top, using  the trigonometric formula  | Two arguments, degrees: The angle of elevation of tree; distance: The distance from base of tree (e.g., meters). Or it can take the relative path of a data file and calculate the tree height. As a default it uses the trees.csv file in data  |  TreeHts.csv in results containing tree heights data appended onto trees.csv  |
|  `try.R` |  Demonstrates how to use try to catch errors in script- Runs a simulation that involves sampling from a synthetic population with replacement and takes its mean, but only if at least 30 unique samples are obtained   |  No input  |  Prints errors in script to variable  |
|  **`Vectorize1.py`** |  A vectorisation example which compares the time taken to run a vectorisation function compared to a loop, this script is the python version of 'Vectorize1.R' |  No input  |  Time taken for loop and in build vectorisation function to run  |
|  `Vectorize1.R` |  A vectorisation example which compares the time taken to run a vectorisation function compared to a loop   |  No input  |  Time taken for loop and in build vectorisation function to run  |
|  **`Vectorize2.py`** |  Two scripts, one stochastic Ricker model and an improved version of this model which is vectorised, this script is the python version of 'Vectorize2.R'   |  No input  |  Speed comparison of both scripts |
|  **`Vectorize2.R`** |  Two scripts, one stochastic Ricker model and an improved version of this model which is vectorised  |  No input  |  Speed comparison of both scripts  |


### Data: 

    	EcolArchives-E089-51-D1.csv: For Lattice practicals 
    	GPDDFiltered.RData: For mapping practical
    	KeyWestAnnualMeanTemperature.RData: Autocorrelate practicals
    	PoundHillData.csv: Data Wrangling practicals
    	PoundHillMetaData.csv: Data Wrangling practicals
    	trees.csv: Tree Height practicals 

### Results: 
Where the results from scripts are stored
	
 #### Sandbox: 
 A practice directory for rough work and practice 
 
  ## Additional notes on packages required for scripts: 
  To run all R scripts the associated packages are required: 
  -  DataWrang.R: reshape2
  -  DataWrangTidy.R: dplyr and tidyr
  -  get_Tree_Height.py: math, os, sys, csv, scipy as sc
  -  GPDD_Data.R: maps
  -  PP_Lattice.R: lattice
  -  PP_Regress.R: ggplot2
  -  SQLinR.R: sqldf
  -  Vectorize1.py: time, numpy as np
  -  Vectorize2.py: random, numpy as np, time 
    
    
    
    
    