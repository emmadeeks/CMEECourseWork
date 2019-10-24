CMEE Coursework Repository- readme.txt 

Week3 focuses on the basics of R as well as data exploration, management and visualisation; Topics covered include:
- Basic R syntax and programming conventions assuming you have never set your eyes on R
- Principles of data processing and exploration (including visualization) using R
- Principles of clean and efficient programming using R
- To generate publication quality graphics in R
- To develop reproducible data analysis "work flows" so you (or anybody else) can run and re-run your analyses, graphics outputs and all, in R
- To make R simulations more efficient using vectorization
- To find and fix errors in R code using debugging
- To make data wrangling and analyses more efficient and convenient using custom tools such as tidyr
- Some additional tools and topics in R (accessing databases, building your own packages, etc.).

Files included:
Week3 
    Code 
    apply1      apply2      AutoCorr.tex    basic_io.R      boilerplate.R      break.R      browse.R        control_flow.R      DataWrangTidy.R     GPDD_Data.R     next.R      PP_Lattice.R        PP_Regress.R    preallocate.R       Ricker.R        sample.R        SSQLinR.R       TAutoCorr.R     TreeHeight.R        try.R       Vectorize1.R        Vecotrize2.R
   
         
    results



File functions: 

Week2 
    Code
        basic_io.R: 
            Function: Illustrates R input-output.

        control_flow.R: 
            Function: Illustrates the use if if statements, while loops and for loops.
            Output: the results from each loop illustrated.

        break.R: 
            Function: Illustrates how to break out of a loop when a certain condition is met
            Output: output of a while loopp where there is a break in the look for a condition to be met and then moving on to 'else' 

        boilerplate.R: 
            Function: Illustrates how to write R functions using the classic boiler plate examples.
            Output: The automated input class

        TreeHeight.R:  
            Function: Function to calculate heights of trees
            Inputs: Two arguments, degrees: The angle of elevation of tree; distance: The distance from base of tree (e.g., meters). Or it can take the relative path of a data file and calculate the tree height. 
            Output: The heights of the tree, sample units as 'distance' to a csv file in the results directory called "TreeHts.csv"
            ***** PRACTICAL ONE *******


        Vectorize1.R: 
            Function: A vectorisation example which compares the time taken to run a vectorisation function compares to aa loop 
            Inputs: No input 
            Output: Time taken for loop and in build vectorisation function to run 



        preallocate.R: 
            Function: Illustrating the speed of allocation
                        
        apply1.R: 
            Function: Builds a random matrix and makes a mean of each row in that matrix. 
            Output: The mean and variance of each column and row of the matrix
            
        apply2.R: 
            Function: How to use apply to define your own functions. This function is an if statement that states if a number is lower than 0 multiple it by 100 and return the output
            Output: Matrix of random numbers that the function has been applied over. 
        
        sample.R: 
            Function: Example script of using vectorization involving lapply and sapply. Also times the different functions under a vectorized and looped approach to lapply and sapply.
            Output: The result of the functioons using loops and vectorisation as well as the time. 

        Ricker.R: 
            Function: A vectorization challenge that runs a simulation of the Ricker model and returns a vector of length generations. 

        Vectorize2.R: 
            Function: PRACTICAL 2: two scripts, one stochastic Ricker model and an improved version of this model which is vectorised
            Output: Speed comparison of both scripts 

        try.R: 
            Function:runs a simulation that involves sampling from a synthetic population with 
            replacement and takes its mean, but only if at least 30 unique samples are obtained. Goes through example functions using try and lapply to illustrate how to debug errors in R
            Output: Output of the functions 
        
        browse.R: 
            Function: Function that shows a different way of debugging by running a simulation of exponential growth and returning a vector of generations before plotting it. Shows how to use break to run through the code line by line and debug. 
            Input: Once run you can input code to navigate the break function 


        TAutoCorr.R: PRACTICAL 3
            Function: an excercise in correlation coefficients and P-values. Calculates the correlation between n-1 pairs of years in temperature, script loads the KeyWestAnnualMeanTemperature data using load and computes the coefficient for this data before randomly shuffling the data 10000 times to randomly permute the time series and then recalculate the correlation coefficient for each randonly permuted year sequene and storing it.
            Output: The fraction of the correlation coefficients from the previous step were greater that that from step 1. Also out outputs a Latex file interpreting results.
            Also outputs pdf of graph for lattice. 

        SQLinR.R: 
            Function: Script to demonstrate how data can be used to access update and manage databases easily. 

        PP_Lattice.R: PRACTICAL 4
            Function: Script that makes three pdf lattices of prey mass, predator mass and the size ratio of prey mass over predator mass. Script also calculates the mean, median and log of these for prey mass, predator mass and size ratio into a csv file. Script calculates this for the data subsetted by feeding type. 
            Output: 3 pdf and a csv file with values. 

        PP_Regress.R: PRACTICAL 5
            Function: Script that creates and saves a graph as a pdf file that exactly replicates a graph and also calculates the regression of the data when its been subsetted two times and outputs the results to a table called PP_Regress_Results.csv.
            Output: PDF file of replicate graph and also csv file with the linear results 

        GPDD.R: PRACTICAL 6
            Function: Loads and plots the species abundance worldwide using the maps package and saves it as a pdf. 

        
    Data
    EcolArchives-E089-51-D1.csv
    GPDDFiltered.RData
    KeyWestAnnualMeanTemperature.RData
    PoundHillData.csv
    PoundHillMetaData.csv
    trees.csv 