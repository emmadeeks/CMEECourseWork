# CMEE Coursework Repository- README.md 

##  MINIPROJECT: 

A project addressing the question on what mathmatical model best fits an empirical dataset? 

### Code: Bold scripts are part of the practical coursework for week 3

| Script       | Function     | Input     | Output    |
  | :------------- | :----------: | -----------: |-----------: |
  |  `preparing_data.py` |  Prepared data for the model fitting script in R  |  No input  |  Outputs a csv file entitled 'modified_CRat.csv that has no NA values and any functional response curves with less than 5 data points have been removed.  |
  |  `fitting_script.R` |  Script that fits the four models to the functional response curves and outputs two tables, one approriate for statistics and the other approriate for plotting. also outputs a sample plot for the methods section on the curve fitting |  Takes as input the 'modified_CRat.csv' script created in the preparing_data.py script  | Two tables, one entitled 'optiisedtable.csv' and the other 'MergedOptTable.csv' into the data directory of the miniproject directory to be used by the potting_script.py. Also produces plot in results directory for the latex document 'miniproject_write.tex' in order to view the model fits |
  |  `plotting_script.py` |  script that takes the two tables produced by the fitting_script.R and creates three plots as well as does statistical analysis on the data  |  Two tables, one entitled 'optiisedtable.csv' and the other 'MergedOptTable.csv' from the data directory of the miniproject folder  |  Three plots and statistic resuts |
  |  `miniproject.bib` |  bibliography for the latex document 'miniproject_write.tex' to use  |  No input  |  References for write up |
  |  `miniproject_write.tex, .bbl, .bcf, .blg, .dvi, .log` |  Latex documents for miniproject write up  |  No input  |  'miniproject_write.pdf' |
  |  `miniproject_write.pdf` |  Final miniproject report  |  No input | Latex document compiled |
  |  `run_miniProject.sh` |  bash script that compiles the miniproject scripts and runs them  |  No input   |  mini_project.pdf latex report of miniproject |
  
  
 
  ## Additional notes on packages required for scripts: 
  To run all R scripts the associated packages are required: 
  -  Preparing_data.py: pandas (For manipulating dataframes), numpy (for removing NAs and replacing them)
  -  fitting_script.R: dplyr and tidyr for datawrangling and 'minpack.lm' for fitting linear models to data
