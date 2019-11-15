# CMEE Coursework Repository- README.md 

## Week2 

Focuses on the basics of Python; Topics covered include:
- Basics of python syntax and data structures
- Python's object-oriented features
- How to write and run python code
- Understand and implement python control flow tools
- Learning to use the ipython environment 
- Writing, debugging, using, and testing python functions 
- Learning efficient numerical programming in python
- Using regular expressions in python
- Introduction to certain particularly useful python packages
- Using python for building and modifying databases
- Using python to run other, non-python tasks and code
- Using python to patch together data analysis and/or numerical simulation work flows

### Code:   Bold scripts are part of the practical coursework for week 2

| Script       | Function     | Input     | Output    |
| :------------- | :----------: | -----------: |-----------: |
|  **`align_seqs_better.py`** |  Runs alignments on files and inputs the highest alignments into a single text file as oppose to align_seqs_fatsa.py which outputs the last highest alignment the programme finds  |  Option to manually input 2 fasta files to be aligned or to have a default where there are no inputs  |  Text file with best alignment  |
|  **`align_seqs_fasta.py`** |  Same basic script as 'align_seqs.py' but aligns fasta files.  |  Option to manually input 2 fasta files to be aligned or to have a default where there are no inputs  | Text file with best aligment   |
|  **`align_seqs.py`** |  Converts the initial 'align_seqs.py' script to a programme that takes DNA sequences as an input from a single external file and saves the best alignment along with the score into a text file.   |  No manual input but in script takes single external csv file with DNA sequences   |  Text file with best alignment  |
|  ` basic_csv.py` |  Reads in the testcsv.csv file containing phylogeny information and appends a tuple with the name of a species.  |  No manual input but uses testcsv.csv from data directory |  bodymass.csv  |
|  `basic_io1.py` |  Returns the contents of 'test.txt' in different formats e.g. excludes second lines  |  No manual input but reads in test.txt  |  Formatted 'test.txt'  |
|  `basic_io2.py` |  Saves the elements of a list to a file and adds a new line when saving  |  No manual input but reads in test.txt  |  testout.txt file to sandbox directory   |
|  `basic_io3.py` |  Uses pickle to store an object from a dictionary before loading data again  |  No manual input but reads in testp.p  |  testp.p to sandbox directory with dictionary  |
|  `boilerplate.py` |  Example exercise on writing python programmes. Programme prints 'This is a boilerplate'). Also illustrates use of docstrings  |  No input  |  'This is a boilerplate'  |
|  **` cfexercises1.py`** |  Conditionals exercise containing 6 different foo_x calculation functions. Later modified to make a module that tests the foo_x functions as well as runs test arguments  |  No input  | The output of all 6 foo_x calculations   |
|  ` cfexercises2.py` |  A series of conditionals containing for and while loops   |  No input  |  Output of for a while loops  |
|  ` control_flow.py` |  Similar to boilerplate.py in structure as its a python programme but adds docstrings and a series of functions  |  No input  |  Outputs of all functions in script  |
|  `debugme.py` |  A script with a bug that was corrected with debugging  | Manual input not needed  | uses ipdb to trace bug |
|  **`dictionary.py`** |  Populates a dictionary to map order names to sets of taxa  |  No input  |  Completed dictionary  |
|  **` lc1.py`** |  List comprehensions compared to for loops   |  No input  | Three lists containing latin names, common names and mean body masses for each species of birds in a given list of birds   |
|  **` lc2.py`** |  Uses list comprehensions and for loops  |  No input  |  Creates a list of month and rainfall tuples when the amount of rain was; Greater than 100mm and Less than 50mm from a list of rainfall averages  |
|  `loops.py` |  A series of for and while loops   |  No input  |  output of "Geronimo! infinite loop!"  |
|  ** `oaks_debugme.py`** |  A script with a bug that was corrected with debugging  | File with tree species; **Note: must enter species name exactly**  |  File with only oak species found in list inputted |
|  `oaks.py` |  Uses for loops and list comprehensions to find taxa that are oak trees from a list of species.   |  No input  |  Oak species from list  |
|  ` scope.py` |  A collection of three blocks of code illustrating variable scope.  |  No input  |  Output of variable scope functions  |
|  ` sysargv.py` |  Exercise of the sys.argv module which contains the names of the argument variables in the current script  |  Script  |  Name, number of arguments and the arguments of the script  |
|  `test_control_flow.py` |  Function exemplifying the use of control elements  |  No input  |  Prints doctest  |
|  **` tuple.py`** |  Script to show use of tuples  |  No input  |  Prints the latin name, common name and mass of a list of birds on separate lines  |
|  ` using_name.py` |  An if else statement within a python programme |  No input  |  Prints whether the programme is being imported from another module of being run by itself  |


### Data: 
    
Files

	407228326.fasta: Input file for fasta exercise of align_seq practical   
	407228412.fasta: Input file for fasta exercise of align_seq practical
	TestOaksData.csv: Input file for oaks_debugme.py practical 
	sequences.csv: Input file for align_seqs practical part 2
	bodymass.csv: Test file for practicals
	testcsv.csv: Test input file for csv practical 


 #### Sandbox: 
 A practice directory for rough work and practice  
 
 ## Additional notes on packages required for scripts: 
- align_seqs_better.py: _Requires import sys and os_
- oaks_debugme.py: _Requires import csv, sys, doctest and re_
- sysargv.py: _Requires import sys_
- control_flow.py: _Requires import sys_
- align_seqs.py: _Requires import sys_
- align_seqs_fasta.py: _Requires import sys and os_
- basic_io3.py: _Requires import pickle_
- basic_csv.py: _Requires import csv_
- boilerplate.py: _Requires import sys_
- debugme.py: _Requires import ipdb_

    
    
    
    