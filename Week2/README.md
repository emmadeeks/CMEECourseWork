CMEE Coursework Repository- readme.txt 

Week2 focuses on the basics of Python; Topics covered include:
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
    - Using python to patch together data analysis and/or numerical        simulation work flows

Files included:
Week2 
    Code 
        align_seqs.py           align_seqs_better.py    boilerplate.py 
        align_seqs_fasta.py     debugme.py              test_control_flow.py
        basic_csv.py            dictionary.py           loops.py  test_control_flow.pyc   basic_io3.py            oaks.py
        basic_io1.py            lc1.py                  tuple.py
        basic_io2.py            lc2.py                  using_name.py
        cfexercises1.py         cfexercises2.py         oaks_debugme.py
        control_flow.py         scope.py                sysargv.py
    Data
        407228326.fasta         407228412.fasta         TestOaksData.csv
        testcsv.csv             bodymass.csv            sequences.csv
         
    results



File functions: 

Week2 
    Code
        basic_io1.py: 
            Function: Returns the contents of 'test.txt' in different formats e.g. excludes second lines

        basic_io2.py: 
            Function: Saves the elements of a list to a file and adds a new line when saving  

        basic_io3.py: 
            Function: Uses pickle to store an object from a dictionary before loading data again 
            ******** Requires import of pickle *********  

        basic_csv.py: 
            Function: Reads in the testcsv.csv file containing phylogeny information and appends a tuple with the name of a species. 
            Writes a seperate file containing only species name and body mass
            ******** Requires import csv *********

        loops.py:  
            Function: A series of for and while loops 

        cfexercises2.py:
            Function: A series of conditionals containing for and while loops 

        oaks.py: 
            Function: A example exercise using for loops and list comprehensions to find taxa that are oak trees from a list of species. 
                        
        scope.py: 
            Function: A collection of three blocks of code illustrating variable scope.
            
        boilerplate.py: 
            Function: Example exercise on writing python programmes. Purpose of example programme is to print 'This is a boilerplate'). Illustrates how programmes are laid out and uses of "main" arguments as well as docstrings.
            ******* Requires import sys ****** 
        
        using_name.py: 
            Function: An if else statement within a python programme printing whether the programme is being imported from another module of being run by itself. 

        sysargv.py: 
            Function: Exercise of the sys.argv module which contains the names of the argument variables in the current script. 
            ***** Requires import sys *******

        control_flow.py: 
            Function: Similar to boilerplate.py in structure as its a python programme but adds docstrings and a series of functions 
            ****** Requires import sys *******

        lc1.py: PRACTICAL 1 PART 1 
            Function: Uses list comprehensions and for loops to create three lists containing latin names, common names and mean body masses for each species of birds in a given list of birds
        
        lc2.py: PRACTICAL 1 PART 2
            Function: Uses list comprehensions and for loops to create list of month, rainfall tuples when the amount of rain was; Greater than 100mm and Less than 50mm from a list of rainfall averages 

        dictionary.py: PRACTICAL 1 PART 3
            Function: Populates a dictionary to map order names to sets of taxa

        tuple.py: PRACTICAL 1 PART 4
            Function: Prints the latin name, common name and mass of a list of birds on seperate lines

        cfexercises1.py: PRACTICAL 2
            Function: Conditionals exercise containing 6 different foo_x calculation functions. Later modified to make a module that tests the foo_x functions as well as runs test arguments 

        test_control_flow.py: 
            Function: Function exemplifying the use of control elements

        align_seqs.py: PRACTICAL 3 PART 1
            Function: Converts the initial 'align_seqs.py' script to a pprogramme that takes DNA sequences as an input from a single external file and saves the best alignment along with the score into a text file. 
            Input: None 
            Output: Text file with best aligment 
            ******* Requires import sys *********

        align_seqs_fasta.py: PRACTICAL 3 PART 2
            Function: Same basic script as 'align_seqs.py' but aligns fasta files. option to manually input fasta files to be aligned or to have a default where there are no inputs. 
            Inputs: Optional. Two fasta files 
            Outputs: Text file with best aligment 
            ******* Requires import sys and os *********

        align_seqs_better.py: PRACTICAL 3 PART 3
            Function: Runs alignments on files and inputs the highest alignments into a single text file as oppose to one the last highest alignment the programme finds. 
            Inputs: Optional. Two fasta files 
            Outputs: Text file with the highest alignments and their associated scores
            ******* Requires import sys and os *********

        oaks_debugme.py PRACTICAL 4 PART 1
            Function: A script with a bug that was corrected with debugging. Takes as input a file with tree species and returns a file with only oak species found.
            Note: must enter species name exactly. 
            Input: File with species 
            Output: Text file with only oak species. 
            ********* Requires import of csv, sys, doctest and re *********

        
    Data
    407228326.fasta: Input file for fasta exercise of align_seq practical   
    407228412.fasta: Input file for fasta exercise of align_seq practical
    TestOaksData.csv: Input file for oaks_debugme.py practical 
    sequences.csv: Input file for align_seqs practical part 2
    bodymass.csv: Test file for practicals
    testcsv.csv: Test input file for csv practical 