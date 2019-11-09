# CMEE Coursework Repository- README.md

## Week1

- Unix Week:  Learnt the basics of UNIX including shell scripting, key commands such as grep and running functions. 

Code:

| Script       | Function     | Input     | Output    |
| :------------- | :----------: | -----------: |-----------: |
|  boilerplate.sh | Shell script that recites 'This is a shell script!' when run   | None    | 'This is a shell script!    |
|  tabtocsv.sh | Shell script to substitute all tabs with commas.    | File for the script to be run on    | File with commas instead of tabs |    
|  variables.sh | Shell script to show the use of variables.    | 1 string and two variables.    | Value of string and adds the two variables together    |
|  MyExampleScript.sh | Example script   | No input | 'Hello' and the Username of the user twice when run.    |
|  CountLines.sh | Counts lines in a file   | File directory | Number of lines   |
|  ConcatenateTwoFiles.sh | Concatenates two files   | Var1 Var2- Two file paths to be merged and Var3-file to be merged to | Merged files in var 3   |
|  tiff2png.sh | Converts a tiff file to a png  | tiff file | png file   |
|  csvtospace.sh | Takes comma seperated values and converts it to space seperated values whilst not changing the input file.   | csv datafile    | Seperate file that is space seperated   |



| Bash scripts       |      |      |
| :------------- | :----------: | -----------: |
| boilerplate.sh | tabtocsv.sh   | variables.sh    |
| MyExampleScript.sh   | CountLines.sh |
| ConcatenateTwoFiles.sh   | tiff2png.sh |
| csvtospace.sh   | **UnixPrac1.txt** |

| Script | Function | Input | Output
| ----------- | ----------- |
|  boilerplate.sh | Shell script that recites 'This is a shell script!' when run | None | 'This is a shell script! |
    
    Code 
        boilerplate.sh      tabtocsv.sh        variables.sh       MyExampleScript.sh 
        CountLines.sh       ConcatenateTwoFiles.sh                tiff2png.sh
        csvtospace.sh       UnixPrac1.txt
    Data
        407228326.fasta     407228412.fasta    E.coli.fasta        spawannxs.txt
        Temperature:    
                    1800.csv    1801.csv       1802.csv         1803.csv 
    Sandbox 
        test.txt        test.txt.csv        ListRootDir.txt
        TestWild



File functions: 
Week1 
    Code
        boilerplate.sh: 
            Function: Shell script that recites 'This is a shell script!' when run

        tabtocsv.sh: Shell script to substitute all tabs with commas. 
            Input: File for the script to be run on
            Output: File with commas instead of tabs 

        variables.sh: Shell script to show the use of variables. 
            Input: 1 string and two variables. 
            Output: Value of string and adds the two variables together  

        MyExampleScript.sh: Bash script that outputs 'Hello' and the Username of the user twice when run. 

        CountLines: Bash script that counts the number of lines in a file. 
            Input: File directory 

        ConcatenateTwoFiles: Bash script that concatenates the contents of two files. 
            Input: Var1 Var2; Two file paths to be merged and Var3; file to be merged to  

        tiff2png.sh: Bash script that converts a tiff file to a png
                     ********* Need to have installed imagemagick on unix system. ************
            Input: tiff file
            Output: png file

        csvtospace.sh: Takes comma seperated values and converts it to space seperated values whilst not changing the input file. 
            Input: csv datafile    
            Output: Seperate file saved under different name 
                        
        UnixPrac1.txt: The first practical submission of the CMEE Course Work covering the chapter in UNIX 
            Input: fasta files  
            Output: How many lines in each fasta file
                    Prints everything in E.coli.fasta file except first lines
                    Count the sequence length of genome in fasta file 
                    Count matches of "ATGC" in genome 
                    AT/GC ratio of E.coli.fasta file
        
        FirstExample.tex: Latex file template 
        
        FirstBiblio.bib: The bibliography of a paper 

        CompileLaTeX.sh: Compiles latex with bibtex
            Input: File that needs to be compiled with bibtex
        
    Data
    407228326.fasta: Input file for fasta exercise    
    407228412.fasta: Input file for fasta exercise 
    E.coli.fasta: Input file for fasta exercise 
    Temperatures: Input files for the shell scripting excercises 
                1800.csv
                1801.csv
                1802.csv
                1803.csv
    spawannxs.txt: Input files for practice excercises in Unix and shell scripting 
    
    Sandbox: A practice directory for work practice.

