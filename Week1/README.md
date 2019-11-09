# CMEE Coursework Repository- README.md

## Week1

- Unix Week:  Learnt the basics of UNIX including shell scripting, key commands such as grep and running functions. 

### Code:

| Script       | Function     | Input     | Output    |
| :------------- | :----------: | -----------: |-----------: |
|  `boilerplate.sh` | Shell script that recites 'This is a shell script!' when run   | None    | 'This is a shell script!    |
|  `tabtocsv.sh` | Shell script to substitute all tabs with commas.    | File for the script to be run on    | File with commas instead of tabs |    
|  `variables.sh` | Shell script to show the use of variables.    | 1 string and two variables.    | Value of string and adds the two variables together    |
|  `MyExampleScript.sh` | Example script   | No input | 'Hello' and the Username of the user twice when run.    |
|  `CountLines.sh` | Counts lines in a file   | File directory | Number of lines   |
|  `ConcatenateTwoFiles.sh` | Concatenates two files   | Var1 Var2: Two file paths to be merged and Var3: file to be merged to | Merged files in var 3   |
|  `tiff2png.sh` | Converts a tiff file to a png  | tiff file | png file   |
|  `csvtospace.sh` | Takes comma separated values and converts it to space separated values whilst not changing the input file.   | csv datafile    | Separate file that is space separated   |
|   `FirstExample.tex` | Latex file template  | No input    | Latex document   |
|   `CompileLaTeX.sh` | Compiles latex with bibtex  | File that needs to be compiled with bibtex    | Latex document   |
|   `FirstBiblio.bib` | The bibliography of a paper   | Bibliography of desired paper    | Bibliography of paper  |
|   **`UnixPrac1.txt`** | The first practical submission of the CMEE Course Work covering the chapter in UNIX  | fasta files    | How many lines in each fasta file; Prints everything in E.coli.fasta file except first lines; Count the sequence length of genome in fasta file; Count matches of "ATGC" in genome; AT/GC ratio of E.coli.fasta file  |

### Data: 

Files

	407228326.fasta: Input file for fasta exercise    
	407228412.fasta: Input file for fasta exercise 
	E.coli.fasta: Input file for fasta exercise 
	Temperatures: Input files for the shell scripting exercises 
                1800.csv
                1801.csv
                1802.csv
                1803.csv
 	spawannxs.txt: Input files for practice exercises in Unix and shell scripting 
 
 #### Sandbox: 
 A practice directory for rough work and practice 
 
 ## Additional notes on packages required for scripts: 
 tiff2png.sh: _Need to have installed imagemagick on unix system_
