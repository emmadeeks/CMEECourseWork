#!/usr/bin/env python3 
#### Add shebang to make the programme excuable from a different call point
### R has an R shebang at the top of its script as well
import subprocess
## Call the R shebang to make python know its changing languages 
# Shell = True means to execute it within the python shell 
## First define the language environment that the script is to run in 
# then defines the specific R script that the script is to run. 
subprocess.call("/usr/bin/env Rscript  fmr.R", shell = True)