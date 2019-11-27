""" Illustrates hhow to use os and subprocess, utilises subprocess.os.walk"""


import subprocess
import os 


# Use the subprocess.os module to get a list of files and directories 
# in your ubuntu home directory 

dirlist = subprocess.os.listdir("/Users/emmadeeks")

#################################
#~Get a list of files and 
#~directories in your home/ that start with an uppercase 'C'

FilesDirsStartingWithC = [filename for filename in os.listdir('/Users/emmadeeks') if filename.startswith("C")]
print(FilesDirsStartingWithC)
# Get the user's home directory.
home = subprocess.os.path.expanduser("~")

# Use a for loop to walk through the home directory.
for (root, dirs, files) in subprocess.os.walk('/Users/emmadeeks'):
    for name in files: 
        (base, ext) = os.path.splitext(name)
        files = base

  
#################################
# Get files and directories in your home/ that start with either an 
# upper or lower case 'C'

FilesDirsStartingWithCorc = [filename for filename in os.listdir('/Users/emmadeeks/Desktop') if filename.startswith("C") or filename.startswith("c")]
print(FilesDirsStartingWithCorc)


