""" Illustrates hhow to use os and subprocess, utilises subprocess.os.walk"""
# Imports the appropirate packages 
import subprocess
import os 


# Use the subprocess.os module to get a list of files and directories 
# in your ubuntu home directory 

# Uses the listdir function of subprocess and set the directory to home directory which for mac users is the username
dirlist = subprocess.os.listdir("/Users/emmadeeks")

#################################
#~Get a list of files and 
#~directories in your home/ that start with an uppercase 'C'

# Uses a if statement and the filename.startswith function to find files that begin with C
FilesDirsStartingWithC = [filename for filename in os.listdir('/Users/emmadeeks') if filename.startswith("C")]
# Then prints it 
print(FilesDirsStartingWithC)
# Get the user's home directory.
home = subprocess.os.path.expanduser("~")

# Use a for loop to walk through the home directory.
for (root, dirs, files) in subprocess.os.walk('/Users/emmadeeks'):
    # goes through the filenames and splits the base from the extension so only the base is returned 
    for name in files: 
        (base, ext) = os.path.splitext(name)
        files = base

  
#################################
# Get files and directories in your home/ that start with either an 
# upper or lower case 'C'
# If statement that prints any files in the specified directory that starts with either a C or a c 
FilesDirsStartingWithCorc = [filename for filename in os.listdir('/Users/emmadeeks') if filename.startswith("C") or filename.startswith("c")]
print(FilesDirsStartingWithCorc)


