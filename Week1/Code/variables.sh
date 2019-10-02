#!/bin/bash 
#shows the use of some variables 

MyVar='some string'
echo 'the current value of the variable is' $MyVar
echo 'Please enter a new string'
read MyVar
echo 'the current value of the variable is' $MyVar 

##Reading mutiple values 
echo 'Enter two numbers separated by space(e)'
read a b 
echo 'you entered' $a 'and' $b '. Their sum is:'
mysum=`expr $a + $b`
echo $mysum