#!/bin/bash
#PBS -l walltime=00:10:00
#PBS -l select=1:ncpus=1:mem=1gb
module load anaconda3/personal 
echo "R is about to run"
R --vanilla < $/rdsgpfs/general/user/ead19/home/cluster_run.R
mv EDIteration* $/rdsgpfs/general/user/ead19/home 
echo "R has finished running"
