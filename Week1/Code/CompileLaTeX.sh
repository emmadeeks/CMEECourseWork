#!/bin/bash
pdflatex $1.tex
pdflatex $1.tex
bibtex $1
pdflatex $1.tex
pdflatex $1.tex 
evince $1.pdf &

#Cleanup 
rm *~
rm *.aux
rm *dvi
rm *.nav
rm *.out
rm *.snm
rm *.toc
