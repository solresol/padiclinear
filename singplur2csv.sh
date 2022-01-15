#!/bin/sh

for SOURCEFILE in $*
do
    OUTPUTFILE=results/$(basename $SOURCEFILE).txt
    rm -f $OUTPUTFILE
    for i in 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
    do
	./singular2plural -n $i $SOURCEFILE >> $OUTPUTFILE
    done
done
