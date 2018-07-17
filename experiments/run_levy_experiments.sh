#!/bin/bash

echo "# initialDensity alpha maxStep averageCorrectness"
for initial_density in `seq 0.1 0.05 0.9`
do
    for alpha in `seq 0.2 0.2 2.0`
    do
        stack exec tvg-eval levy/levy-${alpha}_${initial_density}.conf ../data &
    done
    wait
done
