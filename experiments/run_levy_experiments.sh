#!/bin/bash

echo "Density alpha maxStep averageCorrectness"
for initial_density in `seq 0.0 0.01 1.001`
do
    for alpha in 0.2 0.4 0.8 1.2 1.9
    do
        stack exec tvg-eval levy-127/levy-${alpha}_${initial_density}.conf ../data &
    done
    wait
done
