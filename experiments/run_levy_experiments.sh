#!/bin/bash

echo "# initialDensity alpha maxStep averageCorrectness"
for initial_density in 0.1 0.2 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65 0.7 0.8 0.9
do
    for alpha in `seq 0.2 0.2 2.0`
    do
        stack exec tvg-eval levy-${alpha}_${initial_density}.conf ../data &
    done
    wait
done
