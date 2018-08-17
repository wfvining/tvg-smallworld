#!/bin/bash

run_cr()
{
    for initial_density in `seq 0.00 0.01 1.001`
    do
        stack exec tvg-eval cr-${1}/levy-0.2_${initial_density}.conf ../../data/
    done >cr-${1}_results.txt
}

for comm_radius in 1 2 4 7 14 25 32 63
do
    run_cr $comm_radius &
done
wait