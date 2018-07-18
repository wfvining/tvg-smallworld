#!/bin/bash

for id in `seq 0.1 0.05 0.9`
do
    stack exec tvg-eval ballistic/ballistic-origin-${id}.conf ../data >> ./origin_success.txt &
    stack exec tvg-eval ballistic/ballistic-square-${id}.conf ../data >> ./square_success.txt &
    stack exec tvg-eval ballistic/ballistic-uniform-${id}.conf ../data >> ./uniform_success.txt &
    wait
done