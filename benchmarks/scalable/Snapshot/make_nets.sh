#!/usr/bin/bash

n=$1
comms=(comm nocomm)

for c in ${comms[@]}; do
    for i in $(seq 1 $n); do
        echo "Creating net for n = $i with $c"
        ./make_net.py $i $c > "n$i-$c.pnet"
    done
done
