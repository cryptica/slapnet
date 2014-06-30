#!/usr/bin/bash

#folder="LeaderElectionDKR82"
folder="Snapshot"
suffix="-comm"
n=100
m=2
bf="${folder}/benchmark${suffix}.out"

echo "n user system memory" >$bf

for i in $(seq 1 $n); do
    net="${folder}/n${i}${suffix}.pnet"
    echo "Testing net $net"
    for j in $(seq 1 $m); do
        echo -n "$i " >>$bf
        /usr/bin/time -f "%U %S %M" -a -o $bf ../slapnet $net > "${net}.out"
    done
done
