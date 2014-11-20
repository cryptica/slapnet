#!/usr/bin/bash

#folder="LeaderElectionDKR82"
folder="LeaderElectionCR79"
#folder="Snapshot"
suffix="-ord"
#suffix="-comm"
n=100
m=3
bf="${folder}/benchmark${suffix}.out"

echo "n user system memory" >$bf

for i in $(seq 10 10 100); do
    net="${folder}/n${i}${suffix}.pnet"
    echo "Testing net $net"
    for j in $(seq 1 $m); do
        echo -n "$i " >>$bf
        /usr/bin/time -f "%U %S %M" -a -o $bf ../../slapnet $net > "${net}.out"
    done
done
