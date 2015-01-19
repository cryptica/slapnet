#!/usr/bin/bash

benchmarks=( "Lamport" "Dijkstra" "Peterson" "Szymanski" "LeaderElectionDKR82" "LeaderElectionCR79" "Snapshot" )
suffix=""
n=6
m=1

for benchmark in ${benchmarks[@]}; do
    echo "Runnng benchmark $benchmark"

    bf="${benchmark}/benchmark${suffix}.out"

    echo "n user system memory" >$bf

    for i in $(seq 2 $n); do
        net="${benchmark}/n${i}${suffix}.pnet"
        echo "Testing net $net"
        for j in $(seq 1 $m); do
            echo -n "$i " >>$bf
            /usr/bin/time -f "%U %S %M" -a -o $bf ../../slapnet $net -vv > "${net}.out"
        done
    done

    echo
done
