#!/bin/bash

#benchmarks=( 'service-tech/ibm-soundness' 'service-tech/sap-reference' )
#benchmarks=( 'cav-benchmarks/mist' 'cav-benchmarks/wahl-kroening' 'cav-benchmarks/soter' 'cav-benchmarks/bug_tracking' 'cav-benchmarks/bug_tracking' )
#benchmarks=( 'cav-benchmarks/bug_tracking' )
#benchmarks=( 'cav-benchmarks/medical' )
benchmarks=( 'cav-benchmarks/mist' )

extensions=( 'pnet' 'tpn' 'lola' 'spec' )
executable='../slapnet'

#2 minutes
time_soft=$(expr 2 \* 60)
time_hard=$(expr $time_soft + 60)
# 2 GB
mem_soft=$(expr 2 \* 1048576)
mem_hard=$(expr $mem_soft + 1024)

properties=( 'safety' 'safe' 'deadlock-free' 'terminating' )
property_options=( ''
    '--no-given-properties --safe'
    '--no-given-properties --deadlock-free'
    '--no-given-properties --terminating'
)
# TODO: separate input, output and result files

for benchmark in ${benchmarks[@]}; do
    benchmark_dir="$benchmark"
    for (( propi=0;propi<${#properties[@]};propi++)); do
        prop=${properties[$propi]}
        prop_options=${property_options[$propi]}
        >$benchmark_dir/$prop-positive-slapnet.list
        >$benchmark_dir/$prop-dontknow-slapnet.list
        >$benchmark_dir/$prop-timeout-slapnet.list
        >$benchmark_dir/$prop-error-slapnet.list
        for ext in ${extensions[@]}; do
            for file in $(find $benchmark_dir -name "*.$ext"); do
                # TODO: use /usr/bin/time to measure resources
                timing="$(date +%s%N)"
                (
                    ulimit -S -t $time_soft
                    ulimit -H -t $time_hard
                    ulimit -S -v $mem_soft
                    ulimit -H -v $mem_hard
                    set -o pipefail
                    $executable $prop_options --$ext $file 2>&1 | tee $file.$prop.out
                )
                result=$?
                timing=$(($(date +%s%N)-timing))
                if [[ result -eq 0 ]]; then
                    list='positive'
                elif [[ result -eq 2 ]]; then
                    list='dontknow'
                elif [[ result -gt 127 ]]; then
                    list='timeout'
                    timing=$result
                else
                    list='error'
                    timing=$result
                fi
                echo $timing $file >>$benchmark_dir/$prop-$list-slapnet.list
            done
        done
    done
done
