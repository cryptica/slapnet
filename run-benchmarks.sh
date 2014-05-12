#!/bin/bash

benchmarks=( 'found-in-mist-repo' 'given-by-daniel-kroening' 'ic3-soter' )

for benchmark in ${benchmarks[@]}; do
  benchmark_dir="benchmarks/$benchmark"
  >$benchmark_dir/positive-slapnet.list
  >$benchmark_dir/dontknow-slapnet.list
  >$benchmark_dir/negative-slapnet.list
  >$benchmark_dir/timeout-slapnet.list
  >$benchmark_dir/error-slapnet.list
  >$benchmark_dir/timing-slapnet.log
  for pnet_file in `find $benchmark_dir -name "*.pnet"`; do
    T="$(date +%s%N)"
    (
      set -o pipefail;
      timeout 60 ./src/Main $pnet_file | tee $pnet_file.out
    )
    result=$?
    T=$(($(date +%s%N)-T))
    if [[ result -eq 0 ]]; then
        list='positive'
    elif [[ result -eq 1 ]]; then
        list='negative'
    elif [[ result -eq 2 ]]; then
        list='dontknow'
    elif [[ result -eq 124 || result -eq 137 ]]; then
        list='timeout'
    else
        list='error'
    fi
    echo $T $pnet_file >>$benchmark_dir/$list-slapnet.list
  done
done
