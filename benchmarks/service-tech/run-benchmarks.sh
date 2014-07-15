#!/bin/bash

#benchmarks=( 'ibm-soundness' 'sap-reference' )
#benchmarks=( 'sap-reference' )
benchmarks=( 'ibm-soundness' )
extensions=( 'pnet' 'tpn' 'lola' )
executable='../../slapnet'

for benchmark in ${benchmarks[@]}; do
  benchmark_dir="$benchmark"
  >$benchmark_dir/positive-slapnet.list
  >$benchmark_dir/dontknow-slapnet.list
  >$benchmark_dir/timeout-slapnet.list
  >$benchmark_dir/error-slapnet.list
  for ext in ${extensions[@]}; do
    for file in `find $benchmark_dir -name "*.$ext"`; do
      T="$(date +%s%N)"
      if [ -e $benchmark_dir/final_marking.lola.fin.task ]; then
          final=$benchmark_dir/final_marking.lola.fin.task
      elif [ -e $file.fin.task ]; then
          final=$file.fin.task
      fi
      (
        set -o pipefail;
        timeout 60 $executable --$ext --termination-by-reachability $file -o $file.terminating.lola | tee $file.out
      )
      result=$?
      T=$(($(date +%s%N)-T))
      if [[ result -eq 0 ]]; then
          list='positive'
      elif [[ result -eq 2 ]]; then
          list='dontknow'
      elif [[ result -eq 124 || result -eq 137 ]]; then
          list='timeout'
      else
          list='error'
      fi
      echo $T $file >>$benchmark_dir/$list-slapnet.list
    done
  done
done
