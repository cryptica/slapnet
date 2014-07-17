#!/bin/bash

#benchmarks=( 'ibm-soundness' 'sap-reference' )
benchmarks=( 'sap-reference' )
#benchmarks=( 'ibm-soundness' )
extensions=( 'pnet' 'tpn' 'lola' )
executable='/home/philipp/local/sara-1.0/src/sara'

for benchmark in ${benchmarks[@]}; do
  benchmark_dir="$benchmark"
  >$benchmark_dir/positive-sara.list
  >$benchmark_dir/negative-sara.list
  >$benchmark_dir/timeout-sara.list
  >$benchmark_dir/error-sara.list
  for ext in ${extensions[@]}; do
    for file in `find $benchmark_dir -name "*.$ext"`; do
      T="$(date +%s%N)"
      pushd .
      cd $(dirname $file)
      base=$(basename $file)
      echo "testing $file with $base"
      (
        set -o pipefail;
        timeout 60 $executable -i $base.terminating.sara 2>&1 | tee $base.out
      )
      popd
      result=$?
      ryes=$(grep "SOLUTION" $file.out)
      rno=$(grep "INFEASIBLE" $file.out)
      T=$(($(date +%s%N)-T))
      if [[ -n $ryes ]]; then
          list='positive'
      elif [[ -n $rno ]]; then
          list='negative'
      elif [[ result -eq 124 || result -eq 137 ]]; then
          list='timeout'
      else
          list='error'
      fi
      echo $T $file >>$benchmark_dir/$list-sara.list
    done
  done
done
