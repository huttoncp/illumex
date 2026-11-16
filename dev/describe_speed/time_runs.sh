#!/bin/sh
## Times two versions of the package against each other, alternating one R
## process per version per repeat, and summarises the medians.
##   sh dev/describe_speed/time_runs.sh <tree before> <tree after> <rows> <reps> <out.csv>
## Run from the package root (the "after" tree) with R on the PATH.
before=$1; after=$2; rows=$3; reps=$4; out=$5
rm -f "$out"
i=1
while [ "$i" -le "$reps" ]; do
  Rscript dev/describe_speed/time_describe.R "$before" before "$rows" "$out"
  Rscript dev/describe_speed/time_describe.R "$after" after "$rows" "$out"
  i=$((i + 1))
done
Rscript dev/describe_speed/time_summary.R "$out"
