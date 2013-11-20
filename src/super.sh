#!/bin/bash
 
for i in 1 2 4 8
do

	for c in {0..20..2}
	do
	v=$((c * 100))
	echo "$i threads Input Size $v"
	time scala ParallelGaussElemination ${i} ${v} >out
	done
done
