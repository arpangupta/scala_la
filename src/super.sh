#!/bin/bash
 
for i in 1 2 4 8
do

	for c in 1 2 5 7 10 15 20
	do
	v=$((c * 100))
	echo "$i threads Input Size $v"
	time scala ParallelGaussElemination ${i} ${v} >out
	done
done
