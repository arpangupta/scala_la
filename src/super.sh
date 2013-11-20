#!/bin/bash
 
for i in 1 2 4 8 16
do

	for c in 1 2 3 4 5 6 7 8 9 10
	do
	v=$((c * 500))
	echo "$i threads Input Size $v"
	time scala -J-Xmx8g ParallelMatrix ${i} ${v} ${v} mat_add >out
	done
done
