#!/bin/sh

perl day12.pl < day12.txt > day12.dot
echo -n "Part 1: "
ccomps -X 0 day12.dot | gc -n
echo -n "Part 2: "
gc -c day12.dot
