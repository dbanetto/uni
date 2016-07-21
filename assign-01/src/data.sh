#!/bin/sh

SIZES=(10 100 1000 10000 100000 1000000 10000000)
THREADS=(1 2 4 8 16 32 64 128 256 512 1024)

for N in "${THREADS[@]}"
do
    for M in "${SIZES[@]}"
    do
        echo "$N threads $M items"
        java -cp . ArraySum $N $M | grep "threads.*Sum" | sed -e "s/^.*: \([0-9]*\) ms.*$/\1/" | xargs | sed -e "s/ /,/g" | sed -e "s/^/$N,$M,/" >> data.csv
    done
done

