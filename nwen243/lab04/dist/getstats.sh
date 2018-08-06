#!/bin/sh
#
DURATION="10mins"
EVERY="10"			# seconds
rm -rf data/
mkdir data


for map in "NEWZEALAND.MAP" "K5.MAP" "NZ AUS.MAP" "STAR.MAP"
do
    echo "#include \"maps/${map}\"" > map.h
    for algo in "flooding1" "flooding2" "flooding3" "routing"
    do
        cnet -W -q -T -e $DURATION -s -f ${EVERY}secs -N "$algo"	| \
            grep 'Efficiency'						| \
            cut -d: -f 2						| \
            awk "{ printf(\"%d %s\n\", ++i * $EVERY, \$1); }" > data/result-$map.$algo
    done

    map_low=$(echo ${map} | sed -e 's/.MAP/\-MAP/' | sed -e 's/ /-/')
    echo ${map_low}

    gnuplot <</EOF
map = "${map}"

set title sprintf("Comparison of 4 routing protocols - %s", map)
set xlabel "Running time (seconds)"
set ylabel "Delivery efficiency (%)"

set grid
set yrange [0:]


set terminal png size 800,600 enhanced font "Sans,16"
set output "../res/${map_low}-plot.png"
plot	\
    sprintf("data/result-%s.flooding1", map)		title "flooding1" with linespoints, \
    sprintf("data/result-%s.flooding2", map)		title "flooding2" with linespoints, \
    sprintf("data/result-%s.flooding3", map)		title "flooding3" with linespoints, \
    sprintf("data/result-%s.routing", map)		title "DV Routing" with linespoints
/EOF
done
