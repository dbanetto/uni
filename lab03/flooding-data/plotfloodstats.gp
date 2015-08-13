set title "Comparison of 3 flooding protocols"
set xlabel "Running time (seconds)"
set ylabel "Delivery efficiency (%)"

set grid
set yrange [0:]

map = "WORLD.MAP"

set terminal latex
set output sprintf("%s-plot.tex", map)
plot	\
    sprintf("data/result-%s.flood1", map)		title "flooding1" with linespoints, \
    sprintf("data/result-%s.flood2", map)		title "flooding2" with linespoints, \
    sprintf("data/result-%s.flood3", map)		title "flooding3" with linespoints
