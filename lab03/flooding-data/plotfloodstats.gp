#
set title "Comparison of 3 flooding protocols"
set xlabel "Running time (seconds)"
set ylabel "Delivery efficiency (%)"
#
set grid
set yrange [0:]

list(i)=word(system(ls -1B *.MAP),i)

do for [map in list] {

plot	\
	sprintf("%s-result.flood1", map)		title "flooding1" with linespoints, \
	sprintf("%s-result.flood2", map)		title "flooding2" with linespoints, \
	sprintf("%s-result.flood3", map)		title "flooding3" with linespoints

}
pause -1
