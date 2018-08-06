all:  report.pdf

report.pdf: report.tex res/NEWZEALAND-MAP-plot.png res/WORLD-MAP-hops-plot.png
	pdflatex report.tex

#report.pdf: report.md res/NEWZEALAND.MAP-plot.png res/WORLD.MAP-hops-plot.png
#     pandoc report.md -o report.pdf

res/NEWZEALAND-MAP-plot.png: flooding-data/getfloodstats
	(cd flooding-data ; sh getfloodstats )

res/WORLD-MAP-hops-plot.png: flooding-data/getfloodstats8hop
	(cd flooding-data ; sh getfloodstats8hop )

