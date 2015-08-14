all: report.pdf

report.pdf: report.md res/*.png
	pandoc report.md -o report.pdf

res/NEWZEALAND.MAP-plot.png: flooding-data/getfloodstats
	(cd flooding-data ; sh getfloodstats )

res/WORLD.MAP-hops-plot.png: flooding-data/getfloodstats8hop
	(cd flooding-data ; sh getfloodstats8hop )

