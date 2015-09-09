all: report.pdf design.pdf

report.pdf: report.tex res
	pdflatex -shell-escape $< -o $@
	rm -f report.aux report.log


design.pdf: design.tex
	pdflatex -shell-escape $< -o $@
	rm -f design.aux design.log

res: ../res/K5-MAP-plot.png

../res/K5-MAP-plot.png: ../dist/getstats.sh ../dist/*.c
	(cd ../dist/ ; sh getstats.sh )

clean:
	rm -r ../res/*
	rm -f report.pdf
	rm -f design.pdf
