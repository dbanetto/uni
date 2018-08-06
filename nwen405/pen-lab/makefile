all: report.pdf

clean:
	rm -f report.pdf

%.pdf: %.md
	pandoc -s -S --bibliography report.bib --filter pandoc-citeproc --csl apa.csl -V geometry:margin=1in $< -o $@
