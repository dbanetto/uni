CITE := $(shell command -v pandoc-citeproc 2> /dev/null)

report.pdf: report.md report.bib
ifdef CITE
	pandoc -s -S --bibliography report.bib --filter pandoc-citeproc --csl ieee.csl $< -o $@
else
	echo "WARNING NO CITATION"
	pandoc -s -S $< -o $@
endif

clean:
	rm -f report.pdf
