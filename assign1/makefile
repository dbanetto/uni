report.pdf: report.md report.bib
	pandoc -s -S --bibliography report.bib --filter pandoc-citeproc --csl ieee.csl $< -o $@

clean:
	rm -f report.pdf
