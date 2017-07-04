all: proj_interim.pdf

proj_interim.pdf: proj_interim.tex report.bib
	pdflatex proj_interim
	bibtex proj_interim
	pdflatex proj_interim
	pdflatex proj_interim

.PHONY: clean


count: proj_interim.pdf
	pdftotext $< - | wc -w

clean:
	rm -f proj_interim.aux
	rm -f proj_interim.log
	rm -f proj_interim.pdf
	rm -f proj_interim.bbl
	rm -f proj_interim.blg
	rm -f proj_interim.dvi
