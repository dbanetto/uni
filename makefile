all: report.pdf

%.pdf: %.tex %.bib
	pdflatex report
	bibtex report
	while grep 'Rerun to get ' $*.log ; do pdflatex $< ; done

tidy:
	rm report.aux
	rm report.blg
	rm report.bbl
	rm *.log

submit: all
	cp -f report.pdf swen301-a3-300313764.pdf

clean: tidy
	rm report.pdf
