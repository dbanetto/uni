all: essay.pdf

%.pdf: %.tex %.bib
	pdflatex essay
	bibtex essay
	while grep 'Rerun to get ' $*.log ; do pdflatex $< ; done

tidy:
	rm essay.aux
	rm essay.blg
	rm essay.bbl
	rm essay.log

submit: all
	cp -f essay.pdf swen301-a1-300313764.pdf

clean: tidy
	rm essay.pdf
