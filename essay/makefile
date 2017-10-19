all: essay.pdf

essay.pdf: essay.tex essay.bib
	pdflatex essay
	bibtex essay
	pdflatex essay
	pdflatex essay

clean:
	rm -f *.aux *.blg *.bbl *.log
	rm -f essay.pdf
