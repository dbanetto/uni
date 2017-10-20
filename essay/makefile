all: essay.pdf

essay.pdf: essay.tex essay.bib
	pdflatex essay
	bibtex essay
	pdflatex essay
	pdflatex essay

wc: essay.pdf
	pdftotext essay.pdf - | wc -w

count:
	texcount.pl essay.tex

clean:
	rm -f *.aux *.blg *.bbl *.log
	rm -f essay.pdf
