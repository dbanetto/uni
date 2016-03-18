all: 
	pdflatex essay
	bibtex essay
	pdflatex essay
	pdflatex essay

tidy:
	rm essay.aux
	rm essay.blg
	rm essay.bbl
	rm essay.log

clean: tidy
	rm essay.pdf
