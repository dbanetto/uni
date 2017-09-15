OUT:=proj_report_outline
PDF:=$(OUT).pdf
TEX:=$(wildcard *.tex)
BIB:=$(wildcard *.bib)

all: $(PDF)

$(PDF): $(TEX) $(BIB)
	pdflatex $(OUT)
	bibtex $(OUT)
	pdflatex $(OUT)
	pdflatex $(OUT)
	@echo "complete!"

clean:
	rm -f *.aux *.log *.bbl *.blg *.dvi *.toc *.lof *.out
	rm -f $(PDF)

view: $(PDF)
	evince $< &

count: $(PDF)
	pdftotext $(PDF) - | wc -w

