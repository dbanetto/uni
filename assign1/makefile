CITE := $(shell command -v pandoc-citeproc 2> /dev/null)
PDFTOTEXT := $(shell command -v pdftotext 2> /dev/null)
OUT:=report.pdf

$(OUT): report.md report.bib
ifdef CITE
	pandoc -s -S --bibliography report.bib --filter pandoc-citeproc --csl ieee.csl $< -o $@
else
	@echo "WARNING NO CITATION"
	pandoc -s -S $< -o $@
endif

count: $(OUT)
ifdef PDFTOTEXT
	pdftotext $< - | wc -w
else
	@echo 'pdftotext not installed'
endif

clean:
	rm -f report.pdf
