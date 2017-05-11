PDF:=Assignment\ 2\ David\ Barnett.pdf

$(PDF): report.md report.bib
ifdef CITE
	pandoc -s -S --bibliography report.bib --filter pandoc-citeproc --csl ieee.csl $< -o "$@"
else
	@echo "WARNING NO CITATION"
	pandoc -s -S $< -o "$@"
endif

count: $(OUT)
ifdef PDFTOTEXT
	pdftotext "$<" - | wc -w
else
	@echo 'pdftotext not installed'
endif

clean:
	rm -f report.pdf
