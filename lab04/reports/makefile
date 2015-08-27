all: report.pdf design.pdf

report.pdf: report.tex
	pdflatex -shell-escape $< -o $@

design.pdf: design.tex
	pdflatex -shell-escape $< -o $@

clean:
	rm -f report.pdf report.aux report.log
	rm -f design.pdf design.aux design.log
