all: report.pdf

%.pdf: %.md
	pandoc -V geometry:margin=1in $< -o $@

clean:
	rm -f report.pdf
