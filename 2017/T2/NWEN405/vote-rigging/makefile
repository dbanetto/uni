
all: report.pdf

%.pdf: %.md
	pandoc -V geometry:margin=1in $< -o $@
