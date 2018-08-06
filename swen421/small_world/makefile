PDF:=report.pdf

all: $(PDF)

$(PDF): report.md
	pandoc $< -o $@

clean:
	rm -f $(PDF)

.PHONEY: clean
