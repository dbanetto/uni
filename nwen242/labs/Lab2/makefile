PDFS=nwen242-lab02-answers.pdf

all: $(PDFS)

%.pdf: %.md
	pandoc $< -o $@

clean:
	rm $(PDFS)
