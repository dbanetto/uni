MDS:=$(wildcard *.md)
PDFS:=$(subst md,pdf,$(MDS)) 

all: $(PDFS)

%.pdf: %.md
	pandoc $< -o $@

clean:
	rm $(PDFS)
