all: slides.pdf

%.pdf: %.md
	pandoc $< -t beamer -o $@ -V theme:Boadilla

clean:
	rm -f slides.pdf
