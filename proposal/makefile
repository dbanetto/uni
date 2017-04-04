
all: proj_proposal.pdf

proj_proposal.pdf: proj_proposal.tex proj_proposal.bib
	pdflatex proj_proposal
	bibtex proj_proposal
	pdflatex proj_proposal
	pdflatex proj_proposal

clean:
	rm -f proj_proposal.aux
	rm -f proj_proposal.log
	rm -f proj_proposal.pdf
	rm -f proj_proposal.bbl
	rm -f proj_proposal.blg
	rm -f proj_proposal.dvi
