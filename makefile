
proj_proposal.pdf: proj_proposal.tex
	pdflatex $<

clean:
	rm -f proj_proposal.aux
	rm -f proj_proposal.log
	rm -f proj_proposal.pdf
