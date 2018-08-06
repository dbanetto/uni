
all: comp304a2 comp304a2.pdf

run: comp304a2
	@./comp304a2

test: comp304a2
	@! ./comp304a2 | grep False

comp304a2: comp304a2.lhs
	ghc $<

%.pdf: %.lhs
	pandoc $< -o $@

clean:
	rm comp304a2.o comp304a2.hi comp304a2
	rm comp304a2.pdf
