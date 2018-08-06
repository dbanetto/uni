
all: comp304a1 comp304a1.pdf

%.pdf: %.lhs
	pandoc $< -o $@

run: comp304a1
	./comp304a1

comp304a1: comp304a1.lhs
	ghc comp304a1.lhs

clean:
	rm comp304a1 comp304a1.hi comp304a1.o comp304a1.pdf
