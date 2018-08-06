
all: Straight Straight.pdf

run: Straight
	@./Straight

test: Straight
	@! ./Straight | grep False

Straight: Straight.lhs
	ghc $<

%.pdf: %.lhs
	pandoc $< -o $@

clean:
	rm Straight.o Straight.hi Straight
	rm Straight.pdf
