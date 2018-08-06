CC=gcc
FLAGS=-Wall -g3
LIBS=-I.

all: encode decode

encode: encode.o
	$(CC) -o $@ $^ $(LIBS)

decode: decode.o
	$(CC) -o $@ $^ $(LIBS)

%.o: %.c
	$(CC) $(FLAGS) -c $< -o $@

clean:
	rm *.o
	rm encode decode
