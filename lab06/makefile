CC=gcc
CFLAGS=-Wall -g1

all: tcpserver

tcpserver: tcpserver.c
	$(CC) $< -o $@ $(CFLAGS)

clean:
	rm tcpserver
