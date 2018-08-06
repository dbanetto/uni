CC=gcc
CFLAGS=-Wall -g1

all: tcpserver topic_questions.pdf

tcpserver: tcpserver.c
	$(CC) $< -o $@ $(CFLAGS)

topic_questions.pdf: topic_questions.md
	pandoc topic_questions.md -o topic_questions.pdf

clean:
	rm tcpserver
	rm topic_questions.pdf
