
all: crack.c topic_questions.md
	gcc crack.c -o crack -Wall -Werror -std=c11 -g1
	pandoc topic_questions.md -o topic_questions.pdf
	pandoc report.md -o report.pdf
