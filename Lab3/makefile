CC:=pandoc
TARGET:=nwen242-lab3.pdf

all: $(TARGET)

%.pdf: %.md
	$(CC) $< -o $@
