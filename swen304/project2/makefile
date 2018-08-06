
all: build

build: src/*.java
	(cd src/ && javac -cp /usr/pkg/lib/java/postgresql94.jar:. LibraryUI.java )

run: build
	(cd src && java -cp /usr/pkg/lib/java/postgresql94.jar:. LibraryUI )

clean:
	rm -f src/*.class
