DIR = $(shell basename `pwd`)
DOCDIR = docs

all: build

build: dist/lib/HackAVote.jar

dist/lib/HackAVote.jar: *.java
	ant dist

clean:
	ant clean

README.html: README
	echo '<html><head><title>README</title><body><pre>' > $@
	cat README >> $@
	echo '</pre></body></html>' >> $@

docs: README.html
	mkdir $(DOCDIR)
	javadoc -d $(DOCDIR) -private -overview README.html -windowtitle "Hack-a-Vote" -author *.java

control: build
	java -cp dist/lib/HackAVote.jar BallotControl &

console: build
	java -cp dist/lib/HackAVote.jar Console &

listen:
	ncat -l -p 1840

test: build
	java -cp dist/lib/HackAVote.jar BallotControl -randomtest

run: console control
