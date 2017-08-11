HADOOP_VERSION:=2.8.0
HADOOP_PREFIX:=/local/Hadoop/hadoop-${HADOOP_VERSION}
JAR:=target/loganalysis-1.0.jar
SRC:=$(wildcard src/*.java)
HADOOP_INPUT:=/tmp/aol
HADOOP_OUTPUT:=aol-output
LOCAL_OUTPUT:=output
MAIN_CLASS:=nz.ac.vuw.ecs.barnetdavi.LogAnalysis
SRC:=$(shell find src/ -type f -name '*.java')
ANON_ID?=7980225

all: $(JAR)

$(JAR): $(SRC)
	need java
	mvn package

clean:
	rm -rf target

reset:
	hadoop dfs -rm -r -f $(HADOOP_OUTPUT)

run: $(JAR) reset
	hadoop jar $(JAR) $(MAIN_CLASS)  $(HADOOP_INPUT) $(HADOOP_OUTPUT) -anonid $(ANON_ID)

download:
	rm -rf $(LOCAL_OUTPUT)
	hadoop dfs -copyToLocal -f $(HADOOP_OUTPUT) $(LOCAL_OUTPUT)
