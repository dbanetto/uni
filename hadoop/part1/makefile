HADOOP_VERSION:=2.8.0
HADOOP_PREFIX:=/local/Hadoop/hadoop-${HADOOP_VERSION}
JAR:=target/wordcount-1.0.jar
SRC:=$(wildcard src/*.java)
DATA_FILES:=$(wildcard data/*)
LOCAL_INPUT:=data
HADOOP_INPUT:=part1-input
HADOOP_OUTPUT:=part1-output
LOCAL_OUTPUT:=output

all: $(JAR)

$(JAR): $(SRC)
	need java
	mvn package

clean:
	rm -rf target

reset:
	hadoop dfs -rm -r -f $(HADOOP_OUTPUT)

run: $(JAR) reset
	hadoop jar $(JAR) nz.ac.vuw.ecs.barnetdavi.WordCount $(HADOOP_INPUT) $(HADOOP_OUTPUT)

upload: $(DATA)
	hadoop dfs -copyFromLocal -f -p $(LOCAL_INPUT)/ $(HADOOP_INPUT)

download:
	hadoop dfs -copyToLocal $(HADOOP_OUTPUT) $(LOCAL_OUTPUT)
