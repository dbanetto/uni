HADOOP_VERSION:=2.8.0
HADOOP_PREFIX:=/local/Hadoop/hadoop-${HADOOP_VERSION}
JAR:=target/loganalysis-1.0.jar
HADOOP_INPUT:=/tmp/aol
HADOOP_OUTPUT:=aol-output
LOCAL_OUTPUT:=output
MAIN_CLASS:=nz.ac.vuw.ecs.barnetdavi.LogAnalysis
SRC:=$(shell find src/ -type f -name '*.java')
ANON_ID?=7980225
REDUCE_TASKS?=5

all: $(JAR)

test:
	mvn test

$(JAR): $(SRC)
	need java
	mvn package

clean:
	rm -rf target
	rm -f Part2.zip

reset:
	hadoop dfs -rm -r -f $(HADOOP_OUTPUT)

search: $(JAR) reset
	hadoop jar $(JAR) $(MAIN_CLASS)  $(HADOOP_INPUT) $(HADOOP_OUTPUT) -anonid $(ANON_ID) -reducetasks $(REDUCE_TASKS)

summary: $(JAR) reset
	hadoop dfs -rm -r -f "$(HADOOP_OUTPUT)_temp"
	hadoop jar $(JAR) $(MAIN_CLASS)  $(HADOOP_INPUT) $(HADOOP_OUTPUT) -summary -reducetasks $(REDUCE_TASKS)

download:
	rm -rf $(LOCAL_OUTPUT)
	hadoop dfs -copyToLocal -f $(HADOOP_OUTPUT) $(LOCAL_OUTPUT)

zip:
	zip -r Part2.zip -r src makefile pom.xml data
