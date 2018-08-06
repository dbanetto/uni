#!/bin/sh

javac -cp xchart.jar:. *.java

java -cp xchart.jar:. ArraySum

rm *.class
