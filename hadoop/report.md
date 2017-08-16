% NWEN406 - Project 1
% David Barnett (300313764)

# Preface

My solution uses the Hadoop libraries supplied via maven central.

If an error occurs while building or it takes a long to fetch the libraries
this is most likely due to maven not having the correct proxy settings.

Here are some resources to solve this:

 * [Maven Documentation](https://maven.apache.org/guides/mini/guide-proxies.html)[^1], or
 * [Stackoverflow](https://stackoverflow.com/questions/1251192/how-do-i-use-maven-through-a-proxy#1251216)[^2]

[^1]: https://maven.apache.org/guides/mini/guide-proxies.html
[^2]: https://stackoverflow.com/questions/1251192/how-do-i-use-maven-through-a-proxy#1251216

# Part 1

## Results for `WordCount`

* See `Part1/makefile` for the makefile used
* See `Part1/output/part-r-00000` for the results

# Part 2

The source code for the

## Task 1

<!--
 Document your code in a report. Include the source code and briefly explain how it works. Include evidence of testing for correctness.
 -->

### Documentation & How it Works

`SearchMap` and `SearchReduce` classes are used for this tasks


### Evidence of Correctness

 * Ran on popular ID's listed on the data leak's Wikipedia page

## Task 2

<!--
 Document your code in a report. Include the source code and briefly explain how it works. Include evidence of testing for correctness.
 -->

### Documentation & How it Works

### Design

The design to get the summary statistics included two rounds through Hadoop.

The first round was filtering and parsing the log.
The filtering removed invalid log lines, such as reading from the README file in the input
data folder.
The parsing split the log line from its tab-separated columns into an array of strings.
From the length of the array it can be determined if the log line is for a query or a click through.

### Evidence of Correctness

Compared the results to published statistics of the [data set](http://www.michael-noll.com/blog/2006/08/07/aol-research-publishes-500k-user-queries/)[^3]

### Results

 Distinct Users |  Total Queries | Total Click through
--------------- +----------------+---------
 657426         | 16946938       | 19442628

[^3]: http://www.michael-noll.com/blog/2006/08/07/aol-research-publishes-500k-user-queries/

## Task 3

<!--
    Document how you conducted your experiment, include the raw data, graph it and explain what you observed.
-->

![](graph.eps)
