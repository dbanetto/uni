% NWEN406 - Project 1
% David Barnett (300313764)

# Preface

My solution uses the Hadoop libraries supplied via maven central.
To ensure this works the installation of `mvn` tool is required
for the makefile files.
This has been tested on the general student ECS computers running Linux.

It is assumed that the makefiles are run on the hadoop cluster machines, though
building the applications has been tested to work on general ECS Linux machines.

If an error occurs while building or it takes a long to fetch the libraries
this is most likely due to maven not having the correct proxy settings.

Here are some resources to solve this:

 * [Maven Documentation](https://maven.apache.org/guides/mini/guide-proxies.html)[^1], or
 * [StackOverflow](https://stackoverflow.com/questions/1251192/how-do-i-use-maven-through-a-proxy#1251216)[^2]

[^1]: https://maven.apache.org/guides/mini/guide-proxies.html
[^2]: https://stackoverflow.com/questions/1251192/how-do-i-use-maven-through-a-proxy#1251216

# Part 1

## Results for `WordCount`

* See in `Part1.zip` for `/makefile` for the makefile used
* See `Part1.zip` `/output/part-r-00000` for the results of the application

The results assume that a word is a set of characters separated by 
white space, `,`, `:`, `;`, `?`, `!`, `[`, `]`, `(` and `)`.
It is also assumed that a word or any case should be counted as the same,
e.g. `The` and `the` are the same. To achieve this all words are transformed
into lower case.

## Running

The makefile for this task is in `Part1.zip` in the path `makefile`.
This makefile is also used for the entirety of part 2.

Running this part is available via a makefile.
All commands given are assuming that the current directory is
in the `Part1` directory.
To run this code the makefile provides a task to achieve this:

 * `make run` runs the job on hadoop
 * `make download` will download the results into `./output/`

You can compose these tasks by running `make run download`.

# Part 2

## Task 1

<!--
 * Document your code in a report.
 * Include the source code and briefly explain how it works.
 * Include evidence of testing for correctness.
 -->

### Documentation & How it Works

`SearchMap` and `SearchReduce` classes are used for this tasks.
The source code for this is in `Part2.zip` in the path
`/src/nz/ac/vuw/ecs/barnetdavi/LogAnalysis.java`.

The code is structured to be a simple filter based on the `AnonId` attribute.
This handles testing if the given line is a valid log line and inspecting to
check if the anonymous id is what it being searched for.

The map phase of the application is handled by the `SearchMap` class.
This phase only emits data if they pass two tests:

 * the line is a valid log line, this is determined by ensuring there are 3 or 5 tab characters
  in the line and starts with a string of numeric characters implying it is an anonymous id, and
 * the anonymous id that is parsed matches the given id to search for

For the log lines that pass the tests 
they are reduced to the described format of `{AnonID, Query, ItemRank, ClickURL}`
with the `AnonId` being used as the key and the rest as tab separated strings.
This is then sent to the reduce phase.

The reduce phase of the application is handled by the `SearchReduce` class.
This implementation is an identity function that outputs what it receives.
This is because during the map phase the second test already reduces the output.


#### Makefile

The make file for this task is in `Part2.zip` in the path `makefile`.
This makefile is also used for the entirety of part 2.

Running this part is available via a makefile.
All commands given are assuming that the current directory is
in the `Part2` directory.

The makefile will automatically build the application JAR in case of updates
or missing.
This can be run by running `make`

To run the application to compute the solution for this task 
the makefile provides a task to achieve this:

 * `make search` to search for the default anonymous id `7980225`
 * `ANON_ID=927 make search` to search for given anonymous id, in this case `927`
 * `make download` will download the result to the local folder `output/`,
    this can be configured with the `LOCAL_OUTPUT` variable. 

The following command will search for the given id and download it

`make search download ANON_ID=927 LOCAL_OUTPUT=output_927`


### Evidence of Correctness

To test the correctness of the application unit tests were created to test the map reduce.
This was built using the `minicluster` library which is an additional library for Hadoop.
With this library a locally hosted `HDFS` and hadoop cluster can be created to use for
local testing.
Given this setup the unit tests cover the following cases:

 1. correct query log line of specified user,
 2. correct click through log line of specified user,
 3. correct query log line of NOT specified user, and
 4. incorrect log line

To show that the tests pass the output is parsed to ensure that only cases 1 & 2 are in
the output in the correct format.

For integration tests the application is run with popular anonymous ID's listed on the data 
leak's Wikipedia page[^3]. This allowed me to compare the results of known ids to my own
implementation. This shows the overall correctness of the application.

[^3]: Anonymous id's `927` and `711391` from https://en.wikipedia.org/wiki/AOL_search_data_leak 

## Task 2

<!--
 * Document your code in a report.
 * Include the source code and briefly explain how it works.
 * Include evidence of testing for correctness.
 -->

### Documentation & How it Works

### Design

The design to get the summary statistics included two rounds through Hadoop.
The first round prepare the data to be easily consumed by the second round.

The first round filters and parses the log to emit a key value pair of
`<AnonId, (UserCount, QueryCount, ClickCount) >`.

The filtering removed invalid log lines, such as lines from the README file in the input
data set.
The parsing split the log line from its tab-separated columns into an array of strings.
From the length of the array it can be determined if the log line is for a query or a click through.

The values emitted are in the form:

> `<AnonId, (UserCount, QueryCount, ClickCount) >`

 * The `AnonId` is set to the anonymous id parsed from the log
 * `UserCount` is set always to be `1` as it is confirmed to found 1 user
 * `QueryCount` is set to `1` if the log line is found to be a query log, otherwise `0`
 * `ClickCount` is set to `1` if the log line is found to be a click log, otherwise `0`

This is then sent to the reducer/combinator

### Evidence of Correctness

Compared the results to published statistics of the [data set](http://www.michael-noll.com/blog/2006/08/07/aol-research-publishes-500k-user-queries/)[^4]

### Results

 Distinct Users |  Total Queries | Total Click through
--------------- +----------------+---------
 657426         | 16946938       | 19442628

[^4]: http://www.michael-noll.com/blog/2006/08/07/aol-research-publishes-500k-user-queries/

## Task 3

<!--
    Document how you conducted your experiment, include the raw data, graph it and explain what you observed.
-->

![Graph of Wall time vs. number of reduce nodes](graph.eps)
