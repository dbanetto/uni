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

* See in `Part1.zip` for `makefile` for the makefile used
* See `Part1.zip` `output/part-r-00000` for the results of the application

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
 * `make download` will download the results into `output/`

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
`src/main/java/nz/ac/vuw/ecs/barnetdavi/LogAnalysis.java`.

The code is structured to be a simple filter based on the `AnonId` attribute.
This handles testing if the given line is a valid log line and inspecting to
check if the anonymous id is what it being searched for.

The map phase of the application is handled by the `SearchMap` class.
This phase only emits data if they pass two tests:

 * the line is a valid log line, this is determined by ensuring there are 3 or 5 tab characters
  in the line and starts with a string of numeric characters implying it is an anonymous id, and
 * the anonymous id that is parsed matches the given id to search for

For the log lines that pass the tests they are reduced to the
described format of `{AnonID, Query, ItemRank, ClickURL}`
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
These tests can be found in `Part2.zip` in the path
`src/test/java/nz/ac/vuw/ecs/barnetdavi/TestMapReduce.java`.
This can be run via `make test` in the part 2 directory.

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

The design to get the summary statistics included two stages through Hadoop.
The first stage prepare the data to be easily consumed by the second stage.

#### Stage 1

The first stage filters and parses the log to emit a key value pair of
`<AnonId, (UserCount, QueryCount, ClickCount) >`.

The filtering removed invalid log lines, such as lines from the README file in the input
data set.
This is similar to the filtering of valid log lines from Task 1, the total
number of tab separated segments are used to determine if it is valid.
From the length of the array it can be determined if the log line is for a query or a click through.
If the log line has 3 segments it can be determined to be a query log and if it has 5 segments
a click log, otherwise it is an invalid line and discarded.

The values emitted are in the form:

> `<AnonId, (UserCount, QueryCount, ClickCount) >`

 * The `AnonId` is set to the anonymous id parsed from the log
 * `UserCount` is set always to be `1` as it is confirmed to found 1 user
 * `QueryCount` is set to `1` if the log line is found to be a query log, otherwise `0`
 * `ClickCount` is set to `1` if the log line is found to be a click log, otherwise `0`

This is then sent to the reducer/combinator as a tab separated list to be merged.

The reduce function for stage 1 sums the `QueryCount` and `ClickCount` for each `AnonId`.
This is achieved by parsing the tab separated list into its parts and summed as integers.
The combinator is applying the reduce function on all key-values pairs in memory
of the local node before sending them to the reducer.
This allows the optimisation of in-memory reduction which is similar to Apache SPARK.
In this case applying the reduce code as the combinator works but you would lose the
assumption that all values will have either a `QueryCount` or `ClickCount` value but
the sum of what was computed by a single node.
The output of the stage 1 reduce is the same as the output of stage 1's map.
At this point the results currently show the total number of queries and click through
per anonymous id with one row per id.

#### Stage 2

This is then feed to stage 2 which is a separate Hadoop job.
The goal of this stage is to reduce the distinct anonymous ids and other counts into a grand total.

The map for stage 2 takes in the output of the stage 1 as the input and reduces it down to
a single key-value pair.
To achieve this the map phase for stage 2 removes the anonymous id and replaces it with the
constant string `summary` and emits the value unchanged.
This is then feed into stage 2's reducer.

The reducer for stage 2 sums all of the columns, `UserCound`, `QueryCount` and `ClickCount`.
As in stage 1's reducer this one also is used for as a combinator.
This is a huge help as instead of one reduce node is given millions of values to merge
a majority of it is merged in-memory of the map node.
To accommodate this the `UserCount` cannot be assumed to be 1 during the reduce function
as the result of stage 1 gives the guarantee that one row, or value, is
one distinct user but the combinator breaks this guarantee.
The output of the reducer is a single key-value pair of `<"summary", (UserCount, QueryCount, ClickCount)>`
as a tab separated list.

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

 * `make summary` to compute the summary statistics
 * `make download` will download the result to the local folder `output/`,
    this can be configured with the `LOCAL_OUTPUT` variable.

The following command will compute the summary statistics and download
the results into the `output_summary` folder.

`make search download LOCAL_OUTPUT=output_summary`

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
 5. correct results from stage 1 being passed to stage 2

To show that the tests pass the output is parsed to ensure that only cases 1 & 2 are in
the output in the correct format.
These tests can be found in `Part2.zip` in the path
`src/test/java/nz/ac/vuw/ecs/barnetdavi/TestMapReduce.java`.
This can be run via `make test` in the part 2 directory.

For integration tests the final results of the application is compared to the
published statistics of the [data set](http://www.michael-noll.com/blog/2006/08/07/aol-research-publishes-500k-user-queries/)[^4].
This has also been included in the README file in the input directory.


### Results

 Distinct Users |  Total Queries | Total Click through
--------------- +----------------+---------
 657426         | 16946938       | 19442628

[^4]: http://www.michael-noll.com/blog/2006/08/07/aol-research-publishes-500k-user-queries/

## Task 3

<!--
    Document how you conducted your experiment, include the raw data, graph it and explain what you observed.
-->

The aim of this experiment is to investigate the impact of reduce nodes
has on the wall clock time for a Hadoop job.
In this case the wall clock is the time recorded in the log when the
MapReduce job started and ended ; this is not the total collective CPU time of
all nodes.
From parallelisation it is known that the ideal speed up is $1/n$ where $n$ is the
number of nodes, this implies that ideally by increasing the node count from 1 to 2
the total time would reduce by $1/2$.

### Hypothesis

My hypothesis for this experiment is that the impact of the number of reduce nodes
will be dependent on the type of work load of the task.
In particular jobs where there are more unique keys,
and thus more able to be broken up to more nodes,
would have a more impact from more reduce tasks.

### Method

To execute the applications with the given number of reduce tasks a
flag was added to the application, `-reducetasks N` where `N`
is the number of reduce tasks.
This setting can be verified to be set properly by inspecting the
log output of the completed job.

The following bash script is used to run the experiment:

```bash
for TRY in `seq 1 4` ; do
    for COUNT in `seq 2 2 6` ; do
        ANON_ID=7980225 REDUCE_TASKS=$COUNT make search &> data/search_${COUNT}_${TRY}
        REDUCE_TASKS=$COUNT make summary &> data/summary_${COUNT}_${TRY}
    done
done
```

This script will make 4 replicates of running the jobs with 2, 4 and 6 reduce tasks.
The `TRY` variable is used to keep track how many times the experiment has been
repeated.
The `COUNT` variable is used to represent the number of reduce tasks that should be
used.
This is passed to the application via the makefile parameter `REDUCE_TASKS`
as shown on line 3 & 4 of the script.
Both the search and summary jobs are run to provide more data points for analysis.
The anonymous id that is being searched for is fixed to keep the experiment constant
as the analysis is on the impact of reduce tasks not filtering.
The usage of `&>` enables both `STDOUT` and `STDERR` to be piped to disk,
this is due to Hadoop's logging is written to `STDERR`.

To extract the wall clock time for the job the logged time by Hadoop at the start
 and the end of the job is used.
For example the log snippet below, the start time here would be
`10:01:24` and the end time of `10:01:49`.

```
17/08/15 10:01:24 INFO mapreduce.Job: Job job_1502070693803_0424 running in uber mode : false
17/08/15 10:01:24 INFO mapreduce.Job:  map 0% reduce 0%
17/08/15 10:01:31 INFO mapreduce.Job:  map 5% reduce 0%
17/08/15 10:01:38 INFO mapreduce.Job:  map 10% reduce 0%
17/08/15 10:01:41 INFO mapreduce.Job:  map 43% reduce 0%
17/08/15 10:01:42 INFO mapreduce.Job:  map 62% reduce 0%
17/08/15 10:01:44 INFO mapreduce.Job:  map 67% reduce 0%
17/08/15 10:01:45 INFO mapreduce.Job:  map 95% reduce 0%
17/08/15 10:01:46 INFO mapreduce.Job:  map 100% reduce 0%
17/08/15 10:01:47 INFO mapreduce.Job:  map 100% reduce 100%
17/08/15 10:01:49 INFO mapreduce.Job: Job job_1502070693803_0424 completed successfully
```

To achieve this the following bash magic was used to convert the logs into a CSV file

```bash
grep -i -r -E 'running in|successfully' data | # match on start/end log entries
    paste -d " " - - | # pair up start & end log entries
    cut -d' ' -f1,2,14 | # cut out file and times
    sed -e 's/data\///' | # remove path
    sed -r 's/:[0-9]+\/[0-9]+\/[0-9]+//' | # remove date
    sed -r 's/_([0-9])/,\1/g' | # modify file name into columns (# of tasks, try)
    sed -e 's/ /,/g' # spaces into columns
     > data.csv # dump to file
```

By using this method to get the wall clock time the start and end time are accurate
but not precise.
They are accurate since Hadoop itself is logging directly before and after where
a user made timer wrapping the job would include more unknown variations, such as
uploading the application to the server.
However, this measurement is not precise as the log only gives the resolution
of seconds.
This is an acceptable trade-off as the experiment is focused on observing large changes
in the recorded time not putting it under a microscope to find the absolute fastest time.

### Results

The experiment was run in the evening (10pm) when the Hadoop cluster was
quite.
The experiment was run over a matrix 3 by 3 of configurations,
one axis is the number of reduce tasks (2,4,6) and the other is which
hadoop job is ran (search, summary stage 1, summary stage 2).
Each experiment configuration was repeated 4 times to minimize the
variation of using the shared cluster.
The results are parsed from the logged results from the log of running
the cluster.

Tables of raw data from experiment are available in `data.odt` or can be
mined from the raw experiment data.

#### Table of average wall time elapsed over 4 tries

\# of reduce tasks | Search | Summary stage 1 | Summary stage 2
--+---+---+----
2| 17s| 23s| 11s
4| 18s| 23s|  9s
6| 16s| 23s|  9s

### Expected Idealised results

**Note: these are to show what was expected**

\# of reduce tasks | Search | Summary stage 1 | Summary stage 2
-------------------+--------+-----------------+-------------------
2                  | 17s    | 23s             | 11s
4 (projected)      | 9s\*   | 12s\*           |  5s\*
6 (projected)      | 6s\*\* | 8s\*\*          |  3s\*\*

> \*: reduced by half of results of using 2 reduce tasks

> \*\*: reduced by a third of the projected results of using 4 reduce tasks

![Graph of Wall time vs. number of reduce nodes](graph.eps)

Figure 1 is the rendered graph of the compute time differences.

The results show a negative correlation between the number of reduce tasks and wall clock time.
This is shown by for all jobs tested the by increasing the reduce tasks the elapsed time
decreased.
However, the actual results are very different to the expected idealised results projected.
The graph in figure 1 shows that the relationship is a small overall negative relation.

### Discussion

There are many factors that lead to the difference in expected idealised results of `1/N` speed up and the
recorded figures.
A factor that played a role in this result was that only the reduce part of the job was
reduced where as opposed to the map and combinator phases.
Another factor at play is the volume of data of the job going to the reduce tasks.
A third factor that has impacted the result is the structure of the applications and the use
of combinators.

<!-- TODO: a paragraph for each factor -->
