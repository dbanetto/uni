% NWEN406 - Project 1
% David Barnett (300313764)

# Part 1

## Results for `WordCount`

* See `Part1/makefile` for the makefile used
* See `Part1/output/part-r-00000` for the results

# Part 2

## Task 1

## Task 2

### Design

The design to get the summary statistics included two rounds through Hadoop.

The first round was filtering and parsing the log.
The filtering removed invalid log lines, such as reading from the README file in the input
data folder.
The parsing split the log line from its tab-separated columns into an array of strings.
From the length of the array it can be determined if the log line is for a query or a click through.

## Task 3
