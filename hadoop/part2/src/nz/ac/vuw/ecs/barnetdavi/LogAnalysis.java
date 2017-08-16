package nz.ac.vuw.ecs.barnetdavi;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.util.GenericOptionsParser;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 * Classes to run filter and summary analysis over the AOL log data set using Hadoop
 */
public class LogAnalysis {

    /**
     * Finds the search history for a given user
     * <p>
     * This is set via the configuration of <code>LogAnalysis.Mapper.id</code> as a string
     * <p>
     * Emits a key pair of &gt;AnonId, (Query, ItemRank, ClickUrl)&lt;
     */
    public static class SearchMap extends Mapper<LongWritable, Text, Text, Text> {
        private Text textKey = new Text();
        private Text textValue = new Text();

        private String targetId;

        @Override
        public void setup(Context context) throws IOException {
            targetId = context.getConfiguration().get("LogAnalysis.Mapper.id");
        }

        @Override
        public void map(LongWritable key, Text value, Context context)
                throws IOException, InterruptedException {

            // splits up the columns of the log file
            String[] sections = value.toString().split("\t", 5);

            // validates the row is really a log and not from an invalid line
            if (sections.length <= 3) {
                return;
            }


            // validate it is an ID
            String anonId = sections[0];
            if (anonId.isEmpty() ||
                    !Character.isDigit(anonId.charAt(0))) {
                return;
            }


            // filters by the target ID
            if (anonId.equals(targetId)) {

                // build string in output format for non-key sections
                StringBuilder builder = new StringBuilder();
                boolean first = true;
                for (int i = 1; i < sections.length; i++) {
                    // column 2 is QueryTime & ignores it
                    // skips empty columns as well
                    if (i == 2 || sections[i].isEmpty()) {
                        continue;
                    }

                    if (!first) {
                        builder.append('\t');
                    }
                    first = false;
                    builder.append(sections[i]);
                }

                // outputs the key & reconstructed log line with removed columns
                textKey.set(anonId);
                textValue.set(builder.toString());

                context.write(textKey, textValue);
            }
        }
    }

    /**
     * Reduce to results from <code>SearchMap</code>
     * <p>
     * Is an identity function since map already filters based on IDs
     * <p>
     * Emits a key pair of &gt;AnonId, (Query, ItemRank, ClickUrl)&lt;
     */
    public static class SearchReduce extends Reducer<Text, Text, Text, Text> {
        // do not require a reduce

    }

    /**
     * Map phase of the 1st round of computing summary statistics of all searches
     * <p>
     * Emits a key pair of &gt; AnonId, (UserCount, QueryCount, ClickCount) &lt;
     * <p>
     * Usercount will always be 1
     * Only one of QueryCount or ClickCount is exclusively 1, otherwise 0
     */
    public static class SummaryMap extends Mapper<LongWritable, Text, Text, Text> {
        private final static IntWritable one = new IntWritable(1);
        private Text textKey = new Text();
        private Text textValue = new Text();

        @Override
        public void map(LongWritable key, Text value, Context context)
                throws IOException, InterruptedException {

            String[] sections = value.toString().split("\t");

            if (sections.length < 3) {
                return;
            }

            String anonId = sections[0];

            // validate it is an ID
            if (anonId.isEmpty() || !Character.isDigit(anonId.charAt(0))) {
                return;
            }


            StringBuilder valueBuilder = new StringBuilder();

            valueBuilder.append(BigInteger.ONE).append('\t');

            if (sections.length < 5) {
                // Search Log
                valueBuilder.append(BigInteger.ONE)
                        .append('\t');
                valueBuilder.append(BigInteger.ZERO)
                        .append('\t');
            } else {
                // Click Log
                valueBuilder.append(BigInteger.ZERO)
                        .append('\t');
                valueBuilder.append(BigInteger.ONE)
                        .append('\t');
            }

            textKey.set(anonId);
            textValue.set(valueBuilder.toString());
            context.write(textKey, textValue);
        }

    }


    /**
     * Reduce phase of the 1st round of computing summary statistics of all searches
     * <p>
     * Emits a key pair of &gt; AnonId, (UserCount, QueryCount, ClickCount) &lt;
     * <p>
     * UserCount will always be 1,
     * QueryCount will be the total count of queries for the given AnonId
     * ClickCount will be the total count of click through for the given AnonId
     */
    public static class SummaryReduce extends Reducer<Text, Text, Text, Text> {

        @Override
        protected void reduce(Text key, Iterable<Text> values, Context context) throws IOException, InterruptedException {
            BigInteger totalSearches = BigInteger.ZERO;
            BigInteger totalClicks = BigInteger.ZERO;

            for (Text value : values) {
                String[] parts = value.toString().split("\t");

                BigInteger searches = new BigInteger(parts[1]);
                BigInteger clicks = new BigInteger(parts[2]);

                totalSearches = totalSearches.add(searches);
                totalClicks = totalClicks.add(clicks);
            }

            StringBuilder summary = new StringBuilder();
            summary.append(1)
                    .append('\t');
            summary.append(totalSearches.toString())
                    .append('\t');
            summary.append(totalClicks.toString());

            context.write(key, new Text(summary.toString()));
        }
    }

    /**
     * Map phase of the final round of computing summary statistics of all searches
     * <p>
     * This takes the results of SummaryMap/SummaryReduce such that the total number of
     * distinct users can be found.
     * <p>
     * Emits a key pair of &gt; 'summary', (UserCount, QueryCount, ClickCount) &lt;
     */
    public static class ResultSummaryMap extends Mapper<LongWritable, Text, Text, Text> {
        private final static IntWritable one = new IntWritable(1);
        private Text textKey = new Text();
        private Text textValue = new Text();

        @Override
        public void map(LongWritable key, Text value, Context context)
                throws IOException, InterruptedException {

            String[] sections = value.toString().split("\t", 2);


            textKey.set("summary");
            textValue.set(sections[1]);
            context.write(textKey, textValue);
        }

    }

    /**
     * Reduce phase of the final round of computing summary statistics of all searches
     * <p>
     * Emits a key pair of &gt; 'summary', (UserCount, QueryCount, ClickCount) &lt;
     * <p>
     * UserCount will be the total count of distinct users
     * QueryCount will be the sum of all users queries
     * ClickCount will be the sum of all users clicks
     */
    public static class ResultSummaryReduce extends Reducer<Text, Text, Text, Text> {

        @Override
        protected void reduce(Text key, Iterable<Text> values, Context context) throws IOException, InterruptedException {
            BigInteger totalIds = BigInteger.ZERO;
            BigInteger totalSearches = BigInteger.ZERO;
            BigInteger totalClicks = BigInteger.ZERO;

            for (Text value : values) {
                String[] parts = value.toString().split("\t");

                BigInteger count = new BigInteger(parts[0]);
                BigInteger searches = new BigInteger(parts[1]);
                BigInteger clicks = new BigInteger(parts[2]);

                totalIds = totalIds.add(count);
                totalSearches = totalSearches.add(searches);
                totalClicks = totalClicks.add(clicks);
            }

            StringBuilder summary = new StringBuilder();
            summary.append(totalIds.toString())
                    .append('\t');
            summary.append(totalSearches.toString())
                    .append('\t');
            summary.append(totalClicks.toString());

            context.write(key, new Text(summary.toString()));
        }
    }

    /**
     *
     * Usage
     *  for finding search history for a single user:
     *      analysislog <in> <out> <-anonid anonId> [-reducetasks N]
     *  for getting summary statistics:
     *      analysislog <in> <out> <-summary> [-reducetasks N]
     *
     *  The inclusion of -anonId or -summary dictates what job is run,
     *  -reducetasks N allows for changes in number of reduce tasks.
     *
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();
        GenericOptionsParser optionParser = new GenericOptionsParser(conf, args);
        String[] remainingArgs = optionParser.getRemainingArgs();

        if (remainingArgs.length < 3 || 6 < remainingArgs.length) {
            System.err.println("Usage for finding search history for a single user:\n\t analysislog <in> <out> <-anonid anonId> [-reducetasks N]");
            System.err.println("Usage for getting summary statistics:\n\t analysislog <in> <out> <-summary> [-reducetasks N]");
            System.exit(2);
        }


        Boolean isSearch = null;
        int reduceTasks = 5;
        String anonId = null;

        // Handle arguments passed in
        List<String> otherArgs = new ArrayList<String>();
        for (int i = 0; i < remainingArgs.length; ++i) {
            if ("-anonid".equals(remainingArgs[i]) && i + 1 < remainingArgs.length) {
                isSearch = true;
                anonId =  remainingArgs[i + 1];
                i++;
            }
            if ("-summary".equals(remainingArgs[i])) {
                isSearch = false;
            }
            if ("-reducetasks".equals(remainingArgs[i])) {
                reduceTasks = Integer.parseInt(remainingArgs[i + 1]);
                i++;
            } else {
                otherArgs.add(remainingArgs[i]);
            }
        }

        // apply reduce task setting
        System.err.println("Allocating " + reduceTasks + " reduce tasks");
        Job job = setupJob(conf);
        job.setNumReduceTasks(reduceTasks);

        FileInputFormat.addInputPath(job, new Path(otherArgs.get(0)));
        FileOutputFormat.setOutputPath(job, new Path(otherArgs.get(1)));

        // Handle the chosen operation
        if (isSearch == null) {
            System.err.println("No arguments to determine mode were given, provide -anonid <ID> for filter by id or -summary for statistics");
            System.exit(2);

            return;
        } else if (isSearch) {
            // Do the search of user id
            System.err.println("Running Search algorithm");

            job.getConfiguration().set("LogAnalysis.Mapper.id", anonId);
            job.setJobName("aol-filter");

            job.setMapperClass(SearchMap.class);
            job.setCombinerClass(SearchReduce.class);
            job.setReducerClass(SearchReduce.class);
        } else {
            // Compute the summary statistics
            System.err.println("Running Summary algorithm");

            job.setJobName("aol-summary-1");
            FileOutputFormat.setOutputPath(job, new Path(otherArgs.get(1) + "_temp"));
            job.setMapperClass(SummaryMap.class);
            job.setCombinerClass(SummaryReduce.class);
            job.setReducerClass(SummaryReduce.class);

            job.waitForCompletion(true);

            // completed 1st phase, now collates the final statistics

            job = setupJob(conf);
            job.setJobName("aol-summary-2");
            job.setMapperClass(ResultSummaryMap.class);
            job.setCombinerClass(ResultSummaryReduce.class);
            job.setReducerClass(ResultSummaryReduce.class);
            job.setNumReduceTasks(reduceTasks);

            FileInputFormat.addInputPath(job, new Path(otherArgs.get(1) + "_temp"));
            FileOutputFormat.setOutputPath(job, new Path(otherArgs.get(1)));
        }

        System.exit(job.waitForCompletion(true) ? 0 : 1);
    }

    /**
     * Sets up most of a Hadoop job
     *
     * @param conf
     * @return a partially setup Job object
     * @throws IOException
     */
    private static Job setupJob(Configuration conf) throws IOException {

        Job job = Job.getInstance(conf);

        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);

        job.setJarByClass(LogAnalysis.class);

        job.setInputFormatClass(TextInputFormat.class);
        job.setOutputFormatClass(TextOutputFormat.class);

        return job;
    }
}
