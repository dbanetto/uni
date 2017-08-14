package nz.ac.vuw.ecs.barnetdavi;

import java.io.IOException;
import java.math.BigInteger;
import java.util.*;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.util.GenericOptionsParser;

public class LogAnalysis {

    public static class SearchMap extends Mapper<LongWritable, Text, Text, Text> {
        private Text textKey = new Text();
        private Text textValue = new Text();

        private Configuration conf;

        @Override
        public void setup(Context context) throws IOException {
            conf = context.getConfiguration();
        }

        @Override
        public void map(LongWritable key, Text value, Context context)
                throws IOException, InterruptedException {

            String[] sections = value.toString().split("\t", 5);

            if (sections.length <= 3) {
                return;
            }

            String anonId = sections[0];

            // validate it is an ID
            if (anonId.isEmpty() || !Character.isDigit(anonId.charAt(0))) {
                return;
            }

            String targetId = conf.get("LogAnalysis.Mapper.id");

            if (anonId.equals(targetId)) {
                // build string in output format for non-key sections
                StringBuilder builder = new StringBuilder();
                boolean first = true;
                for (int i = 1; i < sections.length; i++) {
                    if (i == 2 || sections[i].isEmpty()) { continue; }
                    if (!first) {
                        builder.append('\t');
                    }
                    first = false;
                    builder.append(sections[i]);
                }

                textKey.set(anonId);
                textValue.set(builder.toString());

                context.write(textKey, textValue);
            }
        }
    }
 
    public static class SearchReduce extends Reducer<Text, Text, Text, Text> {
        // do not require a reduce

    }

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

            context.write(key, new Text(summary.toString()) );
        }
    }

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

            context.write(key, new Text(summary.toString()) );
        }
    }
    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();
        GenericOptionsParser optionParser = new GenericOptionsParser(conf, args);
        String[] remainingArgs = optionParser.getRemainingArgs();

        if (remainingArgs.length < 3 || 6 < remainingArgs.length) {
            System.err.println("Usage for Task 1: analysislog <in> <out> <-anonid anonId> [-reducetasks N]");
            System.err.println("Usage for Task 2: analysislog <in> <out> <-summary> [-reducetasks N]");
            System.exit(2);
        }

        Job job = setupJob(conf);

        Boolean isSearch = null;
        int reduceTasks = 5;

        List<String> otherArgs = new ArrayList<String>();
        for (int i=0; i < remainingArgs.length; ++i) {
            if ("-anonid".equals(remainingArgs[i])) {
                isSearch = true;
                job.getConfiguration().set("LogAnalysis.Mapper.id", remainingArgs[i + 1]);
                i++;
            } if ("-summary".equals(remainingArgs[i])) {
                isSearch = false;
            } if ("-reducetasks".equals(remainingArgs[i])) {
                reduceTasks = Integer.parseInt(remainingArgs[i + 1]);
                i++;
            } else {
                otherArgs.add(remainingArgs[i]);
            }
        }

        System.err.println("Allocating " + reduceTasks + " reduce tasks");
        job.setNumReduceTasks(reduceTasks);

        FileInputFormat.addInputPath(job, new Path(otherArgs.get(0)));
        FileOutputFormat.setOutputPath(job, new Path(otherArgs.get(1)));

        if (isSearch == null) {
           System.err.println("No arguments to determine mode were given, provide -anonid <ID> for filter by id or -summary for statistics");
           System.exit(2);
           return;
        } else if (isSearch) {
            System.err.println("Running Search algorithm");
            job.setJobName("aol-filter");
            job.setMapperClass(SearchMap.class);
            job.setCombinerClass(SearchReduce.class);
            job.setReducerClass(SearchReduce.class);
        } else {
            System.err.println("Running Summary algorithm");
            job.setJobName("aol-summary-1");
            FileOutputFormat.setOutputPath(job, new Path(otherArgs.get(1) + "_temp"));
            job.setMapperClass(SummaryMap.class);
            job.setCombinerClass(SummaryReduce.class);
            job.setReducerClass(SummaryReduce.class);

            job.waitForCompletion(true);

            job  = setupJob(conf);
            job.setJobName("aol-summary-2");
            job.setMapperClass(ResultSummaryMap.class);
            job.setCombinerClass(ResultSummaryReduce.class);
            job.setReducerClass(ResultSummaryReduce.class);

            FileInputFormat.addInputPath(job, new Path(otherArgs.get(1)+ "_temp"));
            FileOutputFormat.setOutputPath(job, new Path(otherArgs.get(1) ));
        }

        System.exit(job.waitForCompletion(true) ? 0 : 1);
    }

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
