package nz.ac.vuw.ecs.barnetdavi;

import java.io.IOException;
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

    public static class Map  extends Mapper<LongWritable, Text, Text, Text> {
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
            // context.write(new Text(targetId), new Text("WHY IS THERE IS NO OUTPUT " + targetId + " " + anonId + " "));

            // TODO: make this a run time option, see WordCount2.java on the lab handout
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
 
    public static class Reduce extends Reducer<Text, Text, Text, Text> {
        // do not require a reduce
    }
 
    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();
        GenericOptionsParser optionParser = new GenericOptionsParser(conf, args);
        String[] remainingArgs = optionParser.getRemainingArgs();
        if ((remainingArgs.length != 3) && (remainingArgs.length != 4)) {
            System.err.println("Usage: analysislog <in> <out> -anonid anonId");
            System.exit(2);
        }

        Job job = Job.getInstance(conf, "aol-log-analyser");

        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);

        job.setJarByClass(LogAnalysis.class);
        job.setMapperClass(Map.class);
        job.setCombinerClass(Reduce.class);
        job.setReducerClass(Reduce.class);
 
        job.setInputFormatClass(TextInputFormat.class);
        job.setOutputFormatClass(TextOutputFormat.class);

        List<String> otherArgs = new ArrayList<String>();
        for (int i=0; i < remainingArgs.length; ++i) {
            if ("-anonid".equals(remainingArgs[i])) {
                job.getConfiguration().set("LogAnalysis.Mapper.id", remainingArgs[i + 1]);
                i++;
            } else {
                otherArgs.add(remainingArgs[i]);
            }
        }
        FileInputFormat.addInputPath(job, new Path(otherArgs.get(0)));
        FileOutputFormat.setOutputPath(job, new Path(otherArgs.get(1)));


        System.exit(job.waitForCompletion(true) ? 0 : 1);
    }
}
