package nz.ac.vuw.ecs.barnetdavi;

import java.io.IOException;
import java.util.*;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapred.*;

public class LogAnalysis {

    public static class Map extends MapReduceBase implements Mapper<LongWritable, Text, Text, Text> {
        private Text textKey = new Text();
        private Text textValue = new Text();

        public void map(LongWritable key, Text value, OutputCollector<Text, Text> output, Reporter reporter) throws IOException {

            String[] sections = value.toString().split("\t", 5);

            if (sections.length <= 3) {
                return;
            }

            String anonId = sections[0];

            // validate it is an ID
            if (anonId.isEmpty() || !Character.isDigit(anonId.charAt(0))) {
                return;
            }

            // TODO: make this a run time option, see WordCount2.java on the lab handout
            if (anonId.equals("7980225")) {

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

                output.collect(textKey, textValue);
            }
        }
    }
 
    public static class Reduce extends MapReduceBase implements Reducer<Text, Text, Text, Text> {
        public void reduce(Text key, Iterator<Text> values, OutputCollector<Text, Text> output, Reporter reporter) throws IOException {
            while (values.hasNext()) {
                output.collect(key, values.next());
            }
        }
    }
 
    public static void main(String[] args) throws Exception {
        JobConf conf = new JobConf(LogAnalysis.class);
        conf.setJobName("aol-log-analysis");
 
        conf.setOutputKeyClass(Text.class);
        conf.setOutputValueClass(Text.class);
 
        conf.setMapperClass(Map.class);
        conf.setCombinerClass(Reduce.class);
        conf.setReducerClass(Reduce.class);
 
        conf.setInputFormat(TextInputFormat.class);
        conf.setOutputFormat(TextOutputFormat.class);
 
        FileInputFormat.setInputPaths(conf, new Path(args[0]));
        FileOutputFormat.setOutputPath(conf, new Path(args[1]));
 
        JobClient.runJob(conf);
    }
}
