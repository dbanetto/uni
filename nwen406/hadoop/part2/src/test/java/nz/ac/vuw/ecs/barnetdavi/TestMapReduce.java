package nz.ac.vuw.ecs.barnetdavi;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataOutputStream;
import org.apache.hadoop.fs.FileStatus;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.LocalFileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.hdfs.DFSConfigKeys;
import org.apache.hadoop.hdfs.HdfsConfiguration;
import org.apache.hadoop.hdfs.MiniDFSCluster;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.test.PathUtils;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

public class TestMapReduce {

       private static final String CLUSTER_1 = "cluster1";

    private File testDataPath;
    private Configuration conf;
    private MiniDFSCluster cluster;
    private FileSystem fs;

    @org.junit.Before
    public void setUp() throws Exception {
        testDataPath = new File(PathUtils.getTestDir(getClass()),
                "miniclusters");

        System.clearProperty(MiniDFSCluster.PROP_TEST_BUILD_DATA);

        createSimulatedHdfs();
    }

    private void createSimulatedHdfs() {


        File testDataCluster1 = new File(testDataPath, CLUSTER_1);
        String c1Path = testDataCluster1.getAbsolutePath();

        conf = new Configuration();
        // 100K blocksize
        conf.setLong(DFSConfigKeys.DFS_BLOCK_SIZE_KEY, 1048576);
        conf.setInt(DFSConfigKeys.DFS_BYTES_PER_CHECKSUM_KEY, 1);
        conf.setLong(DFSConfigKeys.DFS_HEARTBEAT_INTERVAL_KEY, 1000);
        conf.setInt(DFSConfigKeys.DFS_NAMENODE_REPLICATION_INTERVAL_KEY, 1000);
        conf.set(MiniDFSCluster.HDFS_MINIDFS_BASEDIR, c1Path);

        try {
            // simulated HDFS
            cluster = new MiniDFSCluster.Builder(conf).build();
            cluster.waitActive();
            fs = cluster.getFileSystem();
        } catch (IOException e) {
            Assert.fail("Could not create simulated HDFS " + e.getMessage());
        }
    }

    @org.junit.After
    public void tearDown() throws Exception {
        Path dataDir = new Path(
                testDataPath.getParentFile().getParentFile().getParent());
        fs.delete(dataDir, true);
        File rootTestFile = new File(testDataPath.getParentFile().getParentFile().getParent());
        String rootTestDir = rootTestFile.getAbsolutePath();
        Path rootTestPath = new Path(rootTestDir);
        LocalFileSystem localFileSystem = FileSystem.getLocal(conf);
        localFileSystem.delete(rootTestPath, true);
        cluster.shutdown();
    }

    @org.junit.Test
    public void testSearchMapReduce() throws Exception {
        String IN_DIR = "target/test/search/input";
        String OUT_DIR = "target/test/search/output";
        String DATA_FILE = "data.txt";

        Path inDir = new Path(IN_DIR);
        Path outDir = new Path(OUT_DIR);

        fs.delete(inDir, true);
        fs.delete(outDir,true);

        // create the input data files
        List<String> content = new ArrayList<String>();

        //{AnonID, Query, QueryTime, ItemRank, ClickURL}
        content.add("1000\tquery\t2000-01-01 12:00:00\n");
        content.add("1001\tincorrect userid\t2000-01-01 12:00:00\n");
        content.add("1000\tquery\t2000-01-01 12:00:00\t1\thttps://www.google.com\n");
        content.add("This is an invalid line\n");

        writeHDFSContent(fs, inDir, DATA_FILE, content);

        // set up the job, submit the job and wait for it complete
        Job job = Job.getInstance(conf);
        job.setJobName("Search Test");
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);
        job.setMapperClass(LogAnalysis.SearchMap.class);
        job.setReducerClass(LogAnalysis.SearchReduce.class);

        job.getConfiguration().set(LogAnalysis.SearchMap.ANON_ID_CONFIG, "1000");

        FileInputFormat.addInputPath(job, inDir);
        FileOutputFormat.setOutputPath(job, outDir);
        job.waitForCompletion(true);
        assertTrue(job.isSuccessful());

        // now check that the output is as expected
        List<String> results = getJobResults(fs, outDir, 2);
        assertEquals(results.size(), 2);
        assertTrue(results.contains("1000\tquery"));
        assertTrue(results.contains("1000\tquery\t1\thttps://www.google.com"));

        // clean up after test case
        fs.delete(inDir, true);
        fs.delete(outDir,true);
    }

    @Test
    public void testSummaryMapReduceStage1() throws Exception {
        String IN_DIR = "target/test/summary-1/input";
        String OUT_DIR = "target/test/summary-1/output";
        String DATA_FILE = "data.txt";

        Path inDir = new Path(IN_DIR);
        Path outDir = new Path(OUT_DIR);

        fs.delete(inDir, true);
        fs.delete(outDir,true);

        // create the input data files
        List<String> content = new ArrayList<String>();

        //{AnonID, Query, QueryTime, ItemRank, ClickURL}
        content.add("1000\tquery\t2000-01-01 12:00:00\n");
        content.add("1001\tincorrect userid\t2000-01-01 12:00:00\n");
        content.add("1000\tquery\t2000-01-01 12:00:00\t1\thttps://www.google.com\n");
        content.add("This is an invalid line\n");

        writeHDFSContent(fs, inDir, DATA_FILE, content);

        // set up the job, submit the job and wait for it complete
        Job job = Job.getInstance(conf);
        job.setJobName("Search Test");
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);
        job.setMapperClass(LogAnalysis.SummaryMap.class);
        job.setReducerClass(LogAnalysis.SummaryReduce.class);

        job.getConfiguration().set(LogAnalysis.SearchMap.ANON_ID_CONFIG, "1000");

        FileInputFormat.addInputPath(job, inDir);
        FileOutputFormat.setOutputPath(job, outDir);
        job.waitForCompletion(true);
        assertTrue(job.isSuccessful());

        // now check that the output is as expected
        List<String> results = getJobResults(fs, outDir, 2);
        assertEquals(results.size(), 2);
        assertTrue(results.contains("1000\t1\t1\t1"));
        assertTrue(results.contains("1001\t1\t1\t0"));

        // clean up after test case
        fs.delete(inDir, true);
        fs.delete(outDir,true);

    }


    @Test
    public void testSummaryMapReduceStage2() throws Exception {
        String IN_DIR = "target/test/summary-2/input";
        String OUT_DIR = "target/test/summary-2/output";
        String DATA_FILE = "data.txt";

        Path inDir = new Path(IN_DIR);
        Path outDir = new Path(OUT_DIR);

        fs.delete(inDir, true);
        fs.delete(outDir,true);

        // create the input data files
        List<String> content = new ArrayList<String>();

        //{AnonID, TotalUsers, TotalQueries, TotalClicks}
        content.add("1000\t1\t1\t1\n");
        content.add("1001\t1\t1\t0\n");

        writeHDFSContent(fs, inDir, DATA_FILE, content);

        // set up the job, submit the job and wait for it complete
        Job job = Job.getInstance(conf);
        job.setJobName("Search Test");
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);
        job.setMapperClass(LogAnalysis.ResultSummaryMap.class);
        job.setReducerClass(LogAnalysis.ResultSummaryReduce.class);

        job.getConfiguration().set(LogAnalysis.SearchMap.ANON_ID_CONFIG, "1000");

        FileInputFormat.addInputPath(job, inDir);
        FileOutputFormat.setOutputPath(job, outDir);
        job.waitForCompletion(true);
        assertTrue(job.isSuccessful());

        // now check that the output is as expected
        List<String> results = getJobResults(fs, outDir, 2);
        assertEquals(results.size(), 1);
        assertTrue(results.contains("summary\t2\t2\t1"));

        // clean up after test case
        fs.delete(inDir, true);
        fs.delete(outDir,true);

    }

    private void writeHDFSContent(FileSystem fs, Path dir, String fileName, List<String> content) throws IOException {
        Path newFilePath = new Path(dir, fileName);
        FSDataOutputStream out = fs.create(newFilePath);
        for (String line : content){
            out.writeBytes(line);
        }
        out.close();
    }

    protected List<String> getJobResults(FileSystem fs, Path outDir, int numLines) throws Exception {
        List<String> results = new ArrayList<String>();
        FileStatus[] fileStatus = fs.listStatus(outDir);
        for (FileStatus file : fileStatus) {
            String name = file.getPath().getName();
            if (name.contains("part-r-00000")){
                Path filePath = new Path(outDir + "/" + name);
                BufferedReader reader = new BufferedReader(new InputStreamReader(fs.open(filePath)));
                for (int i=0; i < numLines; i++){
                    String line = reader.readLine();
                    if (line == null){
                        break;
                    }
                    results.add(line);
                }
                assertNull(reader.readLine());
                reader.close();
            }
        }
        return results;
    }
}
