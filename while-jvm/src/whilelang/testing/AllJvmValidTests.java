package whilelang.testing;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class AllJvmValidTests extends AbstractJvmValidTests {

    public AllJvmValidTests(String testName) {
        super(testName);
    }

    private static final String WHILE_SRC_DIR = "tests/valid/".replace('/', File.separatorChar);

    // Here we enumerate all available test cases.
    @Parameters(name = "{0}")
    public static Collection<Object[]> data() {
        ArrayList<String> testcases = new ArrayList<>();
        for (File f : new File(WHILE_SRC_DIR).listFiles()) {
            if (f.isFile()) {
                String name = f.getName();
                if (name.endsWith(".while")) {
                    // Get rid of ".while" extension
                    String testName = name.substring(0, name.length() - ".while".length());
                    testcases.add(testName);
                }
            }
        }
        return AbstractJvmValidTests.data(testcases.toArray(new String[0]));
    }
}
