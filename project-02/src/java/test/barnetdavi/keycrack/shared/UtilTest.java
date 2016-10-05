package barnetdavi.keycrack.shared;

import org.junit.Assert;
import org.junit.Test;

/**
 * Created by drb on 5/10/16.
 */
public class UtilTest {

    @Test
    public void ToAndFromByte() throws Exception {
        int value = 10;

        Assert.assertEquals(value, Util.fromByteArray(Util.toByteArray(value)));
    }

    @Test
    public void toByteArray() throws Exception {
        Assert.assertArrayEquals(new byte[]{0x00, 0x00, 0x00, 0x2a}, Util.toByteArray(42));
    }

    @Test
    public void fromByteArray() throws Exception {
        Assert.assertEquals(42, Util.fromByteArray(new byte[]{0x00, 0x00, 0x00, 0x2a}));
    }

}