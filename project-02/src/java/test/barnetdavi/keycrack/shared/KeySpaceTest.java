package barnetdavi.keycrack.shared;

import org.junit.Assert;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.math.BigInteger;

import static org.junit.Assert.*;

/**
 * Created by drb on 05/10/16.
 */
public class KeySpaceTest {

    @Test
    public void writeToStream() throws Exception {
        // Arrange
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        KeySpace space = new KeySpace(BigInteger.ZERO, BigInteger.TEN);

        // Act
        space.writeToStream(output, 4);

        // Assert
        Assert.assertArrayEquals(new byte[] {0x00, 0x00, 0x00, 0x00,0x00, 0x00, 0x00, 0x00, 0x00, 0x0a}, output.toByteArray());
    }

    @Test
    public void writeToStreamLarge() throws Exception {
        // Arrange
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        KeySpace space = new KeySpace(BigInteger.ZERO, BigInteger.valueOf(286331153L));

        // Act
        space.writeToStream(output, 4);

        // Assert
        Assert.assertArrayEquals(new byte[] {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x11, 0x11, 0x11, 0x11}, output.toByteArray());
    }

    @Test
    public void readFromStream() throws Exception {
        // Arrange
        ByteArrayInputStream input = new ByteArrayInputStream(new byte[] {0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ,0x00, 0x00, 0x00, 0x0a});
        KeySpace expected = new KeySpace(BigInteger.ZERO, BigInteger.TEN);

        // Act
        KeySpace got = KeySpace.readFromStream(input, 4);

        // Assert
        Assert.assertEquals(expected, got);
    }

    @Test
    public void ToFromLarge() throws Exception {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        KeySpace space = new KeySpace(BigInteger.valueOf(3185209680L), BigInteger.valueOf(3185209808L));

        space.writeToStream(output, 4);
        ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());

        Assert.assertEquals(space, KeySpace.readFromStream(input, 4));
    }
}