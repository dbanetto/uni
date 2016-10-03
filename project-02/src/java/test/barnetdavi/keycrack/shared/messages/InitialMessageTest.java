package barnetdavi.keycrack.shared.messages;

import barnetdavi.keycrack.shared.MessageType;
import barnetdavi.keycrack.shared.messages.InitialMessage;
import org.junit.Assert;
import org.junit.Test;

import java.io.*;

/**
 * Created by drb on 01/10/16.
 */
public class InitialMessageTest {

    @Test
    public void getCipherText() throws Exception {
        // Arrange
        InitialMessage msg = new InitialMessage(new byte[] { 0x2a });

        // Assert
        Assert.assertArrayEquals(msg.getCipherText(), new byte[] { 0x2a });
    }

    @Test
    public void writeContents() throws Exception {
        // Arrange
        InitialMessage msg = new InitialMessage(new byte[] { 0x2a });
        ByteArrayOutputStream output = new ByteArrayOutputStream();

        // Act
        msg.writeContents(output);

        // Assert
        Assert.assertArrayEquals(output.toByteArray(), new byte[] { 0x00, 0x00, 0x00, 0x00, 0x01, 0x2a });
    }

    @Test
    public void fromInputStream() throws Exception {
        // Arrange
        InputStream stream = new ByteArrayInputStream(new byte[] { 0x00, 0x00, 0x00, 0x01, 0x2a });

        // Act
        InitialMessage message = InitialMessage.fromInputStream(stream);

        // Assert
        Assert.assertArrayEquals(message.getCipherText(), new byte[] { 0x2a });
    }

    @Test
    public void getType() throws Exception {
        // Arrange
        InitialMessage msg = new InitialMessage(new byte[] { 0x2a });

        // Assert
        Assert.assertEquals(msg.getType(), MessageType.INITIAL_CONNECTION);
    }

}