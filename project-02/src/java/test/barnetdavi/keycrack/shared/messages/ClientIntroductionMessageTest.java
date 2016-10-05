package barnetdavi.keycrack.shared.messages;

import barnetdavi.keycrack.shared.MessageType;
import org.junit.Assert;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;

public class ClientIntroductionMessageTest {

    @Test
    public void writeContents() throws Exception {
        // Arrange
        ClientIntroductionMessage msg = new ClientIntroductionMessage(42);
        ByteArrayOutputStream output = new ByteArrayOutputStream();

        // Act
        msg.writeContents(output);

        // Assert
        Assert.assertArrayEquals(output.toByteArray(), new byte[]{0x00, 0x00, 0x00, 0x00, 0x2a});
    }

    @Test
    public void fromInputStream() throws Exception {
        // Arrange
        InputStream stream = new ByteArrayInputStream(new byte[]{0x00, 0x00, 0x00, 0x2a});

        // Act
        ClientIntroductionMessage message = ClientIntroductionMessage.fromInputStream(stream);

        // Assert
        Assert.assertEquals(message.getChuncksize(), 42);
    }

    @Test
    public void getChuncksize() throws Exception {
        // Arrange
        ClientIntroductionMessage msg = new ClientIntroductionMessage(10);

        // Assert
        Assert.assertEquals(msg.getChuncksize(), 10);
    }

    @Test
    public void getType() throws Exception {
        // Arrange
        ClientIntroductionMessage msg = new ClientIntroductionMessage(10);

        // Assert
        Assert.assertEquals(msg.getType(), MessageType.CLIENT_INTRODUCTION);
    }

}