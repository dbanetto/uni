package barnetdavi.keycrack.shared;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by drb on 02/10/16.
 */
public class MessageTypeTest {
    @Test
    public void byteToEnum() {
        for (MessageType type : MessageType.values()) {

            MessageType fromBye = MessageType.fromByte(type.toByte());

            assertEquals(type, fromBye);
        }
    }
}