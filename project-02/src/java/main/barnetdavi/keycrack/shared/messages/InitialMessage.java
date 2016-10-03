package barnetdavi.keycrack.shared.messages;

import barnetdavi.keycrack.shared.MessageType;

import java.io.*;
import java.math.BigInteger;
import java.nio.ByteBuffer;

public class InitialMessage extends Message {

    private final byte[] ciphertext;

    public InitialMessage(byte[] cipherText) {
        super(MessageType.INITIAL_CONNECTION);
        this.ciphertext = cipherText;
    }

    public byte[] getCipherText() {
        return ciphertext;
    }

    @Override
    void writeContents(OutputStream contents) throws IOException {
        contents.write(getType().toByte());
        contents.write(ByteBuffer.allocate(4).putInt(ciphertext.length).array());
        contents.write(ciphertext);
    }

    public static InitialMessage fromInputStream(InputStream content) throws IOException {
        byte[] buffer = new byte[4];
        content.read(buffer);
        int length = new BigInteger(buffer).intValue();

        buffer = new byte[length];

        content.read(buffer);

        return new InitialMessage(buffer);
    }
}
