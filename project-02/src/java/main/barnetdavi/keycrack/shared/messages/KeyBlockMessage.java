package barnetdavi.keycrack.shared.messages;

import barnetdavi.keycrack.shared.KeySpace;
import barnetdavi.keycrack.shared.MessageType;
import barnetdavi.keycrack.shared.Util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class KeyBlockMessage extends Message {

    private final List<KeySpace> keys;
    private final int keySize;
    private byte[] cipherText;

    public KeyBlockMessage(List<KeySpace> keys, int keySize) {
        super(MessageType.KEY_BLOCK);
        this.keys = keys;
        this.keySize = keySize;
    }

    public KeyBlockMessage(List<KeySpace> keys, int keySize, byte[] cipherText) {
        super(MessageType.KEY_BLOCK_WITH_CIPHER);
        this.keys = keys;
        this.keySize = keySize;
        this.cipherText = cipherText;
    }

    @Override
    public void writeContents(OutputStream contents) throws IOException {
        contents.write(getType().toByte());

        if (getType() == MessageType.KEY_BLOCK_WITH_CIPHER) {
            contents.write(Util.toByteArray(keySize));
            contents.write(Util.toByteArray(cipherText.length));
            contents.write(cipherText);
        }

        contents.write(Util.toByteArray(keys.size()));
        for (KeySpace key : keys) {
            key.writeToStream(contents, keySize);
        }
    }

    public static KeyBlockMessage fromInputStream(MessageType type, int keySize, InputStream contents) throws IOException {
        byte[] buffer = new byte[4];
        byte[] cipherText = null;
        int keysCount;

        if (type == MessageType.KEY_BLOCK_WITH_CIPHER) {
            // 4 byte block
            contents.read(buffer);
            keySize = Util.fromByteArray(buffer);

            // 4 byte block
            contents.read(buffer);
            int cipherLength = Util.fromByteArray(buffer);

            // unknown sized block
            buffer = new byte[cipherLength];
            contents.read(buffer);
            cipherText = buffer;

            // ensure we got a 4 byte buffer for keys count
            buffer = new byte[4];
        }

        // 4 byte block
        contents.read(buffer);
        keysCount = Util.fromByteArray(buffer);

        List<KeySpace> keys = new ArrayList<>(keysCount);
        for (int i = 0; i < keysCount; i++) {
            keys.add(KeySpace.readFromStream(contents, keySize));
        }

        if (type == MessageType.KEY_BLOCK_WITH_CIPHER) {
            return new KeyBlockMessage(keys, keySize, cipherText);
        }

        // KEY_BLOCK
        return new KeyBlockMessage(keys, keySize);
    }

    @Override
    public String toString() {
        return "KeyBlockMessage{" +
                "keys=" + keys +
                ", keySize=" + keySize +
                ", cipherText=" + Arrays.toString(cipherText) +
                '}';
    }
}
