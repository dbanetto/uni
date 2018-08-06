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
    private String plainText;

    public KeyBlockMessage(List<KeySpace> keys, int keySize) {
        super(MessageType.KEY_BLOCK);
        this.keys = keys;
        this.keySize = keySize;
    }

    public KeyBlockMessage(List<KeySpace> keys, int keySize, byte[] cipherText, String plainText) {
        super(MessageType.KEY_BLOCK_WITH_CIPHER);
        this.keys = keys;
        this.keySize = keySize;
        this.cipherText = cipherText;
        this.plainText = plainText;
    }

    @Override
    public void writeContents(OutputStream contents) throws IOException {
        contents.write(getType().toByte());

        if (getType() == MessageType.KEY_BLOCK_WITH_CIPHER) {
            contents.write(Util.toByteArray(keySize));
            contents.write(Util.toByteArray(cipherText.length));
            contents.write(cipherText);
            contents.write(Util.toByteArray(plainText.length()));
            contents.write(plainText.getBytes());
        }

        contents.write(Util.toByteArray(keys.size()));
        for (KeySpace key : keys) {
            key.writeToStream(contents, keySize);
        }
    }

    public static KeyBlockMessage fromInputStream(MessageType type, int keySize, InputStream contents) throws IOException {
        byte[] buffer = new byte[4];
        byte[] cipherText = null;
        String plainText = null;
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

            // 4 byte block
            buffer = new byte[4];
            contents.read(buffer);
            int plainTextLength = Util.fromByteArray(buffer);

            buffer = new byte[plainTextLength];
            contents.read(buffer);
            plainText = new String(buffer);

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
            return new KeyBlockMessage(keys, keySize, cipherText, plainText);
        }

        // KEY_BLOCK
        return new KeyBlockMessage(keys, keySize);
    }

    public List<KeySpace> getKeys() {
        return keys;
    }

    public int getKeySize() {
        return keySize;
    }

    public byte[] getCipherText() {
        return cipherText;
    }

    public String getPlainText() {
        return plainText;
    }

    @Override
    public String toString() {
        return "KeyBlockMessage{" +
                "type=" + getType() +
                ", keys=" + keys +
                ", keySize=" + keySize +
                ", cipherText=" + Arrays.toString(cipherText) +
                ", plainText=" + plainText +
                '}';
    }
}
