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
import java.math.BigInteger;

public class PostResultsMessage extends Message {

    private final int keySize;
    private List<KeySpace> keys;
    private BigInteger foundKey;

    public PostResultsMessage(int keySize, List<KeySpace> keys) {
        super(MessageType.POST_RESULTS);
        this.keySize = keySize;
        this.keys = keys;
    }

    public PostResultsMessage(int keySize, BigInteger foundKey) {
        super(MessageType.POST_RESULTS_FOUND);
        this.keySize = keySize;
        this.foundKey = foundKey;
    }

    @Override
    public void writeContents(OutputStream contents) throws IOException {
        contents.write(getType().toByte());
        if (getType() == MessageType.POST_RESULTS) {
            contents.write(Util.toByteArray(keys.size()));
            for (KeySpace key : keys) {
                key.writeToStream(contents, keySize);
            }

        } else {
            // POST_RESULTS_FOUND
            contents.write(Util.toByteArray(foundKey, keySize + 1));
        }
    }

    public static PostResultsMessage fromInputStream(MessageType type, int keySize, InputStream contents) throws IOException {
        if (type == MessageType.POST_RESULTS) {
            byte[] buffer = new byte[4];
            contents.read(buffer);
            int keysLength = Util.fromByteArray(buffer);

            List<KeySpace> keys = new ArrayList<>(keysLength);
            for (int i = 0; i < keysLength; i++) {
                keys.add(KeySpace.readFromStream(contents, keySize));
            }

            return new PostResultsMessage(keySize, keys);
        } else {
            // POST_RESULTS_FOUND
            byte[] buffer = new byte[keySize + 1];
            contents.read(buffer);
            BigInteger foundKey = new BigInteger(buffer);

            return new PostResultsMessage(keySize, foundKey);
        }
    }

    public List<KeySpace> getKeys() {
        return keys;
    }

    public BigInteger getFoundKey() {
        return foundKey;
    }

    @Override
    public String toString() {
        return "PostResultsMessage{" +
                "type=" + getType() +
                ", keys=" + keys +
                ", foundKey=" + foundKey +
                '}';
    }
}
