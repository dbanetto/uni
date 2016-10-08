package barnetdavi.keycrack.shared.messages;

import barnetdavi.keycrack.shared.MessageType;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public abstract class Message {

    private final MessageType type;

    protected Message(MessageType type) {
        this.type = type;
    }

    public abstract void writeContents(OutputStream contents) throws IOException;

    public static Message readFromStream(InputStream contents, int keySize) throws IOException {
        byte[] type = new byte[1];
        contents.read(type);

        MessageType messageType = MessageType.fromByte(type[0]);

        switch (messageType) {
            case REQUEST_KEYS:
            case REQUEST_KEYS_WITH_CIPHER:
                return RequestKeysMessage.fromInputStream(messageType, contents);
            case KEY_BLOCK:
            case KEY_BLOCK_WITH_CIPHER:
                return KeyBlockMessage.fromInputStream(messageType, keySize, contents);
            case POST_RESULTS:
            case POST_RESULTS_FOUND:
                return PostResultsMessage.fromInputStream(messageType, keySize, contents);
            default:
                throw new IllegalStateException();
        }
    }

    public MessageType getType() {
        return type;
    }
}
