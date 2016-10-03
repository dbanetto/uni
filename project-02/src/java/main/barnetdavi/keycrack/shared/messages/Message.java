package barnetdavi.keycrack.shared.messages;

import barnetdavi.keycrack.shared.MessageType;

import java.io.*;

public abstract class Message {

    private final MessageType type;

    protected Message(MessageType type) {
        this.type = type;
    }

    public abstract void writeContents(OutputStream contents) throws IOException;

    public static Message readFromStream(InputStream contents) throws IOException {
        byte[] type = new byte[1];
        contents.read(type);

        MessageType messageType = MessageType.fromByte(type[0]);

        switch (messageType) {
            case INITIAL_CONNECTION:
                return InitialMessage.readFromStream(contents);
            default:
                throw new IllegalStateException();
        }
    }

    public MessageType getType() {
        return type;
    }
}
