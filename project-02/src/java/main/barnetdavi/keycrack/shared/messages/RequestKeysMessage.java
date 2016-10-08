package barnetdavi.keycrack.shared.messages;

import barnetdavi.keycrack.shared.MessageType;
import barnetdavi.keycrack.shared.Util;
import com.sun.xml.internal.ws.policy.privateutil.PolicyUtils;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class RequestKeysMessage extends Message {

    private final int chunkSize;

    public RequestKeysMessage(int chunkSize, boolean includeCipher) {
        super(includeCipher ? MessageType.REQUEST_KEYS_WITH_CIPHER :  MessageType.REQUEST_KEYS);
        this.chunkSize = chunkSize;
    }

    @Override
    public void writeContents(OutputStream contents) throws IOException {
        contents.write(getType().toByte());
        contents.write(Util.toByteArray(this.chunkSize));
    }

    public static RequestKeysMessage fromInputStream(MessageType type, InputStream contents) throws IOException {
        boolean includeCipher = type == MessageType.REQUEST_KEYS_WITH_CIPHER;

        byte[] buffer = new byte[4];
        contents.read(buffer);
        int chunkSize = Util.fromByteArray(buffer);

        return new RequestKeysMessage(chunkSize, includeCipher);
    }

    public int getChunkSize() {
        return chunkSize;
    }

    @Override
    public String toString() {
        return "RequestKeysMessage{" +
                "type=" + getType() +
                ", chunkSize=" + chunkSize +
                '}';
    }
}
