package barnetdavi.keycrack.shared.messages;

import barnetdavi.keycrack.shared.MessageType;
import barnetdavi.keycrack.shared.Util;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class SetupClientMessage extends Message {

    private final byte[] ciphertext;

    public SetupClientMessage(byte[] cipherText) {
        super(MessageType.SETUP_CLIENT);
        this.ciphertext = cipherText;
    }

    public byte[] getCipherText() {
        return ciphertext;
    }

    @Override
    public void writeContents(OutputStream contents) throws IOException {
        contents.write(getType().toByte());
        contents.write(Util.toByteArray(ciphertext.length));
        contents.write(ciphertext);
    }

    static SetupClientMessage fromInputStream(InputStream content) throws IOException {

        byte[] buffer = new byte[4];
        content.read(buffer);
        int length = Util.fromByteArray(buffer);

        buffer = new byte[length];

        content.read(buffer);

        return new SetupClientMessage(buffer);
    }
}
