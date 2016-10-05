package barnetdavi.keycrack.shared.messages;

import barnetdavi.keycrack.shared.MessageType;
import barnetdavi.keycrack.shared.Util;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class ClientIntroductionMessage extends Message {

    private final int chuncksize;

    public ClientIntroductionMessage(int chuncksize) {
        super(MessageType.CLIENT_INTRODUCTION);

        if (chuncksize <= 0) {
            throw new IllegalStateException("Chuncksize must be greater than 0");
        }

        this.chuncksize = chuncksize;
    }

    @Override
    public void writeContents(OutputStream contents) throws IOException {
        contents.write(getType().toByte());
        contents.write(Util.toByteArray(chuncksize));
    }

    static ClientIntroductionMessage fromInputStream(InputStream content) throws IOException {
        byte[] buffer = new byte[4];
        content.read(buffer);

        return new ClientIntroductionMessage(Util.fromByteArray(buffer));
    }

    public int getChuncksize() {
        return chuncksize;
    }
}
