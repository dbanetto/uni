package barnetdavi.keycrack.shared.messages;

import barnetdavi.keycrack.shared.MessageType;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class PostResultsMessage extends Message {

    public PostResultsMessage() {
        super(MessageType.POST_RESULTS);
    }

    @Override
    public void writeContents(OutputStream contents) throws IOException {

    }

    public static PostResultsMessage fromInputStream(InputStream contents) {
        return null;
    }


}
