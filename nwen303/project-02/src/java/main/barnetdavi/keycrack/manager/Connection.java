package barnetdavi.keycrack.manager;

import barnetdavi.keycrack.shared.KeySpace;
import barnetdavi.keycrack.shared.MessageType;
import barnetdavi.keycrack.shared.messages.KeyBlockMessage;
import barnetdavi.keycrack.shared.messages.Message;
import barnetdavi.keycrack.shared.messages.PostResultsMessage;
import barnetdavi.keycrack.shared.messages.RequestKeysMessage;

import java.util.List;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.net.Socket;

public class Connection implements Runnable {
    private final KeyManager manager;
    private final Socket clientSocket;

    private OutputStream outputStream;

    public Connection(KeyManager manager, Socket clientSocket) {
        this.clientSocket = clientSocket;
        this.manager = manager;
    }

    @Override
    public void run() {
        try {
            InputStream inputStream = clientSocket.getInputStream();
            outputStream = clientSocket.getOutputStream();

            Message received = Message.readFromStream(inputStream, manager.getKeySize());

            handleMessage(received);

        } catch (Exception ex) {
            System.err.printf("Error in main loop client : %s\n", ex.toString());
            ex.printStackTrace();
        } finally {
             if (clientSocket != null) {
                 try {
                     clientSocket.close();
                 } catch (IOException e) {
                     e.printStackTrace();
                 }
             }
        }
    }

    private void handleMessage(Message received) throws IOException {
        // System.out.println("Got: " + received);

        if (received.getType() == MessageType.REQUEST_KEYS_WITH_CIPHER || received.getType() == MessageType.REQUEST_KEYS) {
            RequestKeysMessage request = (RequestKeysMessage) received;

            List<KeySpace> keys = manager.requestKeys(request.getChunkSize());

            Message toSend;
            if (request.getType() == MessageType.REQUEST_KEYS_WITH_CIPHER) {
                toSend = new KeyBlockMessage(keys, manager.getKeySize(), manager.getCipherText(), manager.getPlainText());
            } else {
                toSend = new KeyBlockMessage(keys, manager.getKeySize());
            }
            toSend.writeContents(outputStream);

        } else if (received.getType() == MessageType.POST_RESULTS) {
            // TODO: register that keys failed

        } else if (received.getType() == MessageType.POST_RESULTS_FOUND) {
            PostResultsMessage results = (PostResultsMessage)received;

            // TODO: FOUND IT
            manager.keyFound(results.getFoundKey());
        }

    }
}
