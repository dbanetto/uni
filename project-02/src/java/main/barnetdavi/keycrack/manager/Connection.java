package barnetdavi.keycrack.manager;

import barnetdavi.keycrack.shared.KeySpace;
import barnetdavi.keycrack.shared.MessageType;
import barnetdavi.keycrack.shared.messages.KeyBlockMessage;
import barnetdavi.keycrack.shared.messages.Message;
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
            System.out.println(received);

            handleMessage(received);

        } catch (IOException ex) {
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

        if (received instanceof RequestKeysMessage) {
            RequestKeysMessage request = (RequestKeysMessage) received;

            List<KeySpace> keys = manager.requestKeys(request.getChunkSize());

            Message toSend;
            if (request.getType() == MessageType.REQUEST_KEYS_WITH_CIPHER) {
                toSend = new KeyBlockMessage(keys, manager.getKeySize(), manager.getCipherText());
            } else {
                toSend = new KeyBlockMessage(keys, manager.getKeySize());
            }

            System.out.println("Sent: " + toSend.toString());
            toSend.writeContents(outputStream);
        }

    }
}
