package barnetdavi.keycrack.manager;

import barnetdavi.keycrack.shared.messages.ClientIntroductionMessage;
import barnetdavi.keycrack.shared.messages.Message;
import barnetdavi.keycrack.shared.messages.SetupClientMessage;

import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.util.concurrent.atomic.AtomicInteger;

public class Connection implements Runnable {
    private static AtomicInteger counter = new AtomicInteger(0);

    private final KeyManager manager;
    private final Socket clientSocket;
    private int id;

    public Connection(KeyManager manager, Socket clientSocket) {
        this.clientSocket = clientSocket;
        this.id = counter.getAndAdd(1);
        this.manager = manager;
    }

    @Override
    public void run() {

        try {
            InputStream input = clientSocket.getInputStream();

            Message received = Message.readFromStream(input);

            handleMessage(received);

            // close connection because we only need to handle 1 per connection
            clientSocket.close();
        } catch (IOException ex) {
            System.err.printf("Error in main loop client #%d : %s\n", id, ex.toString());

        }

    }

    private void handleMessage(Message received) throws IOException {

        if (received instanceof ClientIntroductionMessage) {
            ClientIntroductionMessage introductionMessage = (ClientIntroductionMessage) received;

            introductionMessage.getChuncksize(); // record this for the client? Or make them always send it

            // setup message
            new SetupClientMessage(this.manager.getChiphertext()).writeContents(clientSocket.getOutputStream());

            // allocate a block of keys to the client

        }

    }

    public void send(Message message) {
        try {
            message.writeContents(clientSocket.getOutputStream());

        } catch (IOException ex) {
            System.err.printf("Error while sending to client #%d : %s\n", id, ex.toString());
        }
    }
}
