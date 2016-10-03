package barnetdavi.keycrack.manager;

import barnetdavi.keycrack.shared.messages.Message;

import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.util.concurrent.atomic.AtomicInteger;

public class Connection extends Thread {
    private static AtomicInteger counter = new AtomicInteger(0);

    private final Socket clientSocket;
    private final int id;

    public Connection(Socket clientSocket) {
        this.clientSocket = clientSocket;
        this.id = counter.getAndAdd(1);
    }

    @Override
    public void run() {

        while (clientSocket.isConnected()) {
            try {
                InputStream input = clientSocket.getInputStream();

                Message received = Message.readFromStream(input);

            } catch (IOException ex) {
                System.err.printf("Error in main loop client #%d : %s", id, ex.toString());

            }
        }
    }

    public void send(Message message) {
        try {
            message.writeContents(clientSocket.getOutputStream());

        } catch (IOException ex) {
            System.err.printf("Error while sending to client #%d : %s", id, ex.toString());
        }
    }
}
