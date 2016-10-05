package barnetdavi.keycrack.client;

import barnetdavi.keycrack.shared.KeySpace;
import barnetdavi.keycrack.shared.messages.Message;
import barnetdavi.keycrack.shared.messages.RequestKeysMessage;

import java.util.ArrayList;
import java.util.List;
import java.io.IOException;
import java.net.Socket;

public class Client {

    private final String hostname;
    private final int port;
    private final int chunkSize;
    private final List<KeySpace> toExplore;

    private int keySize;

    public static void main(String[] args) {
        if (args.length != 3) {
            System.out.printf("Expected 3 arguments: <hostname> <port> <chunk size>");
            System.exit(1);
        }
        String hostname = args[0];
        int port = Integer.parseInt(args[1]);
        int chunkSize = Integer.parseInt(args[2]);

        new Client(hostname, port, chunkSize).run();
    }

    public Client(String hostname, int port, int chunkSize) {
       this.hostname = hostname;
        this.port = port;
        this.chunkSize = chunkSize;
        this.toExplore = new ArrayList<>(chunkSize);
    }

    public void run() {
        Socket socket = null;

        try {
            socket = new Socket(hostname, port);
            new RequestKeysMessage(chunkSize, true).writeContents(socket.getOutputStream());

            // Get response
            Message response = Message.readFromStream(socket.getInputStream(), keySize);

            System.out.println(response);

        } catch (IOException e) {
            System.err.printf("IO Error Occurred: %s", e.toString());
            e.printStackTrace();
            System.exit(1);
        } finally {
            if (socket != null) {
                try {
                    socket.close();
                } catch (IOException e1) {
                    e1.printStackTrace();
                }
            }
        }
    }

}
