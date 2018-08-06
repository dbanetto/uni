package barnetdavi.keycrack.client;

import barnetdavi.keycrack.shared.Blowfish;
import barnetdavi.keycrack.shared.KeySpace;
import barnetdavi.keycrack.shared.MessageType;
import barnetdavi.keycrack.shared.messages.KeyBlockMessage;
import barnetdavi.keycrack.shared.messages.Message;
import barnetdavi.keycrack.shared.messages.PostResultsMessage;
import barnetdavi.keycrack.shared.messages.RequestKeysMessage;

import java.lang.reflect.Array;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.io.IOException;
import java.net.Socket;

public class Client {

    private final String hostname;
    private final int port;
    private final int chunkSize;
    private final List<KeySpace> toExplore;

    private byte[] chipherText;
    private String plainText;
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
        BigInteger foundKey = null;

        while (foundKey == null) {
            try {

                KeyBlockMessage request = requestBlock();

                for (KeySpace space : request.getKeys()) {
                    for(BigInteger key : space) {
                        Blowfish.setKey(Blowfish.asByteArray(key, keySize));

                        byte[] encryptAttempt = Blowfish.encrypt(plainText.getBytes());

                        if (Arrays.equals(chipherText, encryptAttempt)) {
                            System.out.println("key: " + key);
                            System.out.println("Found!");
                            foundKey = key;
                            break;
                        }
                    }
                }

                postResult(foundKey, request.getKeys());

            } catch (IOException e) {
                System.err.printf("IO Error Occurred: %s", e.toString());
                e.printStackTrace();
                System.exit(1);
            }
        }
    }

    private void postResult(BigInteger foundKey, List<KeySpace> keys) throws IOException {
        Socket socket = new Socket(hostname, port);

        if (foundKey != null) {
            new PostResultsMessage(keySize, foundKey).writeContents(socket.getOutputStream());
        } else {
            new PostResultsMessage(keySize, keys).writeContents(socket.getOutputStream());
        }

        socket.close();
    }

    private KeyBlockMessage requestBlock() throws IOException {
        Socket socket = new Socket(hostname, port);
        new RequestKeysMessage(chunkSize, chipherText == null).writeContents(socket.getOutputStream());

        // Get response
        Message response = Message.readFromStream(socket.getInputStream(), keySize);

        socket.close();

        KeyBlockMessage request = (KeyBlockMessage) response;

        if (request.getType() == MessageType.KEY_BLOCK_WITH_CIPHER) {
            chipherText = request.getCipherText();
            keySize = request.getKeySize();
            plainText = request.getPlainText();
        }

        return request;
    }
}
