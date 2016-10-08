package barnetdavi.keycrack.manager;

import barnetdavi.keycrack.shared.Blowfish;
import barnetdavi.keycrack.shared.KeySpace;

import java.io.IOException;
import java.math.BigInteger;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;

public class KeyManager {

    /**
     *
     * args[0] -> initial key to try
     * args[1] -> key size in number of bytes
     * args[2] -> cipher text in Base64
     * args[3] -> decrypted plain text from the cipher text
     * @param args
     */
    public static void main(String[] args) {
        if (args.length != 4) {
            System.err.println("Expected 4 arguments");
            System.exit(1);
        }

        BigInteger initialKey = new BigInteger(args[0]);
        int keySize = Integer.parseInt(args[1]);

        String cipherText = args[2];
        // This is from a talk with Matt.
        String plainText = args[3];

        KeyManager manager = new KeyManager(initialKey, keySize, Blowfish.fromBase64(cipherText), plainText);
        manager.run();
    }

    private BigInteger currentKey;
    private final int keySize;
    private final byte[] cipherText;
    private final AtomicBoolean keyFound = new AtomicBoolean(false);
    private final String plainText;
    private ExecutorService threadPool;

    public KeyManager(BigInteger currentKey, int keySize, byte[] cipherText, String plainText) {
        this.currentKey = currentKey;
        this.keySize = keySize;
        this.cipherText = cipherText;
        this.plainText = plainText;

        threadPool = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
    }

    public void run() {
        ServerSocket serverSocket = null;

        try {
            serverSocket = new ServerSocket(getPort());
            serverSocket.setSoTimeout(1000);
            // Tell the user what port we are running on
            System.out.printf("Running on %s:%d\n", serverSocket.getInetAddress().getHostAddress(), serverSocket.getLocalPort());

            while (!keyFound.get()) {
                try {
                    Socket connectionSocket = serverSocket.accept();

                    Connection connection = new Connection(this, connectionSocket);
                    threadPool.submit(connection);
                } catch (SocketTimeoutException timeout) {
                    // accept() timeout hit, allows us to not block for ever if a single client
                    // finds the key so we can terminate the while loop.
                }
            }

        } catch (IOException ex) {
            System.out.printf("IO Error: %s\n", ex.toString());
        } finally {
            if (serverSocket != null) {
                try {
                    serverSocket.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

        // prepare for shutdown
        threadPool.shutdown();
    }

    /**
     * Get the port number to use
     *
     * @return 0, unless SERVER_PORT environment variable is set to a valid value
     */
    private int getPort() {
        try {
            // allow the port number to be hinted
            String portHint = System.getenv("SERVER_PORT");
            if (portHint != null) {
                return Integer.parseInt(portHint);
            }
        } catch (NumberFormatException ex) {
            System.out.println("SERVER_PORT variable is not a integer and thus ignored");
        }
        return 0;
    }

    int getKeySize() {
        return keySize;
    }

    byte[] getCipherText() {
        return cipherText;
    }

    public synchronized List<KeySpace> requestKeys(int chunkSize) {
        BigInteger current = currentKey;
        BigInteger limit = current.add(BigInteger.valueOf(chunkSize));
        currentKey = limit;

        // record when this was sent out

        return Collections.singletonList(new KeySpace(current, limit));
    }

    public String getPlainText() {
        return plainText;
    }

    public void keyFound(BigInteger key) {
        this.keyFound.set(true);

        System.out.println("Found key: " + key);
    }
}
