package barnetdavi.keycrack.manager;

import barnetdavi.keycrack.shared.Blowfish;
import barnetdavi.keycrack.shared.KeySpace;

import java.io.IOException;
import java.math.BigInteger;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicBoolean;

public class KeyManager {

    public static void main(String[] args) {
        if (args.length != 3) {
            System.err.println("Expected 3 arguments");
            System.exit(1);
        }

        BigInteger initialKey = new BigInteger(args[0]);
        int keySize = Integer.parseInt(args[1]);

        String cipherText = args[2];

        KeyManager manager = new KeyManager(initialKey, keySize, Blowfish.fromBase64(cipherText));
        manager.run();
    }

    private BigInteger currentKey;
    private final int keySize;
    private final byte[] cipherText;
    private final AtomicBoolean keyFound = new AtomicBoolean(false);
    private ExecutorService threadPool;

    public KeyManager(BigInteger currentKey, int keySize, byte[] cipherText) {
        this.currentKey = currentKey;
        this.keySize = keySize;
        this.cipherText = cipherText;

        threadPool = java.util.concurrent.Executors.newCachedThreadPool();
    }

    public void run() {
        ServerSocket serverSocket = null;

        try {
            serverSocket = new ServerSocket(getPort());
            // Tell the user what port we are running on
            System.out.printf("Running on %s:%d\n", serverSocket.getInetAddress().getHostAddress(), serverSocket.getLocalPort());

            // TODO: check if the code is cracked
            while (!keyFound.get()) {
                Socket connectionSocket = serverSocket.accept();

                Connection connection = new Connection(this, connectionSocket);
                threadPool.submit(connection);
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
}
