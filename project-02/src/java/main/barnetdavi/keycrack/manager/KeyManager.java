package barnetdavi.keycrack.manager;

import barnetdavi.keycrack.shared.Blowfish;

import java.io.IOException;
import java.math.BigInteger;
import java.net.InterfaceAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;

public class KeyManager {

    public static void main(String[] args) {
        if (args.length != 3) {
            System.err.println("Expected 3 arguments");
            System.exit(1);
        }

        BigInteger initalKey = new BigInteger(args[0]);
        int keySize = Integer.parseInt(args[1]);

        String chipherText = args[2];

        KeyManager manager = new KeyManager(initalKey, keySize, Blowfish.fromBase64(chipherText));
        manager.run();
    }

    private final BigInteger initialKey;
    private final int keySize;
    private final byte[] chiphertext;
    private ExecutorService threadPool;

    public KeyManager(BigInteger initialKey, int keySzie, byte[] chiphertext) {
        this.initialKey = initialKey;
        this.keySize = keySzie;
        this.chiphertext = chiphertext;

        threadPool = java.util.concurrent.Executors.newCachedThreadPool();
    }

    public void run() {
        ServerSocket serverSocket = null;

        try {

            int port = 0;
            try {
                // allow the port number to be hinted
                String portHint = System.getenv("SERVER_PORT");
                if (portHint != null) {
                    port = Integer.parseInt(portHint);
                }
            } catch (NumberFormatException ex) {
                System.out.println("SERVER_PORT variable is not a integer and thus ignored");
            }

            serverSocket = new ServerSocket(port);

            System.out.printf("Running on %s:%d", serverSocket.getInetAddress().getHostAddress(), serverSocket.getLocalPort());

            while (true) {
                Socket connectionSocket = serverSocket.accept();

                Connection connection = new Connection(this, connectionSocket);
                threadPool.submit(connection);
            }

        } catch (IOException ex) {
            System.out.println("IO Error:" + ex.toString());
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

    int getKeySize() {
        return keySize;
    }

    byte[] getChiphertext() {
        return chiphertext;
    }
}
