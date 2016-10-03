package barnetdavi.keycrack.manager;

public class KeyManager {

    public static void main(String[] args) {
        if (args.length != 4) {
            System.err.println("Expected 3 arguments");
            System.exit(1);
        }

        int initalKey = Integer.parseInt(args[1]);
        int keySize = Integer.parseInt(args[2]);

        String chipherText = args[3];

    }
}
