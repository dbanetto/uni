package barnetdavi.keycrack.shared;


public enum MessageType {
    // client -> server, asks for chunk size number of keys
    REQUEST_KEYS,
    // client -> server, asks for chunk size number of keys + the cipher text
    REQUEST_KEYS_WITH_CIPHER,

    // client -> server, report the results of the key space did not have the key
    POST_RESULTS,
    // client -> server, found the key
    POST_RESULTS_FOUND,

    // client -> server, the list of requested keys to try
    KEY_BLOCK,
    // client -> server, the list of requested keys to try + the cipher text
    KEY_BLOCK_WITH_CIPHER
    ;

    public byte toByte() {
        switch (this) {
            case REQUEST_KEYS:
                return 0x7F;
            case REQUEST_KEYS_WITH_CIPHER:
                return 0x70;
            case POST_RESULTS:
                return 0x35;
            case POST_RESULTS_FOUND:
                return 0x2a; // hint: 42
            case KEY_BLOCK:
                return 0x00;
            case KEY_BLOCK_WITH_CIPHER:
                return 0x10;
            default:
                throw new IllegalStateException();
        }
    }

    public static MessageType fromByte(byte value) {
        switch (value) {
            case (0x7F):
                return REQUEST_KEYS;
            case (0x70):
                return REQUEST_KEYS_WITH_CIPHER;
            case (0x35):
                return POST_RESULTS;
            case (0x2a):
                return POST_RESULTS_FOUND;
            case (0x00):
                return KEY_BLOCK;
            case (0x10):
                return KEY_BLOCK_WITH_CIPHER;
            default:
                throw new IllegalStateException();
        }
    }
}
