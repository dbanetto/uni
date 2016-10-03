package barnetdavi.keycrack.shared;


public enum MessageType {
    INITIAL_CONNECTION,

    ;

    public byte toByte() {
        switch (this) {
            case INITIAL_CONNECTION:
                return 0x00;
            default:
                throw new IllegalStateException();
        }
    }

    public static MessageType fromByte(byte value) {
        switch (value) {
            case (0x00):
                return INITIAL_CONNECTION;
            default:
                throw new IllegalStateException();
        }
    }
}
