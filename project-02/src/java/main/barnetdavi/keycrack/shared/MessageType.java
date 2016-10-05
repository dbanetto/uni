package barnetdavi.keycrack.shared;


public enum MessageType {
    // client -> server, on 1st connection
    CLIENT_INTRODUCTION,
    // server -> client, response to CLIENT_INTRODUCTION
    SETUP_CLIENT,;

    public byte toByte() {
        switch (this) {
            case CLIENT_INTRODUCTION:
                return 0x00;
            case SETUP_CLIENT:
                return 0x01;
            default:
                throw new IllegalStateException();
        }
    }

    public static MessageType fromByte(byte value) {
        switch (value) {
            case (0x00):
                return CLIENT_INTRODUCTION;
            case (0x01):
                return SETUP_CLIENT;
            default:
                throw new IllegalStateException();
        }
    }
}
