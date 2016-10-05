package barnetdavi.keycrack.shared;

import java.math.BigInteger;

public class Util {

    private Util() {
        throw new IllegalStateException();
    }

    public static byte[] toByteArray(int value) {
        return new byte[]{
                (byte) (value >>> 24),
                (byte) (value >>> 16),
                (byte) (value >>> 8),
                (byte) value
        };
    }

    public static int fromByteArray(byte[] value) {
        if (value.length != 4) {
            throw new IllegalArgumentException();
        }

        return new BigInteger(value).intValue();
    }

    public static byte[] toByteArray(BigInteger value, int arraySize) {
        byte[] buffer = new byte[arraySize];
        byte[] valueBuffer = value.toByteArray();

        if (valueBuffer.length > buffer.length) {
            throw new IllegalArgumentException();
        }

        for (int i = 1; i <= valueBuffer.length; i++) {
            buffer[buffer.length - i] = valueBuffer[valueBuffer.length - i];
        }
        return buffer;
    }
}
