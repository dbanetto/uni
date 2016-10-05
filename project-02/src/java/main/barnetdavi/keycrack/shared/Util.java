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
}
