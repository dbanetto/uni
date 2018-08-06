package barnetdavi.keycrack.shared;

import java.util.List;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.util.Iterator;

public class KeySpace implements Iterable<BigInteger> {
    private final BigInteger start;
    private final BigInteger limit;

    public KeySpace(BigInteger start, BigInteger limit) {
        this.start = start;
        this.limit = limit;
    }

    public void writeToStream(OutputStream stream, int keySize) throws IOException {
        stream.write(Util.toByteArray(start, keySize + 1));
        stream.write(Util.toByteArray(limit, keySize + 1));
    }

    public static KeySpace readFromStream(InputStream stream, int keySize) throws IOException {
        byte[] buffer = new byte[keySize + 1];

        stream.read(buffer);
        BigInteger start = new BigInteger(buffer);

        stream.read(buffer);
        BigInteger limit = new BigInteger(buffer);

        return new KeySpace(start, limit);
    }

    public int size() {
        return limit.subtract(start).intValue();
    }

    public boolean isEmpty() {
        return this.size() == 0;
    }

    @Override
    public Iterator<BigInteger> iterator() {
        return new KeySpaceIterator(start, limit);
    }

    class KeySpaceIterator implements Iterator<BigInteger> {
        private BigInteger index;
        private final BigInteger limit;

        public KeySpaceIterator(BigInteger start, BigInteger limit) {
            this.index = start;
            this.limit = limit;
        }

        @Override
        public boolean hasNext() {
            return index.compareTo(limit) < 0;
        }

        @Override
        public BigInteger next() {
            index = index.add(BigInteger.ONE);
            return index.subtract(BigInteger.ONE);
        }

        @Override
        public void remove() {
            throw new IllegalArgumentException();
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        KeySpace that = (KeySpace) o;

        if (start != null ? !start.equals(that.start) : that.start != null)
            return false;
        return limit != null ? limit.equals(that.limit) : that.limit == null;

    }

    @Override
    public int hashCode() {
        int result = start != null ? start.hashCode() : 0;
        result = 31 * result + (limit != null ? limit.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "KeySpace{" +
                "start=" + start +
                ", limit=" + limit +
                '}';
    }
}
