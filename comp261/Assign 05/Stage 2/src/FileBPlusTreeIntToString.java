import java.io.IOException;

/**
 * File starts with a block of metadata that contains a flag the index of the root node
 */
public class FileBPlusTreeIntToString extends FileBPlusTree<Integer, String> {

    public FileBPlusTreeIntToString(String path, int blockSize) throws IOException {
        super(path, blockSize, 4, 255);
    }

    @Override
    public byte[] keyToBytes(Integer key) {
        byte[] bytes = new byte[4];
        bytes[0] = (byte) (key >>> 24);
        bytes[1] = (byte) (key >>> 16 & 0xff);
        bytes[2] = (byte) (key >>> 8 & 0xff);
        bytes[3] = (byte) (key & 0xff);
        return bytes;
    }

    @Override
    public byte[] valueToBytes(String value) {
        return value.getBytes();
    }

    @Override
    public Integer bytesToKey(byte[] bytes) {
        return (bytes[0] << 24) | ((bytes[1] & 0xff) << 16) | ((bytes[2] & 0xff) << 8) | (bytes[3] & 0xff);
    }

    @Override
    public String bytesToValue(byte[] bytes) {
        return new String(bytes).trim();
    }
}