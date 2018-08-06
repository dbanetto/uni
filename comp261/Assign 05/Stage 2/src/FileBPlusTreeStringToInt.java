import java.io.IOException;

/**
 * Created by drb on 05/06/15.
 */
public class FileBPlusTreeStringToInt extends FileBPlusTree<String, Integer> {

    public FileBPlusTreeStringToInt(String path, int blockSize) throws IOException {
        super(path, blockSize, 255, 4);
    }

    @Override
    public byte[] valueToBytes(Integer key) {
        byte[] bytes = new byte[4];
        bytes[0] = (byte) (key >>> 24);
        bytes[1] = (byte) (key >>> 16 & 0xff);
        bytes[2] = (byte) (key >>> 8 & 0xff);
        bytes[3] = (byte) (key & 0xff);
        return bytes;
    }

    @Override
    public byte[] keyToBytes(String value) {
        return value.getBytes();
    }

    @Override
    public Integer bytesToValue(byte[] bytes) {
        return (bytes[0] << 24) | ((bytes[1] & 0xff) << 16) | ((bytes[2] & 0xff) << 8) | (bytes[3] & 0xff);
    }

    @Override
    public String bytesToKey(byte[] bytes) {
        return new String(bytes).trim();
    }
}
