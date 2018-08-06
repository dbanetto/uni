import java.io.IOException;
import java.util.*;

/**
 * Created by drb on 05/06/15.
 */
public abstract class FileBPlusTree<K extends Comparable<K>,V> implements Iterable<Map.Entry<K,V>> {
    private final static int NULL_POINTER = Integer.MIN_VALUE;
    private final static int INT_SIZE = 4;

    private enum BlockType {
        ROOT_NODE,
        INTERNAL_NODE,
        LEAF_NODE;

        public static byte toByte(BlockType val) {
            for (byte i = 0; i < BlockType.values().length; i++) {
                if (val == BlockType.values()[i]) {
                    return i;
                }
            }
            throw new IllegalArgumentException();
        }

        public static BlockType fromByte(byte val) {
            return BlockType.values()[val];
        }
    }

    private final int blockSize;
    final int sizeOfKey;
    final int sizeOfValue;
    private final BlockFile file;

    public FileBPlusTree(String path, int blockSize, int sizeOfKey, int sizeOfValue) throws IOException {
        this.blockSize = blockSize;
        this.sizeOfKey = sizeOfKey;
        this.sizeOfValue = sizeOfValue;
        file = new BlockFile(path, blockSize);

        if (this.file.getSize() == 0) {
            // init file
            Block metadata = new Block(blockSize);
            // write meta data, index of the 1st node (block 1)
            metadata.setInt(1,0);
            this.file.write(metadata.getBytes());

            // init 1st block
            int index = allocateBlock();
            new FileLeafNode(index, NULL_POINTER, NULL_POINTER).write(file);
        }
    }

    /**
     * Returns the String associated with the given key,
     * or null if the key is not in the B+ tree.
     */
    public final V find(K key) {
        try {
            int rootIndex = getRootIndex();
            return (V)parseNode(rootIndex).find(key);
        } catch (IOException ex) {
            System.out.println(ex.getStackTrace());
        }
        return null;
    }

    /**
     * Stores the value associated with the key in the B+ tree.
     * If the key is already present, replaces the associated value.
     * If the key is not present, adds the key with the associated value
     *
     * @param key
     * @param value
     * @return whether pair was successfully added.
     */
    public final boolean put(K key, V value) {
        byte[] val = valueToBytes(value);
        byte[] k = keyToBytes(key);
        if (val.length > sizeOfValue || k.length > sizeOfKey) {
            throw new IllegalArgumentException();
        }
        try {
            int index = getRootIndex();
            setRootIndex(parseNode(index).put(key, value));
            return true;
        } catch (IOException ex) {
            System.out.println(ex.getStackTrace());
            return false;
        }

    }

    private Node parseNode(int index) throws IOException {
        Block block = new Block(file.read(index));
        BlockType meta = BlockType.fromByte(block.getByte(0));
        if (meta == BlockType.LEAF_NODE) {
            return parseLeafNode(index);
        } else if (meta == BlockType.INTERNAL_NODE) {
            return parseInternalNode(index);
        }
        throw new RuntimeException();
    }

    private void setRootIndex(int newRootIndex) throws IOException {
        Block metadata = new Block(blockSize);
        metadata.setInt(newRootIndex,0);
        this.file.write(metadata.getBytes(), 0);
    }

    private int getRootIndex() throws IOException {
        Block metadata = new Block(file.read(0));
        return metadata.getInt(0);
    }

    /**
     * Returns the index of the new block
     * @return
     * @throws IOException
     */
    private int allocateBlock() throws IOException {
        Block b = new Block(blockSize);
        return file.write(b.getBytes());
    }

    @Override
    public Iterator<Map.Entry<K, V>> iterator() {
        try {
            Node<K, V> node = parseNode(getRootIndex());
            while (node.getType() == BlockType.INTERNAL_NODE) {
                FileInternalNode internal = (FileInternalNode)node;
                node = parseNode(internal.children.get(0));
            }
            FileLeafNode leaf = (FileLeafNode)node;
            return leaf.iterator();
        } catch (IOException ex){
            throw new RuntimeException(ex);
        }
    }

    private int findIndex (K key, List<K> keys) {
        int i = Collections.binarySearch(keys, key);
        if (i < 0) {
            i = -i - 1;
        }
        return i;
    }

    public abstract byte[] keyToBytes(K key);
    public abstract byte[] valueToBytes(V value);
    public abstract K bytesToKey(byte[] bytes);
    public abstract V bytesToValue(byte[] bytes);

    private interface Node<K,V> {
        /**
         * @param key
         * @param value
         * @return index of parent node
         */
        int put(K key, V value) throws IOException;
        V find(K key) throws IOException;
        void write(BlockFile file) throws IOException;
        int splitSize();
        BlockType getType();
    }

    FileLeafNode parseLeafNode(int index) throws IOException {
        Block block = new Block(file.read(index));
        int parent;
        int rightNodeIndex;
        SortedMap<K, V> values = new TreeMap<>();

        // meta-data byte
        BlockType meta = BlockType.fromByte(block.getByte(0));
        if (meta != BlockType.LEAF_NODE) {
            throw new IllegalArgumentException();
        }
        int i = 1;

        parent = block.getInt(i);
        i += INT_SIZE;

        rightNodeIndex = block.getInt(i);
        i += INT_SIZE;

        int keyCount = block.getInt(i);
        i += INT_SIZE;

        for (int n = 0; n < keyCount; n++) {
            K k =  bytesToKey(block.getBytes(i, sizeOfKey));
            i += sizeOfKey;
            V val = bytesToValue(block.getBytes(i, sizeOfValue));
            i += sizeOfValue;

            values.put(k, val);
        }
        return new FileLeafNode(index, values, parent, rightNodeIndex);
    }

    FileInternalNode parseInternalNode(int index) throws IOException {
        Block block = new Block(file.read(index));
        int parent;
        List<K> keys = new ArrayList<>();
        List<Integer> indices = new ArrayList<>();

        // meta-data byte
        BlockType meta = BlockType.fromByte(block.getByte(0));
        if (meta != BlockType.INTERNAL_NODE) {
            throw new IllegalArgumentException();
        }
        int i = 1;

        parent = block.getInt(i);
        i += INT_SIZE;

        int keyCount = block.getInt(i);
        i += INT_SIZE;

        indices.add(block.getInt(i));
        i += INT_SIZE;
        for (int n = 0; n < keyCount; n++) {
            keys.add(bytesToKey(block.getBytes(i, sizeOfKey)));
            i += sizeOfKey;
            indices.add(block.getInt(i));
            i += INT_SIZE;

        }
        return new FileInternalNode(index, parent, keys, indices);
    }

    private class FileInternalNode implements Node<K,V> {

        int parent;
        List<K> keys;
        List<Integer> children;
        final int index;

        FileInternalNode(int index, int parent, List<K> keys, List<Integer> children) {
            this.index = index;
            this.parent = parent;
            this.keys = keys;
            this.children = children;
            if (keys.size() + 1 != children.size()) {
                throw new AssertionError();
            }
        }

        FileInternalNode(int index, int parent, int leftMostIndex) {
            this.index = index;
            this.parent = parent;
            this.keys = new ArrayList<>();
            this.children = new ArrayList<>();
            children.add(leftMostIndex);
        }

        @Override
        public int put(K key, V value) throws IOException {

            for (int index = 0; index <= keys.size(); index++) {
                if (index >= keys.size()) {
                    parseNode(children.get(keys.size())).put(key, value);
                    break;
                }

                if (key.compareTo(keys.get(index)) < 0) {
                    parseNode(children.get(index)).put(key, value);
                    break;
                }
            }
            return (parent == NULL_POINTER ? index : parent);
        }

        @Override
        public V find(K key) throws IOException {
            for (int index = 0; index <= keys.size(); index++) {
                if (index >= keys.size()) {
                    return (V)parseNode(children.get(keys.size())).find(key);
                }

                if (key.compareTo(keys.get(index)) < 0) {
                    return (V)parseNode(children.get(index)).find(key);
                }
            }
            throw new AssertionError();
        }

        @Override
        public void write(BlockFile file) throws IOException {
            Block block = new Block(blockSize);
            int i = 0;
            block.setByte(BlockType.toByte(BlockType.INTERNAL_NODE), 0);
            i += 1;

            block.setInt(parent, i);
            i += INT_SIZE;

            block.setInt(keys.size(), i);
            i += INT_SIZE;

            block.setInt(children.get(0), i);
            i += INT_SIZE;

            for (int c = 0; c < keys.size(); c++) {
                byte[] keyBytes = keyToBytes(keys.get(c));
                if (keyBytes.length > sizeOfKey) {
                    throw new IllegalArgumentException();
                }
                for (int b = 0; b < sizeOfKey; b++) {
                    if (b < keyBytes.length) {
                        block.setByte(keyBytes[b], i + b);
                    } else {
                        break;
                    }
                }
                i += sizeOfKey;

                block.setInt(children.get(c + 1), i);
                i += INT_SIZE;
            }
            file.write(block.getBytes(), index);
        }

        @Override
        public int splitSize() {
            // meta-data(1), parent(4), keys count(4), 1st key(4)
            return (blockSize - 1 - 4 - 4 - 4) / (sizeOfKey + 4);
        }

        public void promoteRight(K key, int index) throws IOException  {
            int i = 0;
            for (; i <= keys.size(); i++) {
                if (i >= keys.size()) {
                    break;
                }

                if (key.compareTo(keys.get(i)) < 0) {
                    break;
                }
            }

            keys.add(i, key);
            children.add(i + 1, index);
            if (keys.size() >= splitSize()) {
                this.split();
            } else {
                this.write(file);
            }
        }

        private void split() throws IOException {
            int midpoint = (int) Math.floor(splitSize() / 2.0);

            // right is greater or equal to midkey
            List<K> leftKeys = new ArrayList<>(keys.subList(0, midpoint));
            List<Integer> leftChildren = new ArrayList<>(children.subList(0, midpoint+1));

            K midKey = keys.get(midpoint);

            List<K> rightKeys = new ArrayList<>(keys.subList(midpoint+1, keys.size()));
            List<Integer> rightChildren = new ArrayList<>(children.subList(midpoint+1, children.size()));

            this.keys = leftKeys;
            this.children = leftChildren;

            // promote midkey to an InternalNode
            if (parent == NULL_POINTER) {
                int parentIndex = allocateBlock();
                this.parent = parentIndex;
                new FileInternalNode(parentIndex, NULL_POINTER, index).write(file);
            }
            int rightIndex = allocateBlock();
            FileInternalNode rightNode = new FileInternalNode(rightIndex, parent, rightKeys, rightChildren);
            rightNode.write(file);

            // maintain parent pointers
            for (int child : rightChildren) {
                try {
                    Node<K,V> node = parseNode(child);
                    if (node.getType() == BlockType.INTERNAL_NODE) {
                        FileInternalNode internalNode = (FileInternalNode)node;
                        internalNode.parent = rightIndex;
                        internalNode.write(file);
                    } else if (node.getType() == BlockType.LEAF_NODE) {
                        FileLeafNode internalNode = (FileLeafNode)node;
                        internalNode.parent = rightIndex;
                        internalNode.write(file);
                    }
                } catch (IOException ex) {
                    System.out.println(ex);
                }
            }

            this.write(file);
            parseInternalNode(parent).promoteRight(midKey, rightIndex);
            System.out.println("split int " + this  + " | " + midKey + " | " + rightNode);
        }

        @Override
        public String toString() {
            if (keys.size() > 1)
                return "{" + this.keys.get(0) +  "..." + this.keys.get(this.keys.size() - 1)+ "}";
            else if (keys.size() > 1)
                return "{"+ this.keys.get(0) +"}";
            else
                return "{}";
        }

        @Override
        public BlockType getType() {
            return BlockType.INTERNAL_NODE;
        }
    }

    private class FileLeafNode implements Node<K,V>, Iterable<Map.Entry<K,V>> {

        int parent;
        int rightNodeIndex;
        SortedMap<K, V> values;
        final int index;

        FileLeafNode(int index, SortedMap<K, V> values, int parent, int rightNodeIndex) {
            this.index = index;
            this.parent = parent;
            this.values = values;
            this.rightNodeIndex = rightNodeIndex;
        }

        FileLeafNode(int index, int parent, int rightNodeIndex) {
            this.index = index;
            this.parent = parent;
            this.rightNodeIndex = rightNodeIndex;
            this.values = new TreeMap<>();
        }

        @Override
        public int splitSize() {
            // meta-data(1), parent(4), right node index(4), count(4)
            return (blockSize - 1 - 4 - 4 - 4) / (sizeOfKey + sizeOfValue);
        }

        @Override
        public int put(K key, V value) throws IOException {
            values.put(key, value);

            if (values.size() > splitSize()) {
                split();
            } else {
                this.write(file);
            }
            return (parent == NULL_POINTER ? index : parent);
        }

        @Override
        public V find(K key) {
            return values.get(key);
        }

        private void split() throws IOException {
            int midpoint = (int) Math.floor(splitSize() / 2.0);

            // right is greater or equal to midkey
            SortedMap<K, V> right = new TreeMap<>();
            SortedMap<K, V> left = new TreeMap<>();

            for (Map.Entry<K, V> entry : this.values.entrySet()) {
                if (left.size() < midpoint) {
                    left.put(entry.getKey(), entry.getValue());
                } else {
                    right.put(entry.getKey(), entry.getValue());
                }
            }
            this.values = left;

            // promote midkey to an InternalNode
            if (parent == NULL_POINTER) {
                int parentIndex = allocateBlock();
                parent = parentIndex;
                new FileInternalNode(parentIndex, NULL_POINTER, index).write(file);
            }
            int rightIndex = allocateBlock();
            FileLeafNode rightNode = new FileLeafNode(rightIndex, right, parent, this.rightNodeIndex);
            rightNode.write(file);

            this.rightNodeIndex = rightIndex;
            this.write(file);
            parseInternalNode(parent).promoteRight(right.firstKey(), rightIndex);
        }

        public void write(BlockFile file) throws IOException {
            Block block = new Block(blockSize);
            int i = 0;
            block.setByte(BlockType.toByte(BlockType.LEAF_NODE), 0);
            i += 1;
            block.setInt(parent, i);
            i += INT_SIZE;
            block.setInt(rightNodeIndex, i);
            i += INT_SIZE;
            block.setInt(values.size(), i);
            i += INT_SIZE;

            for (Map.Entry<K, V> entry : values.entrySet()) {
                byte[] keyBytes = keyToBytes(entry.getKey());
                if (keyBytes.length > sizeOfKey) {
                    throw new IllegalArgumentException();
                }
                for (int b = 0; b < sizeOfKey; b++) {
                    if (b < keyBytes.length) {
                        block.setByte(keyBytes[b], i + b);
                    } else {
                        break;
                    }
                }
                i += sizeOfKey;

                byte[] valBytes = valueToBytes(entry.getValue());
                if (valBytes.length > sizeOfValue) {
                    throw new IllegalArgumentException();
                }
                for (int b = 0; b < sizeOfValue; b++) {
                    if (b < valBytes.length) {
                        block.setByte(valBytes[b], i + b);
                    } else {
                        break;
                    }
                }
                i += sizeOfValue;
            }
            file.write(block.getBytes(), index);
        }

        public String toString() {
            return "Leaf{" + this.values.get(0) +  "..." + this.values.get(this.values.size() - 1) + "}";
        }

        @Override
        public BlockType getType() {
            return BlockType.LEAF_NODE;
        }

        @Override
        public Iterator<Map.Entry<K, V>> iterator() {
            return new Iterator<Map.Entry<K, V>>() {
                Iterator<Map.Entry<K,V>> internal = values.entrySet().iterator();
                int nextIndex = rightNodeIndex;

                @Override
                public boolean hasNext() {
                    if (!internal.hasNext()) {
                        return nextIndex != NULL_POINTER;
                    }
                    return internal.hasNext();
                }

                @Override
                public Map.Entry<K, V> next() {
                    if (!internal.hasNext()) {
                        if (nextIndex != NULL_POINTER) {
                            try {
                                FileLeafNode nextNode = parseLeafNode(nextIndex);
                                internal = nextNode.values.entrySet().iterator();
                                nextIndex = nextNode.rightNodeIndex;
                            } catch (IOException e) {
                                throw new RuntimeException(e);
                            }
                        } else {
                            throw new ArrayIndexOutOfBoundsException();
                        }
                    }
                    return internal.next();
                }
            };

        }
    }

}
