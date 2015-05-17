import java.util.*;

/**
 * Created by drb on 17/05/15.
 */
public class BPlusTree<K extends Comparable, V> {
    private static final int degree = 100;
    private final Comparator<K> comparator = new Comparator<K>() {
        @Override
        public int compare(K o1, K o2) {
            return o1.compareTo(o2);
        }
    };

    private BPlusTreeNode<K, V> rootNode;

    public BPlusTree() {
        rootNode = new BPlusTreeLeafNode<>();
    }

    /**
     * Returns the String associated with the given key,
     * or null if the key is not in the B+ tree.
     */
    public V find(K key) {
        return rootNode.find(key);
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
    public boolean put(K key, V value) {
        return rootNode.put(key, value);
    }

    private interface BPlusTreeNode<K, V> {
        boolean needsSplit();

        boolean needsMerge();

        Map.Entry<K, V> remove(K key);

        V find(K key);

        boolean put(K key, V value);
    }

    private class BPlusTreeInternalNode<K extends Comparable> implements BPlusTreeNode<K, V> {
        private List<K> keys = new ArrayList<>(degree);
        private List<BPlusTreeLeafNode<K, V>> children = new ArrayList<>(degree);

        @Override
        public boolean needsSplit() {
            return keys.size() >= (degree - 1);
        }

        @Override
        public boolean needsMerge() {
            return keys.size() < (degree - 1) / 2;
        }

        @Override
        public Map.Entry<K, V> remove(K key) {
            return null;
        }

        @Override
        public V find(K key) {
            return null;
        }

        @Override
        public boolean put(K key, V value) {
            return false;
        }
    }

    private class BPlusTreeLeafNode<K extends Comparable, V> implements BPlusTreeNode<K, V> {
        // Maintain a sorted map
        SortedMap<K, V> values = new TreeMap<>((Comparator<? super K>) comparator);
        BPlusTreeLeafNode<K, V> next;

        @Override
        public boolean needsSplit() {
            return values.size() >= degree;
        }

        @Override
        public boolean needsMerge() {

            return values.size() < degree / 2;
        }

        @Override
        public Map.Entry<K, V> remove(K key) {
            return null;
        }

        @Override
        public V find(K key) {
            return null;
        }

        @Override
        public boolean put(K key, V value) {
            return false;
        }
    }


}
