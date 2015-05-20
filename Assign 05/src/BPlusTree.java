import java.util.*;

/**
 * Created by drb on 17/05/15.
 */
public class BPlusTree<K extends Comparable, V> {
    private static final int leafDegree = 100; // number of K-V pairs
    private static final int internalDegree = 100; // number of keys

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
        BPlusTreeNode<K, V> newRootNode = rootNode.put(key, value);
        // debug: sanity check, new root cannot be a leaf node after 1st split
        if (newRootNode != rootNode && newRootNode instanceof BPlusTreeLeafNode) {
            throw new AssertionError();
        }
        rootNode = newRootNode;
        return true;
    }

    private interface BPlusTreeNode<K, V> {
        boolean needsSplit();

        boolean needsMerge();

        Map.Entry<K, V> remove(K key);

        V find(K key);

        BPlusTreeNode<K, V> put(K key, V value);
    }

    private class BPlusTreeInternalNode<K extends Comparable> implements BPlusTreeNode<K, V> {
        private BPlusTreeInternalNode<K> parent = null;
        private List<K> keys = new ArrayList<>(internalDegree);
        private List<BPlusTreeNode<K, V>> children = new ArrayList<>(internalDegree + 1);

        public BPlusTreeInternalNode(BPlusTreeNode<K, V> left) {
            children.add(left);
        }

        private BPlusTreeInternalNode(BPlusTreeInternalNode<K> parent,List<K> keys, List<BPlusTreeNode<K, V>> children) {
            this.parent = parent;
            this.keys = keys;
            this.children = children;
        }

        @Override
        public boolean needsSplit() {
            return keys.size() >= internalDegree;
        }

        @Override
        public boolean needsMerge() {
            return keys.size() < (int) Math.floor((double) internalDegree / 2.0);
        }

        @Override
        public Map.Entry<K, V> remove(K key) {
            return null;
        }

        @Override
        public V find(K key) {
            for (int i = 0; i < children.size() - 1; i++) {
                if (key.compareTo(keys.get(i)) < 0) {
                    return children.get(i).find(key);
                }
            }
            return children.get(children.size() - 1).find(key);
        }

        @Override
        public BPlusTreeNode<K, V> put(K key, V value) {
            for (int i = 0; i < children.size() - 1; i++) {
                if (key.compareTo(keys.get(i)) < 0) {
                    children.get(i).put(key, value);
                    return (parent == null ? this : parent);
                }
            }
            children.get(children.size() - 1).put(key, value);
            return (parent == null ? this : parent);
        }

        public void promoteRight(K key, BPlusTreeNode<K, V> node) {
            int i = 0;
            for (; i < children.size() - 1; i++) {
                if (key.compareTo(keys.get(i)) < 0) {
                    break;
                }
            }
            keys.add(i, key);
            children.add(i + 1, node);

            if (needsSplit()) {
                System.out.println("split internal node");
                int midpoint = (int) Math.floor(internalDegree / 2.0);

                // right is greater or equal to midkey
                List<K> rightKeys = new ArrayList<>(internalDegree);
                List<BPlusTreeNode<K, V>> rightChildren = new ArrayList<>(internalDegree + 1);

                List<K> leftKeys = new ArrayList<>(internalDegree);
                List<BPlusTreeNode<K, V>> leftChildren = new ArrayList<>(internalDegree + 1);

                Iterator<K> keyIter = keys.iterator();
                Iterator<BPlusTreeNode<K, V>> childernIter = children.iterator();
                leftChildren.add(childernIter.next());

                K midKey = null;
                for (i = 0; i < internalDegree; i++) {
                    if (i < midpoint) {
                        leftKeys.add(keyIter.next());
                        leftChildren.add(childernIter.next());
                    } else if (i == midpoint) {
                        midKey = keyIter.next();
                        rightChildren.add(childernIter.next());
                    } else {
                        rightKeys.add(keyIter.next());
                        rightChildren.add(childernIter.next());
                    }
                }
                this.keys = leftKeys;
                this.children = leftChildren;

                // promote midkey to an InternalNode
                if (parent == null) {
                    parent = new BPlusTreeInternalNode(this);
                }
                BPlusTreeInternalNode<K> rightNode = new BPlusTreeInternalNode<>(parent, rightKeys, rightChildren);
                parent.promoteRight(midKey, rightNode);
            }
        }

    }

    private class BPlusTreeLeafNode<K extends Comparable, V> implements BPlusTreeNode<K, V> {
        // Maintain a sorted map
        SortedMap<K, V> values = new TreeMap<>((Comparator<? super K>) comparator);
        BPlusTreeLeafNode<K, V> next;
        private BPlusTreeInternalNode parent = null;

        public BPlusTreeLeafNode(BPlusTreeInternalNode parent, SortedMap<K, V> values) {
            this.values = values;
            this.parent = parent;
        }

        public BPlusTreeLeafNode() { }

        @Override
        public boolean needsSplit() {
            return values.size() >= leafDegree;
        }

        @Override
        public boolean needsMerge() {

            return values.size() < (int) Math.floor((double) leafDegree / 2.0);
        }

        @Override
        public Map.Entry<K, V> remove(K key) {
            return null;
        }

        @Override
        public V find(K key) {
            return values.get(key);
        }

        @Override
        public BPlusTreeNode<K, V> put(K key, V value) {
            values.put(key, value);
            if (needsSplit()) {
                System.out.println("split leaf node");
                int midpoint = (int) Math.floor(leafDegree / 2.0);

                // right is greater or equal to midkey
                SortedMap<K, V> right = new TreeMap<>((Comparator<? super K>) comparator);
                SortedMap<K, V> left = new TreeMap<>((Comparator<? super K>) comparator);

                for (Map.Entry<K, V> entry : this.values.entrySet()) {
                    if (left.size() < midpoint) {
                        left.put(entry.getKey(), entry.getValue());
                    } else {
                        right.put(entry.getKey(), entry.getValue());
                    }
                }
                this.values = left;

                // promote midkey to an InternalNode
                if (parent == null) {
                    parent = new BPlusTreeInternalNode(this);
                }
                BPlusTreeLeafNode<K, V> rightNode = new BPlusTreeLeafNode<>(parent, right);
                this.next = rightNode;
                parent.promoteRight(right.firstKey(), rightNode);
            }
            return (parent == null ? this : parent);
        }
    }


}
