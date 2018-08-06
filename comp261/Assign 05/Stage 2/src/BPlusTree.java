import java.util.*;

/**
 * Created by drb on 17/05/15.
 */
public class BPlusTree<K extends Comparable<K>, V> implements Iterable<Map.Entry<K,V>> {
    private static final int leafDegree = 1024; // number of K-V pairs
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
        if (key == null) {
            return null;
        }
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
        if (key == null) {
            throw new IllegalArgumentException();
        }
        rootNode = rootNode.put(key, value);
        return true;
    }

    @Override
    public Iterator<Map.Entry<K, V>> iterator() {
        BPlusTreeNode<K, V> leftMost = rootNode;
        while (leftMost instanceof BPlusTreeInternalNode) {
            BPlusTreeInternalNode<K> internal = (BPlusTreeInternalNode<K>)leftMost;
            leftMost = internal.children.get(0);
        }
        BPlusTreeLeafNode<K,V> leftMostLeaf = (BPlusTreeLeafNode<K,V>)leftMost;
        return leftMostLeaf.iterator();
    }

    private interface BPlusTreeNode<K, V> {
        boolean needsSplit();

        boolean needsMerge();

        Map.Entry<K, V> remove(K key);

        V find(K key);

        BPlusTreeNode<K, V> put(K key, V value);
    }

    private class BPlusTreeInternalNode<K extends Comparable<K>> implements BPlusTreeNode<K, V> {
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
            for (BPlusTreeNode<K, V> child : this.children) {
                if (child instanceof BPlusTreeInternalNode) {
                    ((BPlusTreeInternalNode<K>)child).parent = this;
                } else if (child instanceof BPlusTreeLeafNode) {
                    ((BPlusTreeLeafNode<K,V>)child).parent = this;
                }
            }
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
            int i = Collections.binarySearch(this.keys, key);
            if (i < 0) {
                i = -i - 1;
            }
            return children.get(i).find(key);
        }

        @Override
        public BPlusTreeNode<K, V> put(K key, V value) {
            int i = Collections.binarySearch(this.keys, key);
            if (i < 0) {
                i = -i - 1;
            }
            children.get(i).put(key, value);
            return (parent == null ? this : parent);
        }

        public void promoteRight(K key, BPlusTreeNode<K, V> node) {
            int i = Collections.binarySearch(this.keys, key);
            if (i < 0) {
                i = -i - 1;
            }

            keys.add(i, key);
            children.add(i + 1, node);
            if (needsSplit()) {
                this.split();
            }
        }

        private void split() {
            int midpoint = (int) Math.floor(internalDegree / 2.0);

            // right is greater or equal to midkey
            List<K> rightKeys = new ArrayList<>(internalDegree);
            List<BPlusTreeNode<K, V>> rightChildren = new ArrayList<>(internalDegree + 1);

            List<K> leftKeys = new ArrayList<>(internalDegree);
            List<BPlusTreeNode<K, V>> leftChildren = new ArrayList<>(internalDegree + 1);

            ListIterator<K> keyItr = keys.listIterator();
            ListIterator<BPlusTreeNode<K, V>> childItr = children.listIterator();

            K midKey = null;
            for (int i = 0; i < keys.size(); i++) {
                if (i < midpoint) {
                    leftKeys.add(keyItr.next());
                    leftChildren.add(childItr.next());
                } else if (i == midpoint) {
                    midKey = keyItr.next();
                    leftChildren.add(childItr.next());
                } else {
                    rightKeys.add(keyItr.next());
                    rightChildren.add(childItr.next());
                }
            }
            rightChildren.add(childItr.next());

            if (keys.size() != leftKeys.size() + rightKeys.size() + 1) {
                throw new AssertionError();
            }
            if (children.size() != leftChildren.size() + rightChildren.size()) {
                throw new AssertionError();
            }
            if (leftChildren.size() != leftKeys.size() + 1) {
                throw new AssertionError();
            }
            if (rightChildren.size() != rightKeys.size() + 1) {
                throw new AssertionError();
            }

            this.keys = leftKeys;
            this.children = leftChildren;

            // promote midkey to an InternalNode
            if (parent == null) {
                parent = new BPlusTreeInternalNode(this);
            }
            BPlusTreeInternalNode<K> rightNode = new BPlusTreeInternalNode<>(parent, rightKeys, rightChildren);
            parent.promoteRight(midKey, rightNode);
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
    }

    private class BPlusTreeLeafNode<K extends Comparable<K>, V> implements BPlusTreeNode<K, V>, Iterable<Map.Entry<K,V>> {
        // Maintain a sorted map
        SortedMap<K, V> values = new TreeMap<>((Comparator<? super K>)comparator);
        BPlusTreeLeafNode<K, V> next;
        BPlusTreeInternalNode parent = null;

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
                split();
            }
            return (parent == null ? this : parent);
        }

        private void split() {
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
            rightNode.next = this.next;
            this.next = rightNode;
            parent.promoteRight(right.firstKey(), rightNode);
        }

        @Override
        public Iterator<Map.Entry<K, V>> iterator() {
            return new Iterator<Map.Entry<K, V>>() {
                Iterator<Map.Entry<K,V>> internal = values.entrySet().iterator();
                BPlusTreeLeafNode<K,V> nextNode = next;

                // Debug
                Map.Entry<K,V> last = null;

                @Override
                public boolean hasNext() {
                    if (!internal.hasNext()) {
                        return nextNode != null;
                    }
                    return internal.hasNext();
                }

                @Override
                public Map.Entry<K, V> next() {
                    if (!internal.hasNext()) {
                        if (nextNode != null) {
                            internal = nextNode.values.entrySet().iterator();
                            nextNode = nextNode.next;
                        } else {
                            throw new ArrayIndexOutOfBoundsException();
                        }
                    }
                    return internal.next();
                }
            };
        }

        public String toString() {
            return "Leaf{" + this.values.firstKey() +  "..." + this.values.lastKey() + "}";
        }
    }


}
