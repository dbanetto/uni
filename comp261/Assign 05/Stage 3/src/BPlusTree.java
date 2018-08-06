import java.util.*;

/**
 * Created by drb on 17/05/15.
 */
public class BPlusTree<K extends Comparable<K>, V> implements Iterable<Map.Entry<K,V>> {
    private enum NodeType {
        Leaf,
        Internal;
    }

    private static final int leafDegree = 1024; // number of K-V pairs
    private static final int internalDegree = 100; // number of keys

    private final Comparator<K> comparator = new Comparator<K>() {
        @Override
        public int compare(K o1, K o2) {
            return o1.compareTo(o2);
        }
    };

    private BPlusTreeNode rootNode;

    public BPlusTree() {
        rootNode = new BPlusTreeLeafNode();
    }

    /**
     * Returns the String associated with the given key,
     * or null if the key is not in the B+ tree.
     */
    public List<V> find(K key) {
        if (key == null) {
            return null;
        }
        List<V> result = new ArrayList<>();
        rootNode.find(key, result);
        return result;
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
        BPlusTreeNode leftMost = rootNode;
        while (leftMost.getType() == NodeType.Internal) {
            BPlusTreeInternalNode internal = (BPlusTreeInternalNode)leftMost;
            leftMost = internal.children.get(0);
        }
        BPlusTreeLeafNode leftMostLeaf = (BPlusTreeLeafNode)leftMost;
        return leftMostLeaf.iterator();
    }

    private abstract class BPlusTreeNode {
        abstract boolean needsSplit();

        abstract boolean needsMerge();

        abstract Map.Entry<K, V> remove(K key);

        abstract void find(K key, List<V> result);
        abstract BPlusTreeNode put(K key, V value);

        abstract NodeType getType();
    }

    private class BPlusTreeInternalNode extends BPlusTreeNode {
        private BPlusTreeInternalNode parent = null;
        private List<K> keys = new ArrayList<>(internalDegree);
        private List<BPlusTreeNode> children = new ArrayList<>(internalDegree + 1);

        public BPlusTreeInternalNode(BPlusTreeNode left) {
            children.add(left);
        }

        private BPlusTreeInternalNode(BPlusTreeInternalNode parent, List<K> keys, List<BPlusTreeNode> children) {
            this.parent = parent;
            this.keys = keys;
            this.children = children;
            for (BPlusTreeNode child : this.children) {
                if (child.getType() == NodeType.Internal) {
                    ((BPlusTreeInternalNode)child).parent = this;
                } else if (child.getType() == NodeType.Leaf) {
                    ((BPlusTreeLeafNode)child).parent = this;
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
        public void find(K key, List<V> result) {
            for (int index = 0; index <= keys.size(); index++) {
                if (index >= keys.size()) {
                    children.get(keys.size()).find(key, result);
                    break;
                }

                if (key.compareTo(keys.get(index)) <= 0) {
                    children.get(index).find(key, result);
                    break;
                }
            }
        }

        @Override
        public BPlusTreeNode put(K key, V value) {
            for (int index = 0; index <= keys.size(); index++) {
                if (index >= keys.size()) {
                    children.get(keys.size()).put(key, value);
                    break;
                }

                if (key.compareTo(keys.get(index)) <= 0) {
                    children.get(index).put(key, value);
                    break;
                }
            }
            return (parent == null ? this : parent);
        }

        public void promoteRight(K key, BPlusTreeNode node) {
            int i = 0;
            for (; i <= keys.size(); i++) {
                if (i >= keys.size()) {
                    break;
                }

                if (key.compareTo(keys.get(i)) <= 0) {
                    break;
                }
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
            List<BPlusTreeNode> rightChildren = new ArrayList<>(internalDegree + 1);

            List<K> leftKeys = new ArrayList<>(internalDegree);
            List<BPlusTreeNode> leftChildren = new ArrayList<>(internalDegree + 1);

            ListIterator<K> keyItr = keys.listIterator();
            ListIterator<BPlusTreeNode> childItr = children.listIterator();

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

            this.keys = leftKeys;
            this.children = leftChildren;

            // promote midkey to an InternalNode
            if (parent == null) {
                parent = new BPlusTreeInternalNode(this);
            }
            BPlusTreeInternalNode rightNode = new BPlusTreeInternalNode(parent, rightKeys, rightChildren);
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

        @Override
        NodeType getType() {
            return NodeType.Internal;
        }
    }

    private class BPlusTreeLeafNode extends BPlusTreeNode implements Iterable<Map.Entry<K,V>> {

        private Comparator<AbstractMap.SimpleEntry<K, V>> entryComparator = new Comparator<AbstractMap.SimpleEntry<K, V>>() {
            @Override
            public int compare(AbstractMap.SimpleEntry<K, V> o1, AbstractMap.SimpleEntry<K, V> o2) {
                return o1.getKey().compareTo(o2.getKey());
            }
        };

        // Maintain a sorted map
        List<K> keys = new ArrayList<>();
        List<V> values = new ArrayList<>();

        BPlusTreeLeafNode next;
        BPlusTreeInternalNode parent = null;

        public BPlusTreeLeafNode(BPlusTreeInternalNode parent, List<K> keys, List<V> values) {
            this.keys = keys;
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
        public void find(K key, List<V> result) {
            // Idea is: Find the left most instance of the key
            // Then iterate from the 1st key equal to it to the last key equal to it
            // and collect 'em all
            BPlusTreeLeafNode node = this;
            for (int index = 0; index <= node.values.size(); index++) {
                if (index == node.values.size()) {
                    if (node.next == null) {
                        break;
                    }
                    node = node.next;
                    index = 0;
                }
                if (key.compareTo(node.keys.get(index)) == 0) {
                    result.add(node.values.get(index));
                } else if (key.compareTo(node.keys.get(index)) < 0) { // pass keys that is greater. stop
                    break;
                }
            }
        }

        @Override
        public BPlusTreeNode put(K key, V value) {
            // Does not matter what order the duplicates are stored in
            // We also know binary search will group them
            int index = Collections.binarySearch(keys, key);
            if (index < 0) {
                index = -index - 1;
            }
            this.keys.add(index, key);
            this.values.add(index, value);

            if (needsSplit()) {
                split();
            }
            return (parent == null ? this : parent);
        }

        private void split() {
            int midpoint = (int) Math.floor(leafDegree / 2.0);

            // right is greater or equal to midkey
            // screw SubLists (class)
            List<K> rightKeys =  new ArrayList<>(this.keys.subList(midpoint, this.values.size()));
            List<V> rightVals =  new ArrayList<>(this.values.subList(midpoint, this.values.size()));

            this.values = new ArrayList<>(this.values.subList(0, midpoint));
            this.keys = new ArrayList<>(this.keys.subList(0, midpoint));

            // promote midkey to an InternalNode
            if (parent == null) {
                parent = new BPlusTreeInternalNode(this);
            }
            BPlusTreeLeafNode rightNode = new BPlusTreeLeafNode(parent, rightKeys, rightVals);
            rightNode.next = this.next;
            this.next = rightNode;
            parent.promoteRight(rightKeys.get(0), rightNode);
        }

        private BPlusTreeLeafNode This() {
            return this;
        }

        @Override
        public Iterator<Map.Entry<K, V>> iterator() {
            return new Iterator<Map.Entry<K, V>>() {
                Iterator<K> internal = keys.iterator();
                int index = 0;
                BPlusTreeLeafNode nextNode = next;
                BPlusTreeLeafNode current = This();

                // Debug
                Map.Entry<K,V> last = null;

                @Override
                public boolean hasNext() {
                    if (!internal.hasNext()) {
                        if (nextNode == null) {
                            return false;
                        } else {
                            return true;
                        }
                    }
                    return internal.hasNext();
                }

                @Override
                public Map.Entry<K, V> next() {
                    if (!internal.hasNext()) {
                        if (nextNode != null) {
                            internal = nextNode.keys.iterator();
                            current = nextNode;
                            nextNode = nextNode.next;
                            index = 0;
                        } else {
                            throw new ArrayIndexOutOfBoundsException();
                        }
                    }
                    Map.Entry<K, V> entry = new AbstractMap.SimpleEntry<>(current.keys.get(index), current.values.get(index));
                    index++;
                    internal.next();
                    return entry;
                }
            };
        }

        public String toString() {
            return "Leaf{" + this.keys.get(0) +  "..." + this.keys.get(this.keys.size() - 1) + "}";
        }
        @Override
        NodeType getType() {
            return NodeType.Leaf;
        }

    }


}
