import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;


public class TrieNode {
    Map<Character, TrieNode> edges;
    boolean isEnd;

    /**
     * @param IsEnd flag for end for word
     */
    public TrieNode(boolean IsEnd) {
        edges = new TreeMap<>();
        isEnd = IsEnd;
    }

    /**
     * Insert text into the trie
     * @param text to be inserted
     * @return true if inserted, otherwise false
     */
    public boolean insert(String text) {
        if (text.isEmpty()) { return false; }

        if (edges.containsKey(text.charAt(0))) {
            return edges.get(text.charAt(0)).insert(text.substring(1));
        } else {
            edges.put(text.charAt(0), new TrieNode(text.substring(1).isEmpty()));
            if (!edges.get(text.charAt(0)).isEnd) {
                return edges.get(text.charAt(0)).insert(text.substring(1));
            }
        }
        return false;
    }

    /**
     * Checks if text exists in the trie
     *
     * @param text to be checked
     * @return true if it exists, otherwise false
     */
    public boolean exists(String text) {
        if (text.isEmpty()) { return isEnd; }

        if (!edges.containsKey(text.charAt(0))) {
            return false;
        } else {
            return edges.get(text.charAt(0)).exists(text.substring(1));
        }
    }

    /**
     *  Complete the prefix with a given number of completions
     *
     * @param prefix
     * @param max the number of completions to return
     * @return a list of size max with completions of the prefix
     */
    public List<String> autocomplete(String prefix, int max) {
        List<String> list = complete(prefix);
        if (list != null) {
            list = list.subList(0, (list.size() > max ? max : list.size()));
            for (int i = 0; i < list.size(); i++) {
                list.set(i, prefix + list.get(i));
            }
        }
        return list;
    }

    /**
     * Complete the prefix with available options
     * @param prefix to be completed
     * @return a List of completed strings
     */
    private List<String> complete(String prefix) {
        // Get to the end of the prefix
        if (!prefix.isEmpty()) {
            if (edges.containsKey(prefix.charAt(0))) {
                return edges.get(prefix.charAt(0)).complete(prefix.substring(1));
            } else {
                return null;
            }
        }
        // iterate over the sub-tree with max # of completions
        if (this.isEnd) {
            List<String> l = new ArrayList<>();
            l.add("");
            return l;
        }
        return iterate();
    }

    /**
     * Iterate over children collecting end nodes
     *
     * @return a list of end nodes with its path of characters
     */
    private List<String> iterate() {
        List<String> list = new ArrayList<>();
        for (Character c : edges.keySet()) {
            if (edges.get(c).isEnd) {
                list.add(c.toString());
            } else {
                for (String s : edges.get(c).iterate()) {
                    list.add(c.toString() + s);
                }
            }
        }
        return list;
    }

}
