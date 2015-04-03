import java.util.*;

/**
 * Created by drb on 02/04/15.
 */
public class Articulation {

    public static Set<Intersection> IdentifyArticulationPoints(Collection<Intersection> nodesInGraph) {
        Map<Intersection, ArticulationNode> nodes = new HashMap<>();
        List<Intersection> unVisitedNodes = new ArrayList<>(nodesInGraph);

        Set<Intersection> articulationPoints = new HashSet<>();

        // Handle the forest
        while (!unVisitedNodes.isEmpty()) {
            Intersection rootNode = unVisitedNodes.get(0);

            // Find all the articulation points in a graph
            if (unVisitedNodes.contains(rootNode)) {
                articulationPoints.addAll(articulationPointsRoot(rootNode, unVisitedNodes, nodes));
            } else {
                throw new RuntimeException();
            }
        }
        assert(unVisitedNodes.isEmpty());
        assert(nodes.size() == nodesInGraph.size());
        return articulationPoints;
    }

    private static Set<Intersection> articulationPointsRoot(Intersection rootNode, List<Intersection> unVisitedNodes, Map<Intersection, ArticulationNode> nodes) {
        Set<Intersection> articulationPoints = new HashSet<>();
        nodes.put(rootNode, new ArticulationNode(rootNode, 0, null));
        unVisitedNodes.remove(rootNode);

        int subTrees = 0;
        for (Intersection i : rootNode.getNeighbours()) {
            if (unVisitedNodes.contains(i) && !i.equals(rootNode) && !nodes.containsKey(i)) {
                articulationPoints.addAll(articulationPoints(rootNode, i, unVisitedNodes, nodes));
                subTrees++;
            }
        }
        if (subTrees > 1) {
            articulationPoints.add(rootNode);
        }

        return articulationPoints;
    }

    private static Set<Intersection> articulationPoints(Intersection rootNode, Intersection firstNode, List<Intersection> unVisitedNodes, Map<Intersection, ArticulationNode> nodes) {
        Set<Intersection> articulationPoints = new HashSet<>();
        Stack<ArticulationNode> stack = new Stack<>();
        stack.push(new ArticulationNode(firstNode, 1, new ArticulationNode(rootNode, 0, null)));
        while (!stack.isEmpty()) {
            ArticulationNode elem = stack.peek();
            if (elem.children == null) {
                elem.reachBack = elem.depth;
                elem.children = new LinkedList<>();

                for (Intersection neigh : elem.data.getNeighbours()) {
                    if (!neigh.equals(elem.parent.data)) {
                        elem.children.offer(neigh);
                    }
                }
            } else if (!elem.children.isEmpty()) {
                Intersection child = elem.children.poll();
                if (unVisitedNodes.contains(child)) {
                    ArticulationNode node = new ArticulationNode(child, elem.depth + 1, elem);
                    nodes.put(child, node);
                    unVisitedNodes.remove(child);
                    stack.add(node);
                } else {
                    ArticulationNode childNode = nodes.get(child);
                    elem.reachBack = Math.min(elem.reachBack, childNode.depth);
                }
            } else {
                if (!elem.data.equals(firstNode)) {
                    System.out.println("elem:"+ elem.data.id + "ID  parent:" + elem.parent.data.id + "ID");
                    System.out.println(elem.reachBack + "(dp:" + elem.depth + ")" + " >= " + elem.parent.depth + "(rb:" + elem.parent.reachBack + ")");
                    if (elem.reachBack >= elem.parent.depth) {
                        articulationPoints.add(elem.parent.data);
                    }
                    elem.parent.reachBack = Math.min(elem.parent.reachBack, elem.reachBack);

                }
                stack.pop();
            }
        }

        return articulationPoints;
    }

    private static class ArticulationNode {
        final Intersection data;
        int reachBack = Integer.MAX_VALUE;
        int depth;
        final ArticulationNode parent;
        boolean completed = false;
        Queue<Intersection> children;

        public ArticulationNode(Intersection data, int depth, ArticulationNode parent) {
            this.data = data;
            this.depth = depth;
            this.parent = parent;
            reachBack = depth;
        }


        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            ArticulationNode that = (ArticulationNode) o;

            if (!data.equals(that.data)) return false;
            return children.equals(that.children);

        }

        @Override
        public int hashCode() {
            return data.hashCode();
        }
    }

}
