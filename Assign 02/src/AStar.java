import java.util.*;

/**
 * Created by drb on 29/03/15.
 */
public final class AStar {

    public Stack<RoadSegment> ShortestPath(Intersection start, Intersection goal, Set<RoadUsers> allowedUses) {
        assert(goal != null);
        PriorityQueue<AStarNode> fringe = new PriorityQueue<>();
        Set<Intersection> visited = new HashSet<>();

        AStarNode lastNode = null;
        fringe.offer(new AStarNode(start, null, null, 0, estimate(start, goal)));

        while (!fringe.isEmpty()) {
            AStarNode node = fringe.poll();
            lastNode = node;
            if (!visited.contains(node.node)) {
                visited.add(node.node);
                if (node.node.equals(goal)) {
                    break;
                }
                edges: for (RoadSegment edge : node.node.getOutOf()) {
                    assert(!edge.getTo(node.node).equals(node.node));
                    boolean allowed = false;
                    for (RoadUsers use : edge.parent.roadUsers) {
                        if (allowedUses.contains(use)) {
                            allowed = true;
                            break;
                        }
                    }
                    if (!allowed) {
                        continue;
                    }

                    if (edge.getTo(node.node) != null && !visited.contains(edge.getTo(node.node))) {
                        double costToNeighbour = node.costToHere + edge.length;
                        fringe.offer(new AStarNode(edge.getTo(node.node), node, edge, costToNeighbour, costToNeighbour + estimate(edge.to, goal) ));
                    }
                }
            }
        }

        if (lastNode.node.equals(goal)) {
            Stack<RoadSegment> segments = new Stack<>();
            while (lastNode != null) {
                if (lastNode.using != null) {
                    segments.push(lastNode.using);
                    lastNode = lastNode.from;
                } else {
                    break;
                }
            }
            return segments;
        }
        return null;
    }

    private double estimate(Intersection start, Intersection goal) {
        return start.location.distance(goal.location);
    }

    private class AStarNode implements Comparable {

        public final AStarNode from;
        public final Intersection node;
        public final RoadSegment using;
        public final double costToHere;
        public final double totalCostToGoal;

        public AStarNode(Intersection node, AStarNode from, RoadSegment using, double costToHere, double totalCostToGoal) {
            this.node = node;
            this.from = from;
            this.costToHere = costToHere;
            this.using = using;
            this.totalCostToGoal = totalCostToGoal;
        }

        @Override
        public int compareTo(Object o) {
            if (o instanceof AStarNode) {
                return this.totalCostToGoal >= ((AStarNode)(o)).totalCostToGoal ? 1 : -1;
            }
            throw new IllegalArgumentException();
        }
    }
}
