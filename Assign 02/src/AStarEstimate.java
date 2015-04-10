/**
 * Created by drb on 08/04/15.
 */
public interface AStarEstimate<T, O> {
    /**
     * The estimate function for the AStar
     * @param from current node
     * @param goal goal node
     * @param extra Any extra information that needs to be added
     * @return the estimated cost from to goal
     */
    double estimate(T from , T goal, O extra);

    /**
     * Calculates the exact cost from one to the other points
     *
     * @param from coming from
     * @param to going to
     * @param extra extra information needed for calculation
     * @return the cost of going between the two nodes
     */
    double cost(T from, T to, O extra);
}
