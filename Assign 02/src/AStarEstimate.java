/**
 * Created by drb on 08/04/15.
 */
public interface AStarEstimate<T, O> {
    double estimate(T from , T to, O extra);
    double cost(T from, T to, O extra);
}
