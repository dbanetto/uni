/**
 * Created by drb on 15/10/14.
 */
public class Cyclist implements Comparable<Cyclist> {
    public int AvailableAt;

    public Cyclist() {
        AvailableAt = -1;
    }

    @Override
    public int compareTo(Cyclist o) {
        if (o instanceof Cyclist) {
            Cyclist other = (Cyclist)o;
            return other.AvailableAt - this.AvailableAt;
        }
        return 0;
    }
}
