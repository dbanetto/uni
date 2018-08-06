
public class EdgeListItem {
    private float x_left = Float.POSITIVE_INFINITY;
    private float z_left = 0;

    private float x_right = Float.NEGATIVE_INFINITY;
    private float z_right = 0;

    public EdgeListItem() {

    }

    public void put(float x, float z) {
        if (Float.compare(x, x_right) > 0) {
            x_right = x;
            z_right = z;
        }
        if (Float.compare(x, x_left) < 0) {
            x_left = x;
            z_left = z;
        }
    }

    public float getZ_left() {
        return z_left;
    }

    public float getX_left() {
        return x_left;
    }

    public float getX_right() {
        return x_right;
    }

    public float getZ_right() {
        return z_right;
    }

    @Override
    public String toString() {
        return "EdgeListItem{" +
                "x_left=" + x_left +
                ", z_left=" + z_left +
                ", x_right=" + x_right +
                ", z_right=" + z_right +
                '}';
    }
}
