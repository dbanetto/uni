
public class EdgeListItem {
    private boolean left_filled;
    private float x_left;
    private float z_left;

    private boolean right_filled;
    private float x_right;
    private float z_right;

    public EdgeListItem() {
        left_filled = false;
        right_filled = false;
    }

    public void put(float x, float z) {
        if (left_filled && right_filled) {
            if (Float.compare(x, x_left) != 0 && Float.compare(x, x_right) != 0) {
                // throw new IllegalArgumentException();
            }
        } else if (!left_filled && !right_filled) {
            x_left = x;
            z_left = z;
            left_filled = true;
        } else if (left_filled && !right_filled) {
            if (x >= x_left) {
                x_right = x;
                z_right = z;
                right_filled = true;
            } else {
                //swap left and right
                x_right = x_left;
                z_right = z_left;

                x_left = x;
                z_left = z;
                right_filled = true;
            }
        } else {
            throw new IllegalArgumentException();
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
