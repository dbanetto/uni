/**
 * Created by drb on 16/04/15.
 */
public class Camera {
    private Vector3D position;
    private Vector3D rotation;
    private Vector3D scale;

    public Camera(Vector3D position, Vector3D rotation, Vector3D scale) {
        this.position = position;
        this.rotation = rotation;
        this.scale = scale;
    }

    public Transform getTransformation() {
        Transform t = Transform.newTranslation(position);
        t = t.compose(Transform.newScale(scale));
        t = t.compose(Transform.newXRotation(rotation.x));
        t =  t.compose(Transform.newYRotation(rotation.y));
        t = t.compose(Transform.newZRotation(rotation.z));
        return t;
    }
    public Transform getRotationTransformation() {
        Transform t = Transform.newXRotation(rotation.x);
        t =  t.compose(Transform.newYRotation(rotation.y));
        t = t.compose(Transform.newZRotation(rotation.z));
        return t;
    }


    public void translate(Vector3D by) {
        this.position = position.plus(by);
    }
    public void setPosition(Vector3D pos) {
        this.position = pos;
    }

    public void setScale(Vector3D scale) {
        this.scale = scale;
    }

    public void growScale(float scale) {
        this.scale = new Vector3D(this.scale.x * scale,this.scale.y * scale, 1.0f);
    }

    public void rotateX(float by) {
        rotation = new Vector3D((rotation.x + by) % ((float)Math.PI * 2f), rotation.y, rotation.z);
    }

    public void rotateY(float by) {
        rotation = new Vector3D(rotation.x, (rotation.y + by) % ((float)Math.PI * 2f), rotation.z);
    }

    public void rotateZ(float by) {
        rotation = new Vector3D(rotation.x, rotation.y, (rotation.z + by) % ((float)Math.PI * 2f));
    }

    public Vector3D getPosition() {
        return position;
    }

    @Override
    public String toString() {
        return "Camera{" +
                "position=" + position +
                ", rotation=" + rotation +
                ", scale=" + scale +
                '}';
    }
}
