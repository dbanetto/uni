/**
 * Created by drb on 15/10/14.
 */
public class HeapLinkedNode<E extends Comparable<? super E>> {
    private E data;
    private HeapLinkedNode<E> left, right, parent;

    public HeapLinkedNode(E data) {
        this.data = data;
        this.parent = null;
    }

    public int minHeight() {
        int height = Integer.MAX_VALUE;
        if (left != null)
            height = Math.min(height, left.minHeight() + 1);
        if (right != null)
            height = Math.min(height, right.minHeight() + 1);
        return (height == Integer.MAX_VALUE ? 0 : height);
    }

    public int height() {
        int height = 0;
        if (left != null)
            height = Math.max(height, left.height() + 1);
        if (right != null)
            height = Math.max(height, right.height() + 1);
        return height;
    }

    public E getData() {
        return data;
    }

    public void setData(E data) {
        this.data = data;
    }

    public HeapLinkedNode<E> getLeft() {
        return left;
    }

    public void setLeft(HeapLinkedNode<E> left) {
        this.left = left;
    }

    public HeapLinkedNode<E> getRight() {
        return right;
    }

    public void setRight(HeapLinkedNode<E> right) {
        this.right = right;
    }

    public HeapLinkedNode<E> getParent() {
        return parent;
    }

    public void setParent(HeapLinkedNode<E> parent) {
        this.parent = parent;
    }


}
