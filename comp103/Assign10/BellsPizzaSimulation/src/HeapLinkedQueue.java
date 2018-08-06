/**
 * Created by drb on 15/10/14.
 */

import java.util.*;

public class HeapLinkedQueue<E extends Comparable<? super E>> extends
        AbstractQueue<E> {

    private int count;
    private HeapLinkedNode<E> root;

    public HeapLinkedQueue() {
        count = 0;
    }


    @Override
    public Iterator<E> iterator() {
        return null;
    }

    public int size() {
        return count;
    }

    public boolean isEmpty() {
        return peek() == null;
    }

    @Override
    public boolean offer(E e) {
        if (root == null) {
            root = new HeapLinkedNode<E>(e);
        } else {
            Stack<HeapLinkedNode<E>> nodes = new Stack<>();

            HeapLinkedNode<E> pointer = root;
            while (true) {
                nodes.push(pointer);
                if (pointer.getLeft() == null) {
                    pointer.setLeft(new HeapLinkedNode<E>(e));
                    break;
                } else if (pointer.getRight() == null) {
                    pointer.setRight(new HeapLinkedNode<E>(e));
                    break;
                }

                if (pointer.getLeft().minHeight() == pointer.getRight().minHeight()) {
                    pointer = pointer.getLeft();
                } else if (pointer.getLeft().minHeight() > pointer.getRight().minHeight()) {
                    pointer = pointer.getRight();
                }
            }

            pointer = nodes.pop();
            while (!nodes.isEmpty()) {
                HeapLinkedNode<E> parent = nodes.pop();
                if (pointer.getData().compareTo(parent.getData()) > 0) {
                    swapNodes(pointer, parent);
                }
            }
        }
        count++;
        return true;
    }

    private void swapNodes(HeapLinkedNode<E> a, HeapLinkedNode<E> b) {
        E temp = a.getData();
        a.setData(b.getData());
        b.setData(temp);
    }

    @Override
    public E poll() {
        E toReturn = null;
        if (root != null) {
            toReturn = root.getData();
            removeRoot();
            count--;
        }
        return toReturn;
    }

    private void removeRoot() {
        HeapLinkedNode<E> pointer = root;
        // Simple remove case
        if (pointer.getLeft() == null && pointer.getRight() == null) {
            root = null;
            return;
        } else if (pointer.getLeft() != null && pointer.getRight() == null) {
            root = root.getLeft();
            root.setParent(null);
            return;
        }

        // Complex remove case
        while (pointer.getLeft() != null) {
            int leftHeight = (pointer.getLeft() != null ? pointer.getLeft().minHeight() + 1 : 0);
            int rightHeight = (pointer.getRight() != null ? pointer.getRight().minHeight() + 1 : 0);

            if (leftHeight != 0 && leftHeight == rightHeight) {
                pointer = pointer.getRight();
            } else if (leftHeight != 0 && leftHeight > rightHeight) {
                pointer = pointer.getLeft();
            }
        }
        // Swap root with most bottom right node
        swapNodes(root, pointer);
        HeapLinkedNode<E> toRemove = root;
        root = pointer;

        // Clean up old root node
        if (toRemove.getParent() != null) {
            if (toRemove.getParent().getLeft() == toRemove) {
                toRemove.getParent().setLeft(null);
            } else if (toRemove.getParent().getRight() == toRemove) {
                toRemove.getParent().setRight(null);
            }
            toRemove.setParent(null);
        }

        // Sink the fake root
        while (true) {
            int leftDiff = ((pointer.getLeft() != null) ? pointer.getData().compareTo(pointer.getLeft().getData()) : 0);
            int rightDiff = (pointer.getRight() != null ? pointer.getData().compareTo(pointer.getRight().getData()) : 0);

            if (leftDiff <= 0 && rightDiff <= 0) {
                break; // Cannot sink

            } else if (leftDiff > 0 && rightDiff < 0) {
                swapNodes(pointer, pointer.getLeft());
            } else if (leftDiff < 0 && rightDiff > 0) {
                swapNodes(pointer, pointer.getRight());
            } else if (leftDiff > rightDiff) {
                swapNodes(pointer, pointer.getLeft());
            } else {
                swapNodes(pointer, pointer.getRight());
            }
        }


    }

    @Override
    public E peek() {
        return (root != null ? root.getData() : null);
    }
}
