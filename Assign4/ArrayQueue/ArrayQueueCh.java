import java.util.Collection;
import java.util.Iterator;
import java.util.Random;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;

import javax.activity.InvalidActivityException;

import ecs100.UI;

public class ArrayQueueCh<E> implements BlockingQueue<E> {

	private static int INITIALCAPACITY = 16;
	private final Object lock = new Object();

	private E[] data;
	private int front = 0; // the index of the first item in the queue
	private int back = 0;

	@SuppressWarnings("unchecked")
	public ArrayQueueCh() {
		data = (E[]) new Object[INITIALCAPACITY];
	}

	@Override
	public E remove() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public E poll() {
		if (isEmpty())
			return null;
		else {
			E ans;
			synchronized (this) {
				ans = data[front];
				data[front] = null;
				front = (front + 1);

			}
			return ans;
		}
	}

	@Override
	public E element() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public E peek() {
		if (isEmpty())
			return null;
		else
			return data[front];
	}

	@Override
	public int size() {
		return (back - front);
	}

	@Override
	public boolean isEmpty() {
		return (size() <= 0);
	}

	@Override
	public Iterator<E> iterator() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Object[] toArray() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> T[] toArray(T[] a) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean addAll(Collection<? extends E> c) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		// TODO Auto-generated method stub
		return false;
	}

	@SuppressWarnings("unchecked")
	@Override
	public void clear() {
		data = (E[]) new Object[INITIALCAPACITY];
		front = 0;
		back = 0;
	}

	@Override
	public boolean add(E e) {
		if (e == null)
			return false;

		synchronized (this) {
			ensureCapacity();
			data[back] = e;
			back = (back + 1);
		}
		return true;

	}

	@Override
	public boolean offer(E e) {
		if (e == null)
			return false;

		synchronized (this) {
			ensureCapacity();
			data[back] = e;
			back = (back + 1);
		}
		return true;
	}

	@Override
	public synchronized void put(E e) throws InterruptedException {
		if (e == null)
			return;

		synchronized (this) {
			ensureCapacity();
			data[back] = e;
			back = (back + 1);
			
				notify();
			
		}
	}

	@Override
	public boolean offer(E e, long timeout, TimeUnit unit)
			throws InterruptedException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public synchronized E take() throws InterruptedException {

		while (isEmpty()) {
			
				try {
					wait();
				} catch (InterruptedException e) { }
			
		}

		synchronized (this) {
			E ans = data[front];
			// data[front] = null;
			front++;
			if (ans == null)
				System.out.println("ANS ERROR");

			return ans;
		}

	}

	@Override
	public E poll(long timeout, TimeUnit unit) throws InterruptedException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int remainingCapacity() {
		return data.length - back;
	}

	@Override
	public boolean remove(Object o) {
		if (o == null)
			return true;
		synchronized (this) {
			int itemIndex = -1;
			for (int i = front; i < back; i++) {
				if (data[i].equals(o)) {
					itemIndex = i;
				}
			}

			if (itemIndex == -1)
				return false;

			data[itemIndex] = null;

			for (int i = itemIndex + 1; i < back; i++) {
				data[i - 1] = data[i];
			}
			back--;
		}
		return true;
	}

	@Override
	public boolean contains(Object o) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public int drainTo(Collection<? super E> c) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int drainTo(Collection<? super E> c, int maxElements) {
		// TODO Auto-generated method stub
		return 0;
	}

	@SuppressWarnings("unchecked")
	// this will stop Java complaining
	private void ensureCapacity() {

		if (back == data.length) {
			if (front >= data.length / 2) {
				int j = 0;
				for (int i = front; i < back; i++) {
					data[j] = data[i];
					data[i] = null;
					j++;
				}
				back = back - front;
				front = 0;
			} else {
				E[] newArray = (E[]) (new Object[data.length * 2]);
				int j = 0;
				for (int i = front; i < back; i++) {
					newArray[j++] = data[i];
				}
				back = back - front;
				front = 0;
				data = newArray;
			}

		}
	}

}
