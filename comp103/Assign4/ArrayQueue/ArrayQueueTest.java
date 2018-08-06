// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP103, Assignment 4
 * Name:
 * Usercode:
 * ID:
 */

import java.lang.reflect.Field;
import java.util.Iterator;
import java.util.Queue;
import java.util.Set;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.JUnitCore;

/**
 * ArrayQueueTest A JUnit class for testing an ArrayQueue
 */

public class ArrayQueueTest {

	private ArrayQueue<String> queue;

	@Before
	public void setup() {
		queue = new ArrayQueue<String>();
	}

	@After
	public void tearDown() {
		queue.clear();
	}

	@Test
	public void IsEmptyOnConstruct() {
		assertTrue(queue.isEmpty());
	}

	@Test
	public void HasElements() {
		populateQueue(10);
		assertTrue(!queue.isEmpty());
		assertEquals(10, queue.size());
	}

	@Test
	public void IsEmptyOnClear() {
		populateQueue(10);
		queue.clear();
		assertTrue(queue.isEmpty());
	}

	@Test
	public void PollEmptyQueue() {
		String poll = queue.poll();
		assertEquals(null, poll);
	}

	@Test
	public void PollFilledQueue() {
		populateQueue(10);
		String poll = queue.poll();
		assertEquals("Item0", poll);
	}

	@Test
	public void PeekEmptyQueue() {
		String poll = queue.peek();
		assertEquals(null, poll);
	}

	@Test
	public void PeekFilledQueue() {
		populateQueue(10);
		String poll = queue.peek();
		assertEquals("Item0", poll);
	}

	@Test
	public void OfferNullQueue() {
		assertFalse(queue.offer(null));
	}

	@Test
	public void OfferValidQueue() {
		assertTrue(queue.offer("Valid"));
	}

	@Test
	public void LargePopulate() {
		populateQueue(256);
		assertTrue(!queue.isEmpty());
		assertEquals(256, queue.size());
	}

	@Test
	public void IteratorPass() {
		populateQueue(10);
		int n = 0;
		for (String str : queue) {
			assertTrue(str.equals("Item" + n));
			n++;
		}
	}

	@Test
	public void IteratorHasNext() {
		populateQueue(10);
		Iterator<String> itr = queue.iterator();
		for (int n = 0; n <= 10; n++) {
			if (n != 10) {
				assertTrue(itr.hasNext());
				itr.next();
			} else {
				assertFalse(itr.hasNext());
			}
		}
	}

	@Test
	public void IteratorNextvsIterator() {
		populateQueue(10);
		Iterator<String> itr = queue.iterator();
		for (int n = 0; n < 10; n++) {
			assertEquals(itr.next(), queue.poll());
		}
	}

	@Test
	public void IteratorHasNextPolled() {
		populateQueue(11);
		queue.poll();
		assertEquals(10, queue.size());
		Iterator<String> itr = queue.iterator();
		for (int n = 0; n < 11; n++) {
			if (n < 10) {
				assertTrue(itr.hasNext());
				itr.next();
			} else {
				assertFalse(itr.hasNext());
			}
		}
	}

	@Test(expected = UnsupportedOperationException.class)
	public void IteratorRemove() {
		populateQueue(10);
		Iterator<String> itr = queue.iterator();
		itr.remove();
	}

	@Test
	public void ToArray() {
		populateQueue(10);
		String[] array = new String[10];
		queue.toArray(array);
		for (int i = 0; i < array.length; i++) {
			assertEquals(array[i], queue.poll());
		}
	}

	@Test
	public void ToArrayPolled() {
		populateQueue(11);
		String[] array = new String[11];
		assertEquals("Item0", queue.poll());
		queue.toArray(array);
		assertEquals(array[10], null);
		for (int i = 0; i < array.length; i++) {
			assertEquals(array[i], queue.poll());
		}
	}

	@Test
	public void ArrayLayout() {
		populateQueue(2);
		queue.poll();
		assertArrayEquals(new String[] { null, "Item1", null, null, null, null,
				null, null, null, null, null, null, null, null, null, null },
				queue.getArray());

	}

	@Test
	public void ArrayLayoutCollpaseEnsure() {
		populateQueue(16);
		for (int i = 0; i < 8; i++)
			queue.poll();
		queue.offer("END");
		assertArrayEquals(new String[] { "Item8", "Item9", "Item10", "Item11",
				"Item12", "Item13", "Item14", "Item15", "END", null, null,
				null, null, null, null, null }, queue.getArray());
	}
	
	@Test
	public void ArrayLayoutExpandEnsure() {
		populateQueue(17);
		Object[] data = queue.getArray();
		assertEquals(32, data.length );
		assertEquals(17, queue.size());
		
		for (int n = 0; n < data.length; n++) {
			if (n < queue.size())
				assertEquals("Item" + n , data[n]);
			else 
				assertEquals(null , data[n]);
		}
	}

	public void populateQueue(int n) {
		for (int i = 0; i < n; i++) {
			queue.offer("Item" + i);
		}
	}

	public static void main(String args[]) {
		org.junit.runner.JUnitCore.main("ArrayQueueTest");
	}

}
