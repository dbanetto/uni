// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP 103, Assignment 9
 * Name:
 * Usercode:
 * ID:
 */

import java.util.List;
import java.util.Arrays;

import static org.junit.Assert.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.JUnitCore;

/**
 * BSTSetTestCore A JUnit class for testing BSTSet with just a core set of
 * tests.
 */

public class BSTSetTestCore {

	private List<String> items = Arrays.asList("v13", "v18", "v09", "v12",
			"v16", "v06", "v10", "v02", "v04", "v08", "v17", "v07", "v01",
			"v03", "v14", "v20", "v15", "v11", "v05", "v19");
	private BSTSet<String> set;

	/** initialise set to be an empty set before each test is run */
	@Before
	public void initialiseEmptySet() {
		set = new BSTSet<String>();
	}

	/** method to initialise the set */
	public void fillSet() {
		List<String> its = Arrays.asList("v14", "v07", "v05", "v10", "v20",
				"v16", "v01", "v03", "v13", "v18", "v06", "v17", "v04", "v19",
				"v12", "v09", "v08", "v11", "v02", "v15");
		for (String it : its) {
			set.add(it);
		}
	}

	// --------------------------------------------------------------------------------

	@Test
	public void testIsEmptyOnEmptySet() {
		assertTrue("A new set should be empty", set.isEmpty());
	}

	@Test
	public void testEmptySetHasSizeZero() {
		assertEquals("An empty set should have size zero", 0, set.size());
	}

	@Test
	public void testEmptySetDoesNotContainNull() {
		assertFalse("An empty set should not contain null", set.contains(null));
	}

	@Test
	public void testAddingToSet() {
		int size = 0;
		for (String item : items) {
			size++;
			assertTrue("Set should successfully add item " + item,
					set.add(item));
			assertFalse("Set should not be empty after add", set.isEmpty());
			assertEquals("Size should be " + size + " after " + size + " adds",
					size, set.size());
		}
	}

	// The expected parameter in the Test annotation indicates
	// that this test case expects an IllegalArgumentException
	// The Unit test will fails if the given exception is not thrown.
	// Since JUnit 4.
	@Test(expected = IllegalArgumentException.class)
	public void testAddingNull() {
		set.add(null);
	}

	@Test
	public void testAddingDuplicates() {
		fillSet();
		for (String item : items) {
			assertFalse("Set should not add duplicate item " + item,
					set.add(item));
			assertFalse("Set should not be empty after add.", set.isEmpty());
			assertEquals("Size should still be 20 after adding duplicate.", 20,
					set.size());
		}
	}

	@Test
	public void testContains() {
		fillSet();
		for (String goodValue : items) {
			String badValue = "u" + goodValue;
			assertTrue("Set should contain item " + goodValue,
					set.contains(goodValue));
			assertFalse("Set should not contain item " + badValue,
					set.contains(badValue));
		}
	}

	@Test
	public void testContainsNull() {
		fillSet();
		assertFalse("Set should not contain null.", set.contains(null));
	}

	// This test is here to help you for your assignment
	@Test
	public void testHeight() {
		assertEquals("Height should be -1 on empty set", -1, set.height());

		set.add("v1");
		assertEquals("Height should be 0 on root", 0, set.height());

		set.add("v2");
		assertEquals("Height should be 1 on root + 1 leaf", 1, set.height());
	}

	// This test is here to help you for your assignment
	@Test
	public void testHeightOnBalancedTree() {
		List<String> balancedList = Arrays.asList("v08", "v04", "v12", "v02",
				"v06", "v10", "v14", "v01", "v03", "v05", "v07", "v09", "v11",
				"v13", "v15");
		for (String it : balancedList) {
			set.add(it);
		}
		assertEquals("Height should be 3", 3, set.height());
	}

	// This test is here to help you for your assignment
	@Test
	public void testHeightOnUnbalancedTree() {
		List<String> unbalancedList = Arrays.asList("v08", "v04", "v06", "v05");
		for (String it : unbalancedList) {
			set.add(it);
		}
		assertEquals("Height should be 3", 3, set.height());
	}

}
