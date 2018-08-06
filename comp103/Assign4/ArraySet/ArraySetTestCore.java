// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.


/* Code for COMP 103, Assignment 4
 * Name:
 * Usercode:
 * ID:
 */

import java.util.Iterator;
import java.util.Set;

import static org.junit.Assert.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.JUnitCore;

/** ArraySetTestCore
 *  A JUnit class for testing ArraySet with just a core set of tests.
 */

public class ArraySetTestCore {

    private Set<String> set;

    /** initialise set to be an empty set before each test is run */
    @Before
    public void initialiseEmptySet() {
	set = new ArraySet<String>();
    }

    /** method to initialise the set with the n values "v1", "v2", ... */
    public void fillSet(int n){
	for (int i = 1; i <= n; i++) {
	    set.add("v" + i);
        }
    }
    //-------------------------------------------------------------------

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
	for (int i = 1; i <= 20; i++) {
	    assertTrue("Set should successfully add item " + i, set.add("v" + i));
	    assertFalse("Set should not be empty after add", set.isEmpty());
	    assertEquals("Size should be " + i + " after " + i + " adds", i, set.size());
	}
    }

    @Test
    public void testAddingNull() {
        try {
            set.add(null);
            assertTrue("add(null) should have thrown an exception", false);
        }
        catch (IllegalArgumentException ex) {}
    }
    
    @Test
    public void testAddingDuplicates() {
        fillSet(20);
        for (int i = 1; i <= 20; i++) {
	    String value = "v" + i;
            assertFalse("Set should not add duplicate item " + value, set.add(value));
            assertFalse("Set should not be empty after add.", set.isEmpty());
            assertEquals("Size should still be 20 after adding duplicate.", 20, set.size());
        }
    }
    
    @Test
    public void testContains() {
        fillSet(20);
        for (int i = 1; i <= 20; i++) {
	    String goodValue = "v"+i;
	    String badValue = "u"+i;
            assertTrue("Set should contain item " + goodValue, set.contains(goodValue));
            assertFalse("Set should not contain item " + badValue, set.contains(badValue));
        }
    }
    
    @Test
    public void testContainsNull() {
        fillSet(15);
        assertFalse("Set should not contain null.", set.contains(null));
    }
    
}


