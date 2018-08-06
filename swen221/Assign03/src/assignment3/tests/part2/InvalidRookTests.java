package assignment3.tests.part2;

import org.junit.Test;
import junit.framework.TestCase;
import static assignment3.tests.TestHelpers.*;

public class InvalidRookTests extends TestCase {
	
	public @Test void testInvalidRookMoves() {
		String[] tests = {
			"Ra1xd2",
			"Ra1xa2",
			"Ra1xb2",
			"Ra1xb1",
		};
		checkInvalidTests(tests);
	}
	
	public @Test void testInvalidRookTakes() {
		String[] tests = {
			"Ra1xd2",
			"Ra1xa2",
			"Ra1xb2",
		};
		
		checkInvalidTests(tests);
	}
}
