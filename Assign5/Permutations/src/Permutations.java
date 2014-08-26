// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP103, Assignment 5
 * Name:
 * Usercode:
 * ID:
 */

import ecs100.*;
import java.util.*;

/**
 * Prints out all permutations of a string The static method permute constructs
 * all the permutations The main method gets the string, calls recPermute, and
 * prints the result.
 */
public class Permutations {

	/** Return a List of all the permutations of a String. */
	public static List<String> recPermute(String string) {
		return powerSet(string, new ArrayList<String>());
	}

	private static List<String> powerSet(String str, List<String> list) {
		if (str.length() == 1) {
			list.add(str);
			return list;
		}

		List<List<String>> lists = new ArrayList<>(str.length());
		
		// Split
		for (int i = 0; i < str.length(); i++) {
			String nstr = str.substring(0, i) +
					str.substring(i + 1);
			
			List<String> ls = new ArrayList<String>();
			String rmed = str.substring(i, i + 1);
			for ( String parts : powerSet(nstr, new ArrayList<String>())) {
				ls.add(rmed + parts);
			}
			lists.add(ls);
		}

		// Merge
		for (List<String> arry : lists) {
			for (String parts : arry) {
				list.add(parts);
			}
		}

		return list;
	}

	public List<String> PowerSet(String string) {
		List<String> out = new ArrayList<String>();
		out.add(null);

		return out;
	}

	// Main
	public static void main(String[] arguments) {
		String string = "";
		while (!string.equals("#")) {
			string = UI.askString("Enter string to permute - # to exit: ");
			List<String> permutations = recPermute(string);
			for (String p : permutations)
				UI.println(p);
			UI.println("---------");
		}
		UI.quit();
	}
}
