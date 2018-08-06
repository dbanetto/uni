// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP103, Assignment 4
 * Name:
 * Usercode:
 * ID:
 */

import ecs100.*;

import java.util.*;
import java.io.*;

/**
 * SpellingChecker checks all the words in a file against a dictionary. It
 * prints out (one per line) any word in the file that is not in the dictionary.
 * The program can be very simple - it is OK to just have a main method that
 * contains all the code, but you can use multiple methods if you want.
 * 
 * The key requirement is that the dictionary should be read from the dictionary
 * file into a set. When reading through the document with spelling errors, the
 * program should check each each word against the dictionary. Any word that is
 * not in the dictionary is considered to be an error and should be printed out.
 *
 * The program should record and print out the total time taken to read all the
 * words into the dictionary. It should also record and print out the total time
 * taken to check all the words.
 *
 * Note that the dictionary and the file to check are assumed to be all
 * lowercase, with all punctuation removed.
 */

public class SpellingChecker {

	// Suggested Design:
	//
	// 1. create a Set of String to hold the words in the dictionary.
	// (For your first version, use a HashSet; Then use your ArraySet.)
	//
	// 2.
	// read each word in the file "dictionary.txt" (use a Scanner)
	// and add each word to the dictionary Set.
	// Note that the dictionary contains just over 200,000 words
	//
	// 3.
	// Ask the user (UIFileChooser) for the file to check
	// open the file in a Scanner.
	// record the start time
	// loop through file reading each word.
	// check if the word is in dictionary set
	// if not, print out the word.
	// print out total time taken

	private ArraySet<String> myset;
	private HashSet<String> hashset;
	private boolean loaded = false;
	
	public SpellingChecker() {
		myset = new ArraySet<>();
		hashset = new HashSet<>();
		init();
	}
	
	private void init() {
		UI.initialise();
		UI.addButton("Benchmark HashSet", new UIButtonListener() {
			
			@Override
			public void buttonPerformed(String name) {
				beanchmark(hashset);
			}
		});
		
		UI.addButton("Benchmark ArraySet", new UIButtonListener() {
			
			@Override
			public void buttonPerformed(String name) {
				beanchmark(myset);
			}
		});
		
		long start = System.currentTimeMillis();
		this.readDict("dictionary.txt");
		long delta = System.currentTimeMillis() - start;
		UI.println( "Load Dictionary time : " + delta + "ms" );
	}
	
	private void readDict(String path) {
		try {
			Scanner scan = new Scanner(new File(path));

			myset = new ArraySet<>();
			hashset = new HashSet<>();

			while (scan.hasNext()) {
				String word = scan.next().toLowerCase().trim();
				if (!word.isEmpty()) {
					myset.add(word);
					hashset.add(word);
				}
			}
			scan.close(); // closes the scanner
			loaded = true;
		} catch (IOException ex) {
			UI.println("Could not read the file " + ex.getMessage());
		}
		
	}

	public void beanchmark(AbstractSet<String> set) {
		if (!loaded) {
			UI.println("Dictionary must be loaded before you can benchmark");
			return;
		}
			
		
		String path = UIFileChooser.open("Pick a file to check");
		if (path == null) {
			UI.println("Please choose a file");
			return;
		}
		
		UI.println("Starting Beanch mark" );
		long start = System.currentTimeMillis();
		
		try {
			Scanner scan = new Scanner(new File(path));
			
			while (scan.hasNext()) {
				String word = scan.next().toLowerCase().trim().replace("[", "").replace("]", "").replaceAll("['?:!.,;-]", "");
				if (!word.isEmpty()) {
					if (!set.contains(word))
						UI.println(word); // This is slows down more
				}
			}
			scan.close(); // closes the scanner
		} catch (IOException ex) {
			UI.println("Could not read the file " + ex.getMessage());
		}
		
		long delta = System.currentTimeMillis() - start;
		UI.println( "Total " +  delta + "ms" );
	}

	public static void main(String[] args) {
		new SpellingChecker();
	}
}
