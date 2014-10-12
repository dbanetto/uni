// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

import ecs100.*;
import javax.swing.*;
import java.io.*;
import java.util.*;

/**
 * This is the main class of the program. An instance of this class sets up the
 * input side of the user interface, and references a BSTSet object (the
 * dictionary). Note that this dictionary reference is not null even if the set
 * is empty.
 * 
 * Due to the size of the tree, there is no graphical display of the tree.
 */
public class DictionaryApp implements UIButtonListener {

	// use a Binary Search Tree Set of Strings for the dictionary
	private BSTSet<String> dictionary;

	/**
	 * Constructs a new DictionaryApp object.
	 * 
	 * Sets up the GUI.
	 */
	public DictionaryApp() {

		UI.addButton("Load dictionary", this);
		UI.addButton("Check text spelling", this);
		UI.addButton("Display dictionary", this);
		UI.addButton("Save dictionary", this);
		UI.addButton("Height of tree", this);
		UI.addButton("Remove bad words", this);
		UI.addButton("Iterate dictionary", this);

		dictionary = new BSTSet<String>();

	}

	// GUI methods
	/** Respond to button presses */
	public void buttonPerformed(String button) {
		if (button.equals("Load dictionary"))
			loadDictionary();
		else if (button.equals("Check text spelling"))
			checkSpelling();
		else if (button.equals("Display dictionary"))
			displayDictionary();
		else if (button.equals("Save dictionary"))
			saveDictionary();
		else if (button.equals("Height of tree"))
			heightOfTree();
		else if (button.equals("Remove bad words"))
			removeBadWords();
		else if (button.equals("Iterate dictionary"))
			iterateDictionary();
	}

	// Application functionality:

	void loadDictionary() {
		String fname = "unsorted-dictionary.txt";

		UI.println("Loading dictionary from '" + fname + "'...");

		// start with an empty dictionary
		dictionary = new BSTSet<String>();

		// reading the words one by one and adding them to the dictionary
		try {

			Scanner scan = new Scanner(new File(fname));

			while (scan.hasNext()) {
				dictionary.add(scan.next());
			}

			scan.close();
		} catch (IOException ex) {
			UI.println("Dictionary loading failed: " + ex);
		}

		UI.println("done.\n");
	}

	void checkSpelling() {

		if (dictionary.isEmpty()) {
			UI.println("No dictionary loaded yet.\n");
			return;
		}

		String fname = "plato.txt";
		int unknownWordCount = 0;

		UI.println("Spellchecking '" + fname + "'...");

		// loading the words from the file into an arraylist
		try {
			String textWord;

			Scanner scan = new Scanner(new File(fname));

			while (scan.hasNext()) {
				textWord = scan.next();

				if (!dictionary.contains(textWord)) {
					UI.print(textWord + " ");
					unknownWordCount++;
				}

			}

			scan.close();
		} catch (IOException ex) {
			UI.println("File loading failed: " + ex);
		}

		UI.println("done.\n");
		UI.println("Unknown words: " + unknownWordCount + "\n");
	}

	void displayDictionary() {

		dictionary.printAllToStream(System.out);

		UI.println("Dictionary entries: " + dictionary.size());

		UI.println("Imbalance: "
				+ (dictionary.height() - dictionary.minDepth()) + "\n");
	}

	void saveDictionary() {

		String fname = "dictionary.txt";

		UI.println("Saving dictionary to '" + fname + "'...");

		try {
			PrintStream ps = new PrintStream(new File(fname));

			dictionary.printAllToStream(ps);
		} catch (IOException ex) {
			System.out.println("File Saving failed: " + ex);
		}

		UI.println("done.\n");
	}

	void heightOfTree() {
		UI.println("Height of tree: " + dictionary.height() + "\n");
	}

	void removeBadWords() {
		String fname = "bad-words.txt";

		UI.println("Removing bad words in '" + fname + "' from dictionary...");

		try {
			Scanner scan = new Scanner(new File(fname));

			while (scan.hasNext()) {
				String next = scan.next();
				System.out.println("Removing " + next);
				dictionary.remove(next);
				System.out.println("Done");
			}

			scan.close();
		} catch (IOException ex) {
			UI.println("File loading failed: " + ex);
		}
		UI.println("done\n");
	}

	void iterateDictionary() {
		int count = 0;
		for (Iterator<String> itr = dictionary.iterator(); itr.hasNext();) {
			System.out.println(itr.next());
			count++;
		}

		UI.println("Counted " + count + " entries.\n");
	}

	// Main
	public static void main(String[] arguments) {
		new DictionaryApp();
	}
}
