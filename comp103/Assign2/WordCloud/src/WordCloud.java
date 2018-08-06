// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP 103, Assignment 2
 * Name: David Barnett
 * Usercode: barnetdavi
 * ID: 300313764
 */

import ecs100.*;

import java.awt.Color;
import java.awt.RenderingHints;
import java.util.*;
import java.io.*;
import java.lang.reflect.Array;

/**
 * This program reads 2 text files and compiles word counts for each. It then
 * eliminates rare words, and words that only occur in one document, and
 * displays the remainder as a "word cloud" on a graphics pane, to allow the
 * user to examine differences between the word usage in the two documents.
 */
public class WordCloud implements UIButtonListener {

	// Fields:
	private int numWordsToRemove = 100;

	// The two maps.
	private Map<String, Double> counts1, counts2;

	// Constructor
	/**
	 * Constructs a WordCloud object Set up the graphical user interface, and
	 * call the basic method.
	 */
	public WordCloud() {
		// Set up the GUI.
		UI.addButton("remove standard common words", this);
		UI.addButton("remove infrequent words", this);
		UI.addButton("remove un-shared words", this);

		String fname1 = UIFileChooser.open("First filename to read text from");
		counts1 = buildHistogram(fname1);
		UI.println("Text read from " + fname1);

		String fname2 = UIFileChooser.open("Second filename to read text from");
		counts2 = buildHistogram(fname2);
		UI.println("Text read from " + fname2);
		UI.getGraphics().setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
				RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
		displayWords();
	}

	/**
	 * Read the contents of a file, counting how often each word occurs. Put the
	 * counts (as Doubles) into a Map, which is returned. [CORE]
	 */
	public Map<String, Double> buildHistogram(String filename) {
		if (filename == null)
			return null;
		Map<String, Double> wordcounts;
		double total = 0.0;
		try {
			// Open the file and get ready to read from it
			Scanner scan = new Scanner(new File(filename));

			// The next line tells Scanner to remove all punctuation
			scan.useDelimiter("[^-a-zA-Z']");

			wordcounts = new HashMap<String, Double>();

			while (scan.hasNext()) {
				String word = scan.next().trim();
				if (!word.isEmpty())
					wordcounts
							.put(word,
									wordcounts.containsKey(word) ? wordcounts
											.get(word) + 1 : 1);
			}

			scan.close(); // closes the scanner
			return wordcounts;
		} catch (IOException ex) {
			UI.println("Could not read the file " + ex.getMessage());
			return null;
		}
	}

	/**
	 * Construct and return a Set of all the words that occur in EITHER
	 * document. [CORE]
	 */
	public Set<String> findAllWords() {
		Set<String> words = counts1.keySet();
		words.addAll(counts2.keySet());
		return words;
	}

	/**
	 * Display words that exist in both documents.
	 * 
	 * The x-position is essentially random (it just depends on the order in
	 * which an iterator goes through a Set).
	 * 
	 * However the y-position reflects how much the word is used in the 1st
	 * document versus the 2nd. That is, a word that is common in the 1st and
	 * uncommon in the second should appear at the top.
	 * 
	 * The SIZE of the word as displayed reflects how common the word is
	 * overall, including its count over BOTH documents. NB! There is
	 * UI.setFontSize method that may come in useful!
	 * 
	 * [CORE]
	 */
	public void displayWords() {
		UI.clearGraphics();
		if ((counts1 == null) || (counts2 == null))
			return;

		// First we re-normalise the counts.
		Map<String, Double> norm1 = normaliseCounts(counts1);
		Map<String, Double> norm2 = normaliseCounts(counts2);
		Random rnd = new Random();
		for (String word : norm1.keySet()) {
			if (norm2.containsKey(word)) {
				// The Code below makes the Y position proportional to the
				// relative occurrence of the word in both documents.
				// The doc above describes this function to be built this way,
				// while the online assignment doc
				// in conflict says X position should be proportional. I am
				// keeping it as Y as it looks better
				double prec = norm1.get(word)
						/ (norm1.get(word) + norm2.get(word));
				double fnt = (norm1.get(word) + norm2.get(word) * 100) / 2.0;
				UI.setFontSize((int) (36.0 * fnt));
				// The Colour (using HSV model) of the text is proportional to
				// the relative percentage to give a nice
				// transition of colours down the screen. Saturation is the log of length and
				// brightness is randomised for some variety
				UI.setColor(Color.getHSBColor((float) prec, (float)Math.log((double)word.length()),
						rnd.nextFloat()));
				UI.drawString(word, rnd.nextInt(UI.getCanvasWidth() - 50),
						UI.getCanvasHeight() * prec);
			}
		}
	}

	/**
	 * Take a word count Map, and a Set of words. Remove those words from the
	 * Map. [COMPLETION]
	 */
	public void removeWords(Map<String, Double> wc, Set<String> words) {
		for (String rm : words) {
			wc.remove(rm);
		}
	}

	/**
	 * Takes a Map from strings to integers, and an integer, limitNumWords. It
	 * should leave this Map containing only the limitNumWords most common words
	 * in the original. [COMPLETION]
	 */
	public void removeInfrequentWords(Map<String, Double> c, int limitNumWords) {
		
		// Fixes Program with assessment compiler errors
		final Map<String, Double> wc = c;
		Comparator<String> cmp = new Comparator<String>() {
			@Override
			public int compare(String a, String b) {
				if (wc.get(a) >= wc.get(b)) {
					return -1;
				} else {
					return 1;
				}
			}
		};

		// ArrayList<String> keys = new ArrayList<String>();
		String[] keys = new String[c.size()];
		c.keySet().toArray(keys);
		Arrays.sort(keys, cmp);
		int n = 0;
		for (Object k : keys) {
			n++;
			if (n > limitNumWords)
				c.remove(k);
		}
	}

	/**
	 * Take a Map from words to counts, and "normalise" the counts, so that they
	 * are fractions of the total: they should sum to one.
	 */
	public Map<String, Double> normaliseCounts(Map<String, Double> counts) {
		// Figure out the total in the current Map
		if (counts == null)
			return null;
		Map<String, Double> out = new HashMap<String, Double>();
		double total = 0.0;
		for (String wd : counts.keySet())
			total += counts.get(wd);

		// Divide all values by the total, so they will sum to one.
		for (String wd : counts.keySet())
			out.put(wd, counts.get(wd) / total);
		return out;
	}

	/**
	 * Print the words and their counts to standard out. Not necessary to the
	 * program, but might be useful for debugging
	 */
	public void printcounts(Map<String, Double> counts) {
		if (counts == null) {
			UI.println("The Map is empty");
			return;
		}
		for (String s : counts.keySet())
			UI.printf("%15s \t : \t %.3f \n", s, counts.get(s));
		UI.println("----------------------------------");
	}

	// -- GUI stuff --------------------------------------------------------
	/** Respond to button presses */
	public void buttonPerformed(String button) {

		if (button.equals("remove standard common words")) {
			String fname = UIFileChooser
					.open("filename to read common words from"); // More general
																	// form: ;
			if (fname == null)
				return;
			UI.println("Getting ignorable words from " + fname);

			// Set the elements of the toRemove Set to be the words in file
			try {
				Set<String> toRemove = new HashSet<String>();
				Scanner scan = new Scanner(new File(fname));
				while (scan.hasNext()) {
					String str = scan.next().toLowerCase().trim();
					toRemove.add(str);
				}
				scan.close();

				// Remove the words
				removeWords(counts1, toRemove);
				removeWords(counts2, toRemove);
			} catch (IOException ex) { // what to do if there is an io error.
				UI.println("Could not read the file " + ex.getMessage());
			}
		}

		else if (button.equals("remove infrequent words")) {
			UI.println("Keeping only the most common " + numWordsToRemove
					+ " words");
			removeInfrequentWords(counts1, numWordsToRemove);
			removeInfrequentWords(counts2, numWordsToRemove);
			numWordsToRemove = numWordsToRemove / 2; // It halves each time.
		}

		else if (button.equals("remove un-shared words")) {
			UI.println("Keeping only words that occur in BOTH docs ");
			Set<String> wordsToBeRemoved = new HashSet<String>();
			for (String wd : counts1.keySet())
				if (!counts2.keySet().contains(wd))
					wordsToBeRemoved.add(wd);
			for (String wd : counts2.keySet())
				if (!counts1.keySet().contains(wd))
					wordsToBeRemoved.add(wd);
			// Notice you do need to do both!
			// Now actually remove them.
			removeWords(counts1, wordsToBeRemoved);
			removeWords(counts2, wordsToBeRemoved);
		}

		printcounts(counts1);
		printcounts(counts2);

		// Now redo everything on the screen
		displayWords();
	}

	// ================================================================
	// Main
	public static void main(String[] args) {
		// Fixes a change in Java7 which makes the comparator in
		// removeInfrequent to cause a contract voliation error
		// From :
		// http://stackoverflow.com/questions/13575224/comparison-method-violates-its-general-contract-timsort-and-gridlayout
		System.setProperty("java.util.Arrays.useLegacyMergeSort", "true");
		new WordCloud();
	}
}
