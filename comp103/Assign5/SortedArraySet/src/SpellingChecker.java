// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.


/* Code for COMP103, Assignment 5
 * Name:
 * Usercode:
 * ID:
 */

import ecs100.*;
import java.awt.Color;
import java.util.*;
import java.io.*;

/** SpellingChecker checks all the words in a file against a dictionary.
 *  It prints out (one per line) any word in the file that is not in the dictionary.
 *  The program can be very simple - it is OK to just have a main method
 *  that contains all the code, but you can use multiple methods if you want.
 *  
 *  The key requirement is that the dictionary should be read from the
 *  dictionary file into a set.
 *  When reading through the document with spelling errors, the program should
 *  check each each word against the dictionary.
 *  Any word that is not in the dictionary is considered to be an error and
 *  should be printed out.
 *
 *  The program should record and print out the total time taken to read
 *  all the words into the dictionary.
 *  It should also record and print out the total time taken to check all the words.
 *
 *  Note that the dictionary and the file to check are assumed to be all
 *  lowercase, with all punctuation removed.
 */

public class SpellingChecker{

    // Suggested Design:
    // 
    // 1. create a Set of String to hold the words in the dictionary.
    //    (For your first version, use a HashSet; Then use your ArraySet.)
    //
    // 2. 
    //   read each word in the file "dictionary.txt" (use a Scanner)
    //   and add each word to the dictionary Set.
    //   Note that the dictionary contains just over 200,000 words
    //
    // 3. 
    //   Ask the user (UIFileChooser) for the file to check
    //   open the file in a Scanner.
    //   record the start time
    //   loop through file reading each word.
    //   check if the word is in dictionary set
    //   if not, print out the word.
    //   print out total time taken
    public static void main(String[] arguments){
        UI.println("Reading Dictionary");
        try{

            // Scanner dictFile = new Scanner(new File("dictionaryUnsorted.txt"));
            Scanner dictFile = new Scanner(new File("dictionarySorted.txt"));

            //load the words from the file into an arraylist, to avoid measuring
            //the file reading time
            List<String> dwords = new ArrayList<String>();
            while (dictFile.hasNext()){ dwords.add(dictFile.next()); }

            //measure the set construction
            long start = System.currentTimeMillis();
            Set<String> dictionary = new SortedArraySet<String>();
            //Set<String> dictionary = new ArraySet<String>();
            // Set<String> dictionary = new HashSet<String>();
            for (String word : dwords){
                dictionary.add(word);
                if (dictionary.size()%1000==0) UI.println(dictionary.size());
            }

            long loading = (System.currentTimeMillis()-start);
            dictFile.close();
            UI.println("Dictionary Loaded");

            //load the words from the file into an arraylist, to avoid measuring
            //the file reading time
            Scanner wordFile = new Scanner(new File("plato.txt")); //UIFileChooser.open()));
            List<String> words = new ArrayList<String>();
            while (wordFile.hasNext()){ words.add(wordFile.next().toLowerCase()); }

            //measure the set construction
            start = System.currentTimeMillis();
            for (String word : words){
                if (!dictionary.contains(word)) {
                    UI.println(word);
                }
            }
            long checking = (System.currentTimeMillis()-start);
            UI.printf("------------------\nLoading took: %.3f seconds\nChecking took: %.3f seconds\n",
                loading/1000.0, checking/1000.0);

            wordFile.close();
        }
        catch(IOException e){UI.println("Fail: " + e);}

    }	
}
