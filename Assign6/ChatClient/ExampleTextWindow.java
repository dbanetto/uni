// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

/* Example Code for COMP112 Assignment 
 */

import ecs100.*;
import java.util.*;
import java.io.*;
import java.awt.Color;

import javax.swing.JFrame;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;
import java.awt.BorderLayout;


/**
 * ExampleTextWindow
 * Creates an extra window with a text area that strings can be written to.
 */
public class ExampleTextWindow{

    // The main method creates the text area and passes it to the interact method.
    // But it could have stored the text area in a field if many methods
    // needed to access it.

    // Main creates an ExampleTextWindow object. But if the other methods
    // had been static, it wouldn't have needed to even do this.
    // Alternatively, the calls to createNewFrame and interact could have been
    // in the constructor, and then main could have just constructed a new
    // ExampleTextWindow object.

    public static void main(String[] arguments){
        ExampleTextWindow etw = new ExampleTextWindow();
        JTextArea textOutput = etw.createNewFrame();
        etw.interact(textOutput);
    }        


    /**
     * Create and return a JTextArea in a new JFrame.
     * The text area will have scroll bars around it.
     * Other methods can then write to the JTextArea using the append(...) method
     */
    public JTextArea createNewFrame()
    {
        JFrame frame = new JFrame("Output");    // make a frame
        frame.setSize(200,300);// set its size
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // make it close properly
     
        JTextArea textA = new JTextArea(40,60);  // text area (lines, chars per line)
        JScrollPane textSP = new JScrollPane(textA); // put scrollbars around it
        frame.add(textSP, BorderLayout.CENTER);              // add it to the frame.
        frame.pack();                                        // pack things in to the frame
        frame.setVisible(true);                              // make it visible.
        return textA;
    }


    /** Copy every line the user types to the text pane */
    public void interact(JTextArea textOutput) {
        UI.println("Enter text on multiple lines"); 
        while (true){
            UI.print(">");
            String line = UI.nextLine();                // read a line from the user
            textOutput.append("COPY: "+line+"\n"); // put it in the text output area
                                                        // Need to specify the end of line
            if (line.startsWith("clear")){
                textOutput.setText(null);  // clear the text area
            }
            if (line.contains("quit")){
                UI.quit();
            }
        }
    }


}
