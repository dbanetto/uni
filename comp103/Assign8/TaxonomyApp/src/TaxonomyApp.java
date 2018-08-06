// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.


import ecs100.*;
import javax.swing.*;
import java.io.*;
import java.util.*;

/**
 * This is the main class of the entire program. It sets up the input side
 * of the user interface, and maintains a GeneralTree object. Note that this
 * GeneralTree object is not null even if there are no actual nodes inside the tree.
 * 
 * The actual display of the tree is handled in the GeneralTree class itself.
 * This class deals with input, and the output of responses to report queries.
 *
 */
public class TaxonomyApp implements UIButtonListener {

  private GeneralTree taxonomy;  // the tree representing the taxonomy

  /** 
   * Constructs a new TaxonomyApp object.
   * Sets up the GUI
   */
  public TaxonomyApp() {

    taxonomy = new GeneralTree();

    // General buttons
    UI.addButton("New taxonomy", this);
    UI.addButton("Load taxonomy", this);
    UI.addButton("Save taxonomy", this);

    UI.addButton("", this);

    // Nodes buttons
    UI.addButton("Add node", this);
    UI.addButton("Remove node", this);
    UI.addButton("Move subtree", this);

    UI.addButton("", this);

    // Report buttons
    UI.addButton("Report all below", this);
    UI.addButton("Report all above", this);
    UI.addButton("Report all same depth nodes", this);
    UI.addButton("Find closest ancestor", this);

    taxonomy.redraw();
  }

  // GUI methods

  /** Respond to button presses */
  public void buttonPerformed(String button) {
    if (button.equals("New taxonomy")) {
      String rootString = UI.askString("Name of root node:");

      taxonomy = new GeneralTree();
      taxonomy.addNode(rootString, null);
      taxonomy.redraw();
    }
    else if (button.equals("Load taxonomy")) {

      String fname = UIFileChooser.open("Filename to read text from");

      if (fname == null) {
        JOptionPane.showMessageDialog(null, "No file name specified");
        return;
      }
      try {
        Scanner scan = new Scanner(new File(fname));
        taxonomy.load(scan);
        scan.close();
      }
      catch(IOException ex) {
        UI.println("File loading failed: " + ex);
      } 
      taxonomy.redraw();
    }
    else if (button.equals("Save taxonomy")) {
      taxonomy.save();
    }
    else if (button.equals("Add node")) {
      String name;
      String parentName = UI.askString("Parent to add under:");
      
      // translate an empty input into the value required by 'addNode'
      if (parentName.length() == 0)
      {
        parentName = null;
        name = UI.askString("Name to add above root:");
      }
      else
        name = UI.askString("Name to add under " + parentName + ":");
        
      taxonomy.addNode(name, parentName);
      taxonomy.redraw();
    }
    else if (button.equals("Remove node")) {
      String name = UI.askString("Name to remove:");
      taxonomy.removeNode(name);
      taxonomy.redraw();
    }
    else if (button.equals("Move subtree")) {
      String subtreeName = UI.askString("Name at root of subtree:");
      String destinationName = UI.askString("To new parent destination");
      taxonomy.moveSubtree(subtreeName, destinationName);
      taxonomy.redraw();
    }
    //Reports
    else if (button.equals("Report all below")) {
      UI.println("Subtree:");
      String subtreeRootName = UI.askString("Name at root of subtree:");
      taxonomy.printSubtreeFrom(subtreeRootName);
    }
    else if (button.equals("Report all above")) {
      String targetName = UI.askString("Name of target node:");
      taxonomy.printPathToRootFrom(targetName);
    }
    else if (button.equals("Report all same depth nodes")) {
      int depth = UI.askInt("Report at depth:");
      taxonomy.printAllAtDepth(depth);
    }
    else if (button.equals("Find closest ancestor")) {
      String name1 = UI.askString("First name:");
      String name2 = UI.askString("Second name:");
      String closestAncestor = taxonomy.findClosestCommonAncestor(name1, name2);
      UI.println("Closest ancestor is: " + closestAncestor);
    }
  }

  public static void main(String[] arguments) {
    new TaxonomyApp();
  }   

}
