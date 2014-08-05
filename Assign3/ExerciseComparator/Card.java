// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP103 assignment.
// You may not distribute it in any other way without permission.


import ecs100.UI;

import java.util.*;
import java.io.*;
import java.awt.Image;
import javax.imageio.ImageIO;
import java.net.URL;
import java.net.URLClassLoader;

public class Card implements Comparable<Card>{

    public static final String[] rankNames = new String[]{"", "ace" ,"2" ,"3" ,"4" ,"5" ,"6" ,"7" ,"8" ,"9" ,"10" ,"jack" ,"queen","king"};

    public static final List<String> suitNames = Arrays.asList(new String[]{"hearts", "diamonds", "spades", "clubs"});

    private String suit;
    private int rank;
    private Image image;

    /** Create a new Card of the given suit and rank. */
    public Card(String s, int r) {
	suit = s.toLowerCase();
        rank = r;
	if (!suitNames.contains(suit) || rank<1 || rank > 13) {
	    throw new RuntimeException("Invalid card specification");
	}
	image = getImage(rankNames[rank]+ "_of_"+ suit+".png");
    }

    public String getSuit() {return suit;}
    public int getRank() {return rank;}
    public String toString() {return (rankNames[rank]+ " of "+ suit);}

    /** This card comes first if its suit comes AFTER the other suit (alphabetically)
     *  or the suits are the same and the rank is less */
    public int compareTo(Card other){
	UI.println("calling Card.compareTo, \nNot implemented yet!!!!");
	/*# YOUR CODE HERE */
    }

    /** Draws the front of a card on the UI. Does not repaint the graphics */
    public void draw(double x, double y) {
        UI.drawImage(image, x, y, false);
    }

    /** Method to get the image associated with a given name out of the zip file.
	By storing the image rather than the image file name,
        the drawing is much faster - it does not have to read
        the image file each time it draws the card. */
    private static Image getImage(String name){
	Image image = null;
	URL source = Card.class.getResource("cards-75px.zip");
	if (source == null) {
	    System.out.println("Could not find card images");
	    return null;
	}
	URLClassLoader imageSource = URLClassLoader.newInstance(new URL[] { source });
        try {
	    image = ImageIO.read(imageSource.getResourceAsStream(name));
	} catch(IOException e){System.out.println("Reading Card Image failed: "+e);}
	return image;
    }
}
