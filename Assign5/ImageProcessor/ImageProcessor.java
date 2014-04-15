// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP 102 Assignment 9 
 * Name: David Barnett
 * Usercode: barnetdavi
 * ID: 300313764
 */

import ecs100.*;
import java.util.*;
import java.io.*;
import java.awt.Color;
import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import javax.swing.JColorChooser;

/** ImageProcessor allows the user to load, display, modify, and save an image in a number of ways.
The program should include
 - Load [X], commit[X], save. (Core)
 - Brightness adjustment [X] (Core)
 - Horizontal flip[X] and 90 degree rotation.[X] (Core)
 - Merge  (Core)
 - Crop & Zoom  (Core)
 - Blur (3x3 filter) [X]  (Core)

 - Rotate arbitrary angle (Completion)
 - Pour (spread-fill)  (Completion)
 - General Convolution Filter [X](Completion)

 - Red-eye detection and removal (Challenge)
 - Filter brush (Challenge)
 */
public class ImageProcessor implements UIButtonListener, UIMouseListener,  UISliderListener, UITextFieldListener{
    /*# YOUR CODE HERE */
    
    private Image base_img = null;
    private Image render_img = null;
    private Image merge_img = null;
    private String filename = "";
    
    private boolean thread_lock = false;

    public ImageProcessor()
    {
        UI.addButton("Open" , this);
        
        UI.addSlider("Brightness" , -100 , 100 , this);
        UI.addButton("Blur" , this);
        UI.addButton("Flip Horizontal" , this);
        UI.addButton("Sharpen" , this);
        UI.addButton("Flip 90 deg" , this);
        
        UI.addButton("Merge" , this);
        UI.addSlider("Merge" , 0 , 100 , this);

        UI.addButton("Commit" , this);
        
        UI.addTextField("Text", this);
    }

    public void buttonPerformed (String name)
    {
    	//Discard 
    	if (thread_lock)
    		return;

        if (name.equals("Open"))
        {
            thread_lock = true;
            filename = UIFileChooser.open();
            base_img = new Image (filename);
            render_img = new Image (filename);
            this.Draw();
            thread_lock = false;
        }else if (name.equals("Commit"))
        {
            thread_lock = true;
            base_img =  render_img;
            this.Draw();
            thread_lock = false;
        } else if (name.equals("Blur"))
        {
            thread_lock = true;
            render_img = Image.applyBlur3x3(base_img);
            this.Draw();
            thread_lock = false;
        } else if (name.equals("Sharpen"))
        {
            thread_lock = true;
            render_img = Image.applySharpen3x3(base_img);
            this.Draw();
            thread_lock = false;
        } else if (name.equals("Flip Horizontal"))
        {
            thread_lock = true;
            render_img = Image.applyHorizontalFlip(base_img);
            this.Draw();
            thread_lock = false;
        } else if (name.equals("Flip 90 deg"))
        {
            thread_lock = true;
            render_img = Image.apply90degFlip(base_img);
            this.Draw();
            thread_lock = false;
        } else if (name.equals("Merge"))
        {
            thread_lock = true;
            merge_img = new Image( UIFileChooser.open());
            this.Draw();
            thread_lock = false;
        }
    }

    public void mousePerformed (String action , double x, double y)
    {
    	if (thread_lock)
    		return;
    }
    
    public void sliderPerformed(String name, double value)
    {
        if (thread_lock || render_img == null || base_img == null)
    		return;

        if (name.equals("Brightness"))
        {
        	thread_lock = true;
            render_img = Image.applyBrightness(base_img , 100/(value+100));
            this.Draw();
            thread_lock = false;
        } else if (name.equals("Merge") && merge_img != null)
        {
        	thread_lock = true;
            render_img = Image.applyMerge(base_img , merge_img , value/100.0);
            this.Draw();
            thread_lock = false;
        }
    }

    public void textFieldPerformed(String name, String newText)
    {
        
        UI.println(name + ":" + newText );
    }
    
    public void Draw()
    {
        UI.clearGraphics(false);
        render_img.Draw(0, 0);
        base_img.Draw(render_img.getWidth(), 0);
        UI.repaintGraphics();
    }
    
    public static void main (String[] args)
    {
        ImageProcessor imagesProc = new ImageProcessor();
        UI.setImmediateRepaint(false);
    }

}
