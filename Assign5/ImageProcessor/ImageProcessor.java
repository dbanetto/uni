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
 - Load, commit, save. (Core)
 - Brightness adjustment (Core)
 - Horizontal flip and 90 degree rotation. (Core)
 - Merge  (Core)
 - Crop&Zoom  (Core)
 - Blur (3x3 filter)  (Core)

 - Rotate arbitrary angle (Completion)
 - Pour (spread-fill)  (Completion)
 - General Convolution Filter  (Completion)

 - Red-eye detection and removal (Challenge)
 - Filter brush (Challenge)
 */
public class ImageProcessor implements UIButtonListener, UIMouseListener,  UISliderListener, UITextFieldListener{
    /*# YOUR CODE HERE */
    
    private Image base_img = null;
    private Image render_img = null;
    
    private String filename = "";
    
    public ImageProcessor()
    {
        UI.addButton("Open" , this);
        
        UI.addSlider("Brightness" , -100 , 100 , this);
        UI.addButton("Blur" , this);
       
        UI.addTextField("Text", this);
    }

    public void buttonPerformed (String name)
    {
        if (name.equals("Open"))
        {
            filename = UIFileChooser.open();;
            base_img = new Image (filename);
            render_img = new Image (filename);
            this.Draw();
        } else if (name.equals("Blur"))
        {
            render_img = Image.applyBlur(base_img);
            this.Draw();
        }
    }

    public void mousePerformed (String action , double x, double y)
    {

    }
    
    public void sliderPerformed(String name, double value)
    {
        if (name.equals("Brightness") && render_img != null)
        {
            render_img = Image.applyBrightness(base_img , 100/(value+100));
            this.Draw();
        } else if (name.equals("Constrast") && render_img != null)
        {

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
