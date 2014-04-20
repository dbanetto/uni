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

import java.awt.Point;
/** ImageProcessor allows the user to load, display, modify, and save an image in a number of ways.
The program should include
- Load [X], commit[X], save [X]. (Core)
- Brightness adjustment [X] (Core)
- Horizontal flip [X] and 90 degree rotation.[X] (Core)
- Merge [X] (Core)
- Crop [X] & Zoom [X]  (Core)
- Blur (3x3 filter) [X]  (Core)

- Rotate arbitrary angle [X] (Completion)
- Pour (spread-fill) [/]  (Completion)
- General Convolution Filter [X] (Completion)

- Red-eye detection and removal (Challenge)
- Filter brush (Challenge)
 */
public class ImageProcessor implements UIButtonListener, UIMouseListener,  UISliderListener, UITextFieldListener{
    /*# YOUR CODE HERE */

    private Image base_img = null;
    private Image render_img = null;
    private Image merge_img = null;
    private String filename = "";

    /*
     * Area Selectors flag
     * 0 - Do nothing
     * 1 - Crop
     * 2 - Zoom
     * 
     * Point Selection Flags
     * 10 - Pout
     * 
     * Brush Selction Flags
     * 20 - Brush
     */
    private int SELECTOR_MODE = 0;
    private Color pourColour = Color.white;
    private boolean thread_lock = false;
    private float pourTolernace = 0.25f;
    private double brushsize = 125;
    private float[][] brushfilter = null;

    public ImageProcessor()
    {

        UI.addButton("Open" , this);
        UI.addSlider("Brightness" , -100 , 100 , this);
        UI.addButton("Blur" , this);
        UI.addButton("Flip Horizontal" , this);
        UI.addButton("Convolution Filter" , this);
        
        UI.addButton("Convolution Filter Brush" , this);
        UI.addSlider("Brush Size" , 0 , 250 , this);
        UI.addButton("Flip 90 deg" , this);
        
        UI.addButton("Pour" , this);
        UI.addSlider("Pour Tolerance" , 0 , 50 , this);
        
        UI.addButton("Merge" , this);
        UI.addSlider("Merge" , 0 , 100 , this);

        UI.addButton("Zoom" , this);
        UI.addSlider("Zoom" , 1 , 200 , this);

        UI.addButton("Crop" , this);
        UI.addTextField("Crop : x,y,w,h", this);
        UI.addTextField("Rotate : x,y,angle", this);

        UI.addButton("Save" , this);
        UI.addButton("Commit" , this);

        UI.setMouseMotionListener(this);
    }

    public void buttonPerformed (String name)
    {
        //Discard 
        if (thread_lock)
            return;

        //Clear Flag
        if (SELECTOR_MODE != 0)
            SELECTOR_MODE = 0;

        if (name.equals("Open"))
        {
            thread_lock = true;
            filename = UIFileChooser.open();
            if (filename == null)
            {
                thread_lock = false;
                return;
            }
            base_img = new Image (filename);
            render_img = new Image (filename);
            this.Draw();
            thread_lock = false;
        }

        if ( base_img == null)
            return;

        if (name.equals("Commit"))
        {
            thread_lock = true;
            base_img =  render_img;
            this.Draw();
            thread_lock = false;
        }else if (name.equals("Save"))
        {
            thread_lock = true;
            String fout = UIFileChooser.save();
            if (fout == null)
            {
                thread_lock = false;
                return;
            }

            base_img.Save(fout);
            thread_lock = false;
        } else if (name.equals("Merge"))
        {
            thread_lock = true;
            String merge_fname = UIFileChooser.open("Open Another image");
            if (merge_fname == null)
            {
                thread_lock = false;
                return;
            }
            merge_img = new Image( merge_fname );
            UI.println("Opened " + merge_fname + " for merging.");
            //Display merge
            render_img = Image.applyMerge(base_img , merge_img , 0.5);
            this.Draw();
            thread_lock = false;
        } else if (name.equals("Blur"))
        {
            thread_lock = true;
            render_img = Image.applyBlur3x3(base_img);
            this.Draw();
            thread_lock = false;
        }else if (name.equals("Convolution Filter"))
        {
            thread_lock = true;
            String filter_name = UIFileChooser.open("Pick a *.filter");
            if (filter_name == null)
            {
                thread_lock = false;
                return;
            }
            float[][] filtermatrix = loadFilter(filter_name);
            if (filtermatrix != null) {
                render_img = Image.applyFilter(base_img , 0 , 0 , base_img.getWidth() , base_img.getHeight() , filtermatrix);
                this.Draw();
            }
            thread_lock = false;
        } else if (name.equals("Convolution Filter Brush"))
        {
            thread_lock = true;
            String filter_name = UIFileChooser.open("Pick a *.filter");
            if (filter_name == null)
            {
                thread_lock = false;
                return;
            }
            brushfilter = loadFilter(filter_name);
            if (brushfilter != null) {
                SELECTOR_MODE = 20;
                
                //Decrease Filter to 1%
                for (int x = 0; x < brushfilter.length; x++)
                {
                    for (int y = 0; y < brushfilter[0].length; y++)
                    {
                        brushfilter[x][y] *= 0.01f;
                    }
                }
                render_img = Image.Copy( base_img );
                this.Draw();
            }
            thread_lock = false;
        }
        else if (name.equals("Flip Horizontal"))
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
        }else if (name.equals("Pour"))
        {
            thread_lock = true;
            SELECTOR_MODE = 10;
            render_img = Image.Copy( base_img );
            UI.println("Selector entered Pour Mode.");
            UI.println("Click on a pixel to start the pour with "+ pourTolernace*100 + "% Tolerance.");
            this.Draw();
            thread_lock = false;
        } else if (name.equals("Crop"))
        {
            thread_lock = true;
            render_img = Image.Copy( base_img );
            SELECTOR_MODE = 1;
            UI.println("Selector entered Crop Mode.");
            UI.println("Click and drag on the image to select an area");
            this.Draw();
            thread_lock = false;
        }else if (name.equals("Zoom"))
        {
            thread_lock = true;
            render_img = Image.Copy( base_img );
            SELECTOR_MODE = 2;
            UI.println("Selector entered Zoom Mode.");
            UI.println("Click and drag on the image to select an area");
            this.Draw();
            thread_lock = false;
        }
    }

    private Point selection_pt = new Point(0,0) , startpoint = new Point(0,0);
    private double selection_w = 0, selection_h = 0;
    private double old_x = 0 , old_y = 0;
    private double last_applied_x = 0 , last_applied_y = 0;
    
    public void mousePerformed (String action , double x, double y)
    {
        if (thread_lock || base_img == null || render_img == null || SELECTOR_MODE == 0)
            return;

        if (SELECTOR_MODE == 1 || SELECTOR_MODE == 2) {
            if (action.equals("pressed"))
            {
                selection_pt = new Point((int)x,(int)y);
                startpoint   = new Point((int)x,(int)y);
                selection_w = 0;
                selection_h = 0;
            }else if (action.equals("dragged"))
            {
                UI.invertRect(selection_pt.getX(), selection_pt.getY(), selection_w, selection_h);

                selection_w = Math.abs( startpoint.getX() - Math.max( x , 0) );
                selection_h = Math.abs( startpoint.getY() - Math.max( y , 0) );

                selection_pt = new Point ( (int)Math.min(Math.min( startpoint.getX() , Math.max( x , 0) ), render_img.getWidth()  ), 
                                           (int)Math.min(Math.min( startpoint.getY() , Math.max( y , 0) ), render_img.getHeight() ) );
                                           
                selection_w = (int)Math.max(Math.min( selection_w + selection_pt.getX() ,render_img.getWidth() ) - selection_pt.getX()  , 0);
                selection_h = (int)Math.max(Math.min( selection_h + selection_pt.getY() ,render_img.getHeight() ) - selection_pt.getY() , 0);                           

                UI.invertRect(selection_pt.getX(), selection_pt.getY(), selection_w, selection_h);
                UI.repaintGraphics();
            }
            else if (action.equals("released"))
            {
                if (selection_w > 0 && selection_h > 0)
                {
                    switch (SELECTOR_MODE)
                    {
                        //Crop
                        case (1):
                        render_img = Image.applyCrop(base_img , (int)selection_pt.getX(), (int)selection_pt.getY(),
                            (int)selection_w, (int)selection_h);
                        break;
                        case (2):
                        double precx = (double)base_img.getWidth() / selection_w;
                        double precy = (double)base_img.getHeight() / selection_h;
                        render_img = Image.applyZoom(base_img , precx , precy ,  (int)selection_pt.getX(), (int)selection_pt.getY(),
                            base_img.getWidth(), base_img.getHeight());
                        break;
                        default:
                        UI.println("Unsupported SELECTOR_MODE used : " + SELECTOR_MODE);
                    }
                }
                //Clean up flag
                SELECTOR_MODE = 0;
                this.Draw();
            }
        }

        if (SELECTOR_MODE == 10)
        {   
            if (action.equals("released"))
            {
                thread_lock = true;
                render_img =  Image.applyPour(base_img, (int)x, (int)y, this.pourColour , pourTolernace);
                this.Draw();
                thread_lock = false;
            }
        }
        
        if (SELECTOR_MODE == 20)
        {   
            UI.invertOval(old_x - brushsize/2, old_y - brushsize/2, brushsize, brushsize);
            UI.invertOval(x - brushsize/2, y - brushsize/2, brushsize, brushsize);
            if (action.equals("dragged"))
            {
                thread_lock = true;
                
                double distance = Math.sqrt(  Math.pow((double)(last_applied_x-x) , 2 ) + Math.pow((double)(last_applied_y-y) , 2 ) );
                
                if (distance > brushsize/10)
                 {   
                    render_img =  Image.applyBrushFilter(render_img, (int)x, (int)y, (int)brushsize , brushfilter);
                
                    last_applied_x = x;
                    last_applied_y = y;
                }
                thread_lock = false;
            }
            if (action.equals("released"))
            {
                SELECTOR_MODE = 0;
                this.Draw();
            }
            UI.repaintGraphics();
        }
        
        old_x = x;
        old_y = y;
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
        } else if (name.equals("Zoom"))
        {
            thread_lock = true;
            render_img = Image.applyZoom(base_img , value/100.0);
            this.Draw();
            thread_lock = false;
        } else if (name.equals("Pour Tolerance"))
        {
            thread_lock = true;
            pourTolernace = (float)(value/100.0);
            thread_lock = false;
        } else if (name.equals("Brush Size"))
        {
            thread_lock = true;
            brushsize = value;
            thread_lock = false;
        }
    }

    public void textFieldPerformed(String name, String newText)
    {
        if (thread_lock || render_img == null || base_img == null)
            return;

        if (name.equals("Crop : x,y,w,h"))
        {

            String[] paras = newText.split(",");
            if (paras.length != 4)
            {
                UI.println("Invalid input.\nNot Enough parameters\nRequires a string of postive integers seperated by commas\n eg: 5,5,5,5");
                return;
            }
            try {
                int x = Integer.parseInt(paras[0]);
                int y = Integer.parseInt(paras[1]);
                int w = Integer.parseInt(paras[2]);
                int h = Integer.parseInt(paras[3]);

                if (x < 0 || y < 0 || w < 0 || h < 0)
                {
                    UI.println("Invalid input.\nAll values must be postive\nRequires a string of postive integers seperated by commas\n eg: 5,5,5,5");
                    return;
                }

                thread_lock = true;
                render_img = Image.applyCrop(base_img , x, y,w, h);
                this.Draw();
                thread_lock = false;
            } catch (NumberFormatException ex)
            {
                UI.println("Number Format Exception : " + ex.toString());
                return;
            }

        } else if (name.equals("Rotate : x,y,angle"))
        {

            String[] paras = newText.split(",");
            if (paras.length != 3)
            {
                UI.println("Invalid input.\nNot Enough parameters\nRequires a string of postive integers seperated by commas\n eg: 5,5,5");
                return;
            }
            try {
                int x = Integer.parseInt(paras[0]);
                int y = Integer.parseInt(paras[1]);
                int a = Integer.parseInt(paras[2]);

                if (x < 0 || y < 0 || a < 0)
                {
                    UI.println("Invalid input.\nAll values must be postive\nRequires a string of postive integers seperated by commas\n eg: 5,5,5,5");
                    return;
                }

                thread_lock = true;
                render_img = Image.applyRotate(base_img , x, y, a);
                this.Draw();
                thread_lock = false;
            } catch (NumberFormatException ex)
            {
                UI.println("Number Format Exception : " + ex.toString());
                return;
            }
        }
    }

    public void Draw()
    {
        UI.clearGraphics(false);
        if (render_img != null && base_img != null)
        {
            render_img.Draw(0, 0);
            base_img.Draw(render_img.getWidth(), 0);
        }
        UI.repaintGraphics();
    }

    public static float[][] loadFilter (String fname)
    {
        float[][] out = null;
        try {

            BufferedReader br = new BufferedReader(new FileReader(fname));
            int width = 0, height = 0;
            String line = "";
            int lineNum = 0;

            int x = 0, y= 0 ;
            while ( (line = br.readLine()) != null )
            {
                if (line.equals(""))
                    continue;
                line = line.split("#")[0].trim();

                if (line.equals(""))
                    continue;

                if (lineNum == 0)
                {
                    String[] dimension = line.split(",");

                    assert (dimension.length == 2 ) : "Invaild Dimension in " + fname + 
                    ". Expected two ints, got " + dimension.length;

                    width = Integer.parseInt(dimension[0]);
                    height = Integer.parseInt(dimension[1]);

                    out = new float[width][height];

                    assert (width > 1) : "Width is less than zero";
                    assert (height > 1) : "Height is less than zero";
                    lineNum++;
                    continue;
                } else {
                    String[] ln = line.split(",");
                    x = 0;
                    for (String val : ln)
                    {
                        out[x][y] = Float.parseFloat(val);
                        x++;
                        if(x == width)
                            break;
                    }
                    y++;
                }
            }
        } catch (IOException ex)
        {
            UI.println(ex.toString());
        } catch (Exception ex)
        {
            UI.println(ex.toString());
        }
        return out;
    }

    public static void main (String[] args)
    {
        int post = Image.FloatToIntARGB( Image.PixelIntToFloat( Color.blue.getRGB() ) );
        assert Color.blue.getRGB() == post : "Float to Int does not work properly normal("+Color.blue.getRGB()+") Image("+post+")";

        ImageProcessor imagesProc = new ImageProcessor();
        UI.setImmediateRepaint(false);

    }
}
