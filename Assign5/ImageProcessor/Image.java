import java.awt.Color;
import ecs100.*;
import java.util.*;
import java.io.*;
import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;

public class Image
{
    private int[][][] pixels = new int[1][1][3];
    private int width = -1;
    private int height = -1;


    public Image(int width, int height , Color color)
    {
        assert height > 0 : "Height has to be greater than 0";
        assert width > 0 : "Width has to be greater than 0";
        
        this.width = width;
        this.height = height;
    }

    public Image(String filename)
    {
        try {
            BufferedImage image = ImageIO.read(new File(filename));

            this.width = image.getWidth();
            this.height = image.getHeight();

            this.pixels = new int[width][height][3];

            for (int x = 0; x < pixels.length; x++)
            {
                for (int y = 0; y < pixels[x].length; y++)
                {
                    int pixelARGB = image.getRGB(x,y);

                    this.pixels[x][y][0] = (pixelARGB >> 16) & 255;
                    this.pixels[x][y][1] = (pixelARGB >>  8) & 255;
                    this.pixels[x][y][2] =  pixelARGB        & 255;
                }
            }

        } catch (IOException ex)
        {
            UI.println(ex.toString());
        }
    }

    public void Draw(int xpos , int ypos)
    {
        assert height > 0 : "Height has to be greater than 0";
        assert width > 0 : "Width has to be greater than 0";
        
        for (int x = 0; x < pixels.length; x++)
        {
            for (int y = 0; y < pixels[x].length; y++)
            {
                UI.setColor(new Color( pixels[x][y][0],
                                       pixels[x][y][1],
                                       pixels[x][y][2]  )); 
            }
        }
    }

    public static int[][][] Copy(int width, int height, int[][][] InputImage)
    {
        return Copy(width,height,InputImage,0,0);
    }

    public static int[][][] Copy(int width, int height, int[][][] InputImage , int xoffset, int yoffset)
    {
        if (InputImage.length + xoffset > width)
            UI.println("Warning! Copying an image that will not fully fit on the destination image");
        if (InputImage[0].length + yoffset > height)
            UI.println("Warning! Copying an image that will not fully fit on the destination image");

        int[][][] output = new int[width][height][3];

        for (int x = 0; x < InputImage.length; x++)
        {
            for (int y = 0; y < InputImage[x].length; y++)
            {
              output[x+xoffset][y+yoffset] = Arrays.copyOf(InputImage[x][y] , InputImage[x][y].length ); 
            }       
        }
        return output;
    }
}
