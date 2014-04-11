import java.awt.Color;
import ecs100.*;
import java.util.*;
import java.io.*;
import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;

public class Image
{
    private float[][][] pixels = new float[1][1][3];
    private int width = -1;
    private int height = -1;

    public Image(int width, int height , Color color)
    {
        assert height > 0 : "Height has to be greater than 0";
        assert width > 0 : "Width has to be greater than 0";

        this.width = width;
        this.height = height;

        for (int x = 0; x < pixels.length; x++)
        {
            for (int y = 0; y < pixels[x].length; y++)
            {
                int pixelARGB = color.getRGB();

                //Load the values as floats
                this.pixels[x][y][0] = ((pixelARGB >> 16))/255f;
                this.pixels[x][y][1] = ((pixelARGB >>  8) & 255)/255f;
                this.pixels[x][y][2] = (pixelARGB        & 255 )/255f;
            }
        }
    }

    public Image(int width, int height , float[][][] pixels)
    {
        assert height > 0 : "Height has to be greater than 0";
        assert width > 0 : "Width has to be greater than 0";

        this.width = width;
        this.height = height;

        this.pixels = pixels;
    }

    public Image(String filename)
    {
        try {
            BufferedImage image = ImageIO.read(new File(filename));

            this.width = image.getWidth();
            this.height = image.getHeight();

            this.pixels = new float[width][height][3];

            for (int x = 0; x < pixels.length; x++)
            {
                for (int y = 0; y < pixels[x].length; y++)
                {
                    this.pixels[x][y] = PixelIntToFloat(image.getRGB(x,y));
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
                UI.drawRect(x + xpos, y + ypos, 1, 1);
            }
        }
    }

    /*======================= Getters and Setters ==========================*/
    public int getHeight()
    {
        return this.height;
    }

    public int getWidth()
    {
        return this.width;
    }

    public float[][][] getPixels()
    {
        return this.pixels;
    }

    public float[] getPixel(int x , int y)
    {
        if (x > 0 && x < this.width && y > 0 && y < this.height)
        {
            return this.pixels[x][y];
        } else {
            return null;
        }
    }

    public void setPixel(int x , int y , float[] rgb)
    {
        if (x > 0 && x < this.width && y > 0 && y < this.height)
        {
            this.pixels[x][y] = Arrays.copyOf(rgb , rgb.length);
        }
    }

    /* ============== Static Functions =================*/
    public static float[] PixelIntToFloat(int pixelARGB)
    {
        float[] out = new float[3];
        out[0] = ((pixelARGB >> 16) & 255)/255f;
        out[1] = ((pixelARGB >>  8) & 255)/255f;
        out[2] = (pixelARGB        & 255 )/255f;
        return out;
    }

    public static float[][][] Copy(int width, int height, float[][][] InputImage)
    {
        return Copy(width,height,InputImage,0,0);
    }

    public static float[][][] Copy(int width, int height, float[][][] InputImage , int xoffset, int yoffset)
    {
        if (InputImage.length + xoffset > width)
            UI.println("Warning! Copying an image that will not fully fit on the destination image");
        if (InputImage[0].length + yoffset > height)
            UI.println("Warning! Copying an image that will not fully fit on the destination image");

        float[][][] output = new float[width][height][3];

        for (int x = 0; x < InputImage.length; x++)
        {
            for (int y = 0; y < InputImage[x].length; y++)
            {
                output[x+xoffset][y+yoffset] = Arrays.copyOf(InputImage[x][y] , InputImage[x][y].length ); 
            }       
        }
        return output;
    }

    /*===================== Image Minplulation ============================*/

    /*=============== Brightness ======================= */
    public static Image applyBrightness(Image in, double perc)
    {
        return applyBrightness(in, perc, 0,0 , in.getWidth() , in.getHeight() );
    }

    public static Image applyBrightness(Image in, double perc , int xoffset, int yoffset, int width , int height)
    {
        float[][][] pixels = Image.Copy(width , height,in.getPixels() , xoffset , yoffset);

        for (int x = 0; x < width; x++)
        {
            for (int y = 0; y < height; y++)
            {
                float[] hsb = Color.RGBtoHSB((int)(pixels[x][y][0]*255) , 
                        (int)(pixels[x][y][1]*255) ,
                        (int)(pixels[x][y][2]*255), null );

                float brightness = hsb[2];
                
                brightness = (float)Math.pow( (double)(brightness) , (double)(perc));
                
                brightness = Math.max (Math.min(brightness , 0.999f) , 0.001f);
                
                int brightpixel = Color.HSBtoRGB(hsb[0] , hsb[1] , brightness );

                pixels[x][y] = Image.PixelIntToFloat(brightpixel);
            }
        }

        Image out = new Image(width , height , pixels );
        return out;
    }

    /*=============== Blur =====================*/
    public static Image applyBlur(Image in)
    {
        return applyBlur(in, 0,0 , in.getWidth() , in.getHeight() );
    }

    public static Image applyBlur(Image in, int xoffset, int yoffset, int width , int height)
    {
        float[][][] pixels = Image.Copy(width , height,in.getPixels() , xoffset , yoffset);

        for (int x = 0; x < width; x++)
        {
            for (int y = 0; y < height; y++)
            {
                float[][] blur3x3 =  { 
                        {0.05f , 0.15f , 0.05f},
                        {0.15f , 0.2f ,  0.15f},
                        {0.05f , 0.15f , 0.05f}
                    }; 

                float[] outpixel = new float[3];

                for (int n = x-1; n < width && n <= x+1; n++)
                {

                    for (int j = y-1; j <= y+1; j++)
                    {
                        int nn = n;
                        int jj = j;
                        if (n < 0)
                            nn = x;
                        if (n >= width)
                            nn =x;
                        if (j < 0)
                            jj = y;
                        if (j >= height)
                            jj = y;

                        outpixel[0] += pixels[nn][jj][0]*blur3x3[n-x+1][j-y+1];
                        outpixel[1] += pixels[nn][jj][1]*blur3x3[n-x+1][j-y+1];
                        outpixel[2] += pixels[nn][jj][2]*blur3x3[n-x+1][j-y+1];

                    }
                }

                outpixel[0] = Math.min(Math.max(outpixel[0] , 0f),1f);
                outpixel[1] = Math.min(Math.max(outpixel[1] , 0f),1f);
                outpixel[2] = Math.min(Math.max(outpixel[2] , 0f),1f);

                pixels[x][y] = outpixel;
            }
        }

        Image out = new Image(width , height , pixels );
        return out;
    }
}
