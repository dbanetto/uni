import java.awt.Color;
import ecs100.*;
import java.util.*;
import java.io.*;
import javax.imageio.*;
import javax.imageio.stream.*;
import java.awt.image.*;
import java.awt.Point;
import java.lang.*;

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
                this.pixels[x][y][0] = ((pixelARGB >> 16) & 255)/255f;
                this.pixels[x][y][1] = ((pixelARGB >>  8) & 255)/255f;
                this.pixels[x][y][2] = ( pixelARGB        & 255)/255f;
            }
        }
    }

    public Image(int width, int height , float[][][] pixels)
    {
        assert height > 0 : "Height has to be greater than 0";
        assert width > 0 : "Width has to be greater than 0";

        assert width == pixels.length : "Width is not the same as width of pixels";
        assert height == pixels[0].length : "Height is not the same as height of pixels";

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

    public void Save(String fname)
    {
        try {
            BufferedImage image = new BufferedImage(this.width , this.height , BufferedImage.TYPE_INT_ARGB );
            for (int x = 0; x < pixels.length; x++)
            {
                for (int y = 0; y < pixels[x].length; y++)
                {
                    image.setRGB(  x , y , Image.FloatToIntARGB(this.pixels[x][y]));
                }
            }
            
            String extension = "";
            int i = fname.lastIndexOf('.');
            if (i > 0) { 
                extension = fname.substring(i+1);
            } 
            
            ImageIO.write(image, "png" , new File(fname+".png"));
        } catch (IOException ex)
        {
            UI.println(ex.toString());
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

    public static int FloatToIntARGB(float[] pixel)
    {
        int out = (int)(pixel[2]*255);
        out +=    (int)(pixel[1]*255) << 8;
        out +=    (int)(pixel[0]*255) << 16;
        out +=    255 << 24; //Alpha Channel
        return out;
    }
    
    public static Image Copy (Image in)
    {
        return new Image ( in.getWidth() , in.getHeight() , Image.Copy(in.getWidth() , in.getHeight() , in.getPixels()));
    }
    
    public static float[][][] Copy(int width, int height, float[][][] InputImage)
    {
        return Copy(width,height,InputImage,0,0);
    }

    public static float[][][] Copy(int width, int height, float[][][] InputImage , int xoffset, int yoffset)
    {
        if (InputImage.length < width + xoffset)
            UI.println("Warning! Image being copied to will not be fully copied. target width and x offset greater than input image width");
        if (InputImage[0].length < height + yoffset)
            UI.println("Warning! Image being copied to will not be fully copied. target height and y offset greater than input image height");

        float[][][] output = new float[width][height][3];

        for (int x = xoffset; x-xoffset < width && x < InputImage.length; x++)
        {
            for (int y = yoffset; y-yoffset < height && y < InputImage[x].length; y++)
            {
                output[x-xoffset][y-yoffset] = Arrays.copyOf(InputImage[x][y] , InputImage[x][y].length ); 
            }       
        }
        return output;
    }
    
    public static boolean PixelEquals (float[] pixelA , float[] pixelB , float tollerance)
    {
        assert pixelA.length == 3 : "Invalid number of floats in pixel A (" + pixelA.length + ")";
        assert pixelB.length == 3 : "Invalid number of floats in pixel B (" + pixelB.length + ")";
        for (int i = 0; i < pixelA.length; i++)
        {
            float diff = Math.abs( pixelA[i] - pixelB[i]);
            if ( diff > tollerance)
            {
                return false;
            }
        }
        return true;
    }
    
    public static boolean PixelEquals (float[] pixelA , float[] pixelB)
    {
        assert pixelA.length == 3 : "Invalid number of floats in pixel A (" + pixelA.length + ")";
        assert pixelB.length == 3 : "Invalid number of floats in pixel B (" + pixelB.length + ")";
        for (int i = 0; i < pixelA.length; i++)
        {
            if ( Float.compare( pixelA[i], pixelB[i]) != 0 )
            {
                return false;
            }
        }
        return true;
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

                brightness = (float)Math.pow( (double)(brightness) , (double)(perc) );
                
                brightness = Math.max (Math.min(brightness , 1f) , 0f);

                int brightpixel = Color.HSBtoRGB(hsb[0] , hsb[1] , brightness );

                pixels[x][y] = Image.PixelIntToFloat(brightpixel);
            }
        }

        Image out = new Image(width , height , pixels );
        return out;
    }

    /*=============== Blur =====================*/
    public static Image applyBlur3x3(Image in)
    {
        return applyFilter(in, 0,0 , in.getWidth() , in.getHeight() ,
            new float[][] {{0.1f, 0.2f, 0.1f},
                {0.2f, 0.4f, 0.2f},
                {0.1f, 0.2f, 0.1f}} );
    }

    public static Image applyFilter(Image in, int xoffset, int yoffset, int width , int height , float[][] blurMatrix)
    {
        float[][][] pixels = Image.Copy( width , height , in.getPixels() , xoffset , yoffset );
        float[][][] outpixels = new float[width][height][3];

        for (int x = 0; x < width; x++)
        {
            for (int y = 0; y < height; y++)
            {

                float[] outpixel = new float[3];
                double prec_applied = 0;
                int noffset = blurMatrix.length/2;
                int blury = 0;
                for (int n = x-noffset; n < width && n <= x+noffset; n++)
                {
                    int joffset = blurMatrix[0].length/2;
                    int blurx = 0;
                    
                    if (blury >= blurMatrix[0].length)
                            break;
                    
                    for (int j = y-(joffset); j <= y+(joffset); j++)
                    {
                        int nn = n;
                        int jj = j;
                        if (n < 0)
                            nn = 0;
                        if (n >= width)
                            nn = width-1;
                        if (j < 0)
                            jj = 0;
                        if (j >= height)
                            jj = height-1;
                        
                        if (blurx >= blurMatrix.length)
                            break;
                        
                        outpixel[0] += pixels[nn][jj][0]*blurMatrix[blurx][blury];
                        outpixel[1] += pixels[nn][jj][1]*blurMatrix[blurx][blury];
                        outpixel[2] += pixels[nn][jj][2]*blurMatrix[blurx][blury];

                        prec_applied += blurMatrix[n-x+noffset][j-y+joffset];
                        blurx++;
                    }
                    blury++;
                }

                if (prec_applied != 1.0)
                {
                    double diff = 1.0 / prec_applied;

                    outpixel[0] *= diff;
                    outpixel[1] *= diff;
                    outpixel[2] *= diff;

                }

                outpixel[0] = Math.min(Math.max(outpixel[0] , 0f),1f);
                outpixel[1] = Math.min(Math.max(outpixel[1] , 0f),1f);
                outpixel[2] = Math.min(Math.max(outpixel[2] , 0f),1f);

                outpixels[x][y] = Arrays.copyOf(outpixel , 3);
            }
        }

        Image out = new Image(width , height , outpixels );
        return out;
    }

    /* ============================ Horizontal flip ========================================== */
    public static Image applyHorizontalFlip(Image in)
    {
        int width  = in.getWidth();
        int height  = in.getHeight();

        float[][][] inpixels = in.getPixels();
        float[][][] outpixels = new float[width][height][3];

        for (int x = 0; x < width; x++)
        {
            for (int y = 0; y < height; y++)
            {
                int horzX =  (width-1) - x;
                outpixels[x][y] =  Arrays.copyOf( inpixels[horzX][y] , 3);
            }
        }

        Image out = new Image(width , height , outpixels );
        return out;
    }

    /* ============================ 90 deg flip ========================================== */
    public static Image apply90degFlip(Image in)
    {
        int width  = in.getWidth();
        int height  = in.getHeight();

        float[][][] inpixels = in.getPixels();
        float[][][] outpixels = new float[height][width][3];

        for (int x = 0; x < width; x++)
        {
            for (int y = 0; y < height; y++)
            {
                outpixels[y][x] =  Arrays.copyOf( inpixels[x][y] , 3);
            }
        }

        Image out = new Image(height , width , outpixels );
        return out;
    }

    /* ============================ Rotate ========================================== */
    //(x,y)cor = Center of Rotation
    public static Image applyRotate(Image in , int xcor ,int ycor , int angle)
    {
        int width  = in.getWidth();
        int height  = in.getHeight();

        float[][][] inpixels = in.getPixels();
        float[][][] outpixels = new float[width][height][3];
        double anglerads = angle/180.0 * Math.PI;

        for (int x = 0; x < width; x++)
        {
            for (int y = 0; y < height; y++)
            {
                int oldx = (int)( Math.cos(anglerads)*(double)(x-xcor) - Math.sin(anglerads)*(double)(y-ycor) ) + xcor;
                int oldy = (int)( Math.sin(anglerads)*(double)(x-xcor) + Math.cos(anglerads)*(double)(y-ycor) ) + ycor;
                
                if (oldx > 0 && oldx < width && oldy > 0 && oldy < height) {
                    outpixels[x][y] =  Arrays.copyOf( inpixels[oldx][oldy] , 3);
                } else {
                    outpixels[x][y] = new float[] {1.0f,1.0f,1.0f};
                }
            }
        }

        Image out = new Image(width , height , outpixels );
        return out;
    }

    /*======================== Merge ================================*/
    public static Image applyMerge(Image base , Image merge , double prec)
    {
        int width  = base.getWidth();
        int height  = base.getHeight();

        float[][][] inmerge = merge.getPixels();
        float[][][] inbase  = base.getPixels();
        float[][][] outpixels = new float[width][height][3];

        for (int x = 0; x < width; x++)
        {
            for (int y = 0; y < height; y++)
            {
                if (x >= merge.getWidth() || y >= merge.getHeight())
                {
                    outpixels[x][y] = Arrays.copyOf( inbase[x][y] , 3);
                } else {
                    outpixels[x][y][0] = inbase[x][y][0]*(1.0f-(float)prec) + inmerge[x][y][0]*(float)prec;
                    outpixels[x][y][1] = inbase[x][y][1]*(1.0f-(float)prec) + inmerge[x][y][1]*(float)prec; 
                    outpixels[x][y][2] = inbase[x][y][2]*(1.0f-(float)prec) + inmerge[x][y][2]*(float)prec;
                }
            }
        }

        Image out = new Image(width , height , outpixels );
        return out;
    }

    /*========================= Zoom =====================================*/
    public static Image applyZoom(Image base , double prec )
    {
        return applyZoom(base , prec , prec , 0 ,0 , (int)(base.getWidth()*prec) , (int)(base.getHeight()*prec) );
    }
    
    public static Image applyZoom(Image base , double precx , double precy , int xoffet , int yoffset , int width , int height)
    {
        int oldwidth   = base.getWidth();
        int oldheight  = base.getHeight();

        float[][][] inpixels = base.getPixels();
        float[][][] outpixels = new float[width][height][3];

        for (int x = 0; x < width; x++)
        {
            for (int y = 0; y < height; y++)
            {
                int ex = (int)(x/precx) + xoffet;
                int ey = (int)(y/precy) + yoffset;

                if (ex >= oldwidth)
                {
                    ex = oldwidth-1;
                }
                if (ey >= oldheight)
                {
                    ey = oldheight-1;
                }

                outpixels[x][y] = Arrays.copyOf(inpixels[ex][ey] , 3);
            }
        }

        Image out = new Image(width , height , outpixels );
        return out;
    }

    /*====================== Crop ===========================*/
    public static Image applyCrop(Image base , int x, int y , int w, int h)
    {

        Image out = new Image(w , h , Image.Copy(w, h, base.getPixels(), x, y) );
        return out;
    }
    
    /*================= Pour ===================*/
    public static Image applyPour(Image base, int x , int y , Color ncolour , float tolerance)
    {
        
        
        int width = base.getWidth();
        int height = base.getHeight();
        
        float[][][] pixels = Image.Copy( width , height, base.getPixels() , 0 , 0);
        
        //Points to fill
        List<Point> points = new ArrayList<Point>();
        points.add(new Point(x, y));
        
        final float[] oldcolour = Arrays.copyOf( pixels[x][y] , 3 );
        final float[] newcolour = Image.PixelIntToFloat(ncolour.getRGB());
        
        if ( Arrays.equals(oldcolour , newcolour) )
        {
            UI.println("Cannot Pour onto the same Colour");
            return new Image(width , height , pixels );
        }
        int itrs = 0;
        while (points.size() > 0)
        {
            Point pt = points.remove(0);
            int ptx = (int)pt.getX();
            int pty = (int)pt.getY();
            
            //Set Pxiel
            pixels[ptx][pty] = Arrays.copyOf( newcolour , 3 );
            
            //Check Others if tehy should be changed
            if (ptx+1 < width)
            {
                if ( PixelEquals( pixels[ptx+1][pty] , oldcolour , tolerance ) && !points.contains(new Point(ptx+1, pty)) )
                {
                    points.add(new Point(ptx+1, pty));
                }
            }
            if (ptx-1 >= 0)
            {
                if ( PixelEquals( pixels[ptx-1][pty] , oldcolour , tolerance ) && !points.contains(new Point(ptx-1, pty)) )
                {
                    points.add(new Point(ptx-1, pty));
                }
            }
            
            if (pty+1 < height)
            {
                if ( PixelEquals( pixels[ptx][pty+1] , oldcolour , tolerance ) && !points.contains(new Point(ptx, pty+1)) )
                {
                    points.add(new Point(ptx, pty+1));
                }
            }
            if (pty-1 >= 0)
            {
                if ( PixelEquals( pixels[ptx][pty-1] , oldcolour , tolerance ) && !points.contains(new Point(ptx, pty-1)) )
                {
                    points.add(new Point(ptx, pty-1));
                }
            }
        }
        Image out = new Image(width , height , pixels );
        return out;
    }
    
    /*========================= Brush Filter ============================================*/
    public static Image applyBrushFilter(Image in, int xoffset, int yoffset, int brushsize , float[][] blurMatrix)
    {
        int width = in.getWidth();
        int height = in.getHeight();
        float[][][] pixels = Image.Copy( width , height ,  in.getPixels() );
        float[][][] outpixels = Image.Copy( width , height ,  in.getPixels() );
        
        int radius = brushsize/2;
        
        for (int x = xoffset - radius; x < width && x < xoffset+(radius-1); x++)
        {
            if (x < 0)
                continue;
            for (int y = yoffset - radius; y < height && y < yoffset+(radius-1); y++)
            {
                if (y < 0)
                    continue;
                    
                double distance = Math.sqrt(  Math.pow((double)(xoffset-x) , 2 ) + Math.pow((double)(yoffset-y) , 2 ) );
                if (distance > radius)
                    continue;
                
                float[] outpixel = new float[3];
                double prec_applied = 0;
                int noffset = blurMatrix.length/2;
                int blury = 0;
                for (int n = x-noffset; n < width && n <= x+noffset; n++)
                {
                    int joffset = blurMatrix[0].length/2;
                    int blurx = 0;
                    
                    if (blury >= blurMatrix[0].length)
                            break;
                    
                    for (int j = y-(joffset); j <= y+(joffset); j++)
                    {
                        int nn = n;
                        int jj = j;
                        if (n < 0)
                            nn = 0;
                        if (n >= width)
                            nn = width-1;
                        if (j < 0)
                            jj = 0;
                        if (j >= height)
                            jj = height-1;
                        
                        if (blurx >= blurMatrix.length)
                            break;
                        
                        outpixel[0] += pixels[nn][jj][0]*blurMatrix[blurx][blury];
                        outpixel[1] += pixels[nn][jj][1]*blurMatrix[blurx][blury];
                        outpixel[2] += pixels[nn][jj][2]*blurMatrix[blurx][blury];

                        prec_applied += blurMatrix[n-x+noffset][j-y+joffset];
                        blurx++;
                    }
                    blury++;
                }

                if (prec_applied != 1.0)
                {
                    double diff = 1.0 / prec_applied;

                    outpixel[0] *= diff;
                    outpixel[1] *= diff;
                    outpixel[2] *= diff;

                }

                outpixel[0] = Math.min(Math.max(outpixel[0] , 0f),1f);
                outpixel[1] = Math.min(Math.max(outpixel[1] , 0f),1f);
                outpixel[2] = Math.min(Math.max(outpixel[2] , 0f),1f);

                outpixels[x][y] = Arrays.copyOf(outpixel , 3);
            }
        }

        Image out = new Image(width , height , outpixels );
        return out;
    }
}
