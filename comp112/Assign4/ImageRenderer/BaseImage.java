import ecs100.*;
import java.util.*;
import java.io.*;
import java.awt.Color;

public abstract class BaseImage
{
    public static final int pixelSize = 2;

    /**
     * An example of a method header - replace this comment with your own
     * 
     * @param  y    a sample parameter for a method
     * @return        the result produced by sampleMethod 
     */
    Color[][] pixels = null;
    int width = 0;
    int height = 0; 
    boolean binary = false;
    boolean loaded = false;
    int bitdepth = 0;

    public BaseImage(int Width , int Height , int Bitdepth , boolean Binary)
    {
        this.width = Width;
        this.height = Height;
        this.binary = Binary;
        this.bitdepth = Bitdepth;
        this.pixels = new Color[Width][Height];
    }

    public Boolean Load(String Data)
    {
        return false;
    }

    public void Draw(int x, int y)
    {
        assert (loaded) : "Trying to draw before the image is loaded";
        for (int row = 0; row < width; row++)
        {
            for (int col = 0; col < height; col++)
            {
                UI.setColor(pixels[row][col]);
                if (pixels[row][col] == Color.white)
                    continue;
                UI.fillRect(x + row*pixelSize, y + col*pixelSize,
                    pixelSize, pixelSize );
            }
        }
    }
    
    void setPixel(int x, int y , Color col)
    {
        assert (col != null) : "Pixel set to null colour";
        //if (x > 0 && x < width && y > 0 && y < height)
        this.pixels[x][y] = col;
    }
}
