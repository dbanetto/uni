import java.lang.*;
import java.util.*;
import java.awt.Color;
import ecs100.*;
/**
 * Write a description of class PGMImage here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class PGMImage extends BaseImage
{
    public PGMImage(int Width , int Height , int Bitdepth , boolean Binary)
    {
        super(Width,Height,Bitdepth,Binary);

    }

    public Boolean Load(String Data)
    {
        try {
            String[] pixels = Data.split(" ");
            for (int y = 0; y < height; y++)
            {
                for (int x = 0; x < width; x++)
                {
                    int numb = Integer.parseInt(pixels[x + y*width]);
                    int gray = 0;
                    if (bitdepth != 255) {
                        float grayr = 255.0f * ( (float)numb / (float)bitdepth );
                        gray = Math.round( grayr );
                    } else {
                        gray = numb;
                    }
                    Color out = new Color( gray , gray , gray );

                    this.setPixel(x, y, out);
                }
            }
        } catch (Exception ex)
        {
            UI.println(ex.toString());
            return false;
        }
        this.loaded = true;
        return true;
    }
}
