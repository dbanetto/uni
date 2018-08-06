import java.lang.*;
import java.util.*;
import java.awt.Color;
import ecs100.*;
/**
 * Write a description of class PBMImage here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class PBMImage extends BaseImage
{
    public PBMImage(int Width , int Height , int Bitdepth , boolean Binary)
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
                    Color out;
                    if (numb == 0)
                    {
                        out = Color.white;
                    } else {
                        out = Color.black;
                    }

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
