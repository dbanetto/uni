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
public class PPMImage extends BaseImage
{
    public PPMImage(int Width , int Height , int Bitdepth , boolean Binary)
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
                    int r = Integer.parseInt(pixels[x*3 + y*width*3]);
                    int g = Integer.parseInt(pixels[x*3 + y*width*3 + 1]);
                    int b = Integer.parseInt(pixels[x*3 + y*width*3 + 2]);
                    
                    int gray = 0;
                    if (bitdepth != 255) {
                        float scaler = 255.0f * ( (float)r / (float)bitdepth );
                        r = Math.round( scaler );
                        
                        float scaleg = 255.0f * ( (float)g / (float)bitdepth );
                        g = Math.round( scaleg );
                        
                        float scaleb = 255.0f * ( (float)b / (float)bitdepth );
                        b = Math.round( scaleb );
                    }
                    Color out = new Color( r , g , b );

                    this.setPixel(x, y, out);
                    UI.println(x+ ","  + y + " = " + r + "," + g  + ","+ b );
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
