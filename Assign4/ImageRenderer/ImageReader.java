import java.lang.*;
import ecs100.*;
import java.util.*;
import java.io.*;
/**
 * Write a description of class ImageReader here.
 * 
 * @author (your name) 
 * @version (a version number or a date)
 */
public class ImageReader
{
    /**
     * An example of a method - replace this comment with your own
     * 
     * @param  String Filename - Path and File name of the given file
     * @return A List of Image Objects
     */
    public static List<BaseImage> LoadImages (String filename)
    {
        List<BaseImage> images = new ArrayList<BaseImage>();

        try {

            BufferedReader br = new BufferedReader(new FileReader(filename  ));
            String line = "";

            int bitdepth = 0;
            String type = "";
            int width = 0, height = 0;
            int lineNum = 0;
            //Bytes per pixel
            int bytes = 1;
            int width_count = 0;
            String data = "";
            while ( (line = br.readLine()) != null )
            {
                //Remove the comments from the lines
                if (line.equals(""))
                    continue;
                line = line.split("#")[0].trim();
                if (line.equals(""))
                    continue;

                //Get the type of the Image
                if (line.startsWith("P"))
                {
                    type = line;
                    if (type.equals("P3") || type.equals("P6"))
                        bytes = 3;
                    //Reset line number count
                    //lineNum = 0;
                    continue;
                }

                if (lineNum == 0)
                {
                    String[] dimension = line.split(" ");

                    assert (dimension.length == 2 ) : "Invaild Dimension in " + filename + 
                    ". Expected two ints, got " + dimension.length;

                    width = Integer.parseInt(dimension[0]);
                    height = Integer.parseInt(dimension[1]);

                    assert (width > 1) : "Width is less than zero";
                    assert (height > 1) : "Height is less than zero";
                    lineNum++;
                    continue;
                } else if (lineNum == 1)
                {
                    if ( type.equals("P1") == false && type.equals("P4") == false )
                    {   
                        bitdepth = Integer.parseInt(line);
                        assert (bitdepth > 1) : "Bitdepth is less than zero";
                        lineNum++;
                        continue;
                    }
                }

                data += line + " ";
                width_count += line.split(" ").length;
                /*if (width_count > width*height*bytes)
                {
                    int index = nthOccurrence(data, ' ', width*bytes);
                    String out = data.substring(0,index).trim();
                    String keep = data.substring(index, data.length());
                    
                    assert (out.split(" ").length == width*height*bytes) : "Not correct amount of bytes. Given:"
                        +   out.split(" ").length + "(" + width_count + ")" + " Expected : " + width*height*bytes;
                    
                    images.add(LoadImage(type, bitdepth ,
                            width , height , 
                            out)
                    );
                    
                    data = keep;
                    width_count = keep.split(" ").length;
                    UI.println("End of Image load. remaining width:" + width_count );
                } else */if ( width_count == width*height*bytes) {
                    images.add(LoadImage(type, bitdepth ,
                            width , height , 
                            data)
                    );
                    
                    UI.println("End of Image load. remaining width: 0." );
                    data = "";
                    width_count = 0;
                    lineNum = 0;
                    assert (width_count == 0) : "wat";
                }
                
            }
            
            if (data != "")
            {
                images.add(LoadImage(type, bitdepth ,
                            width , height , 
                            data)
                    );
                    data = "";
            }
            
            UI.println("Meta Info");
            UI.println("Type : " + type);
            UI.println("Bitdepth : " + bitdepth);
            UI.println("Bytes per pixel : " + bytes);
            UI.println("Size : " + width + "x" + height);
            UI.println("# of Images : " + images.size());

            //catch Block
        } catch (FileNotFoundException ex)
        {
            UI.println( ex.toString());
        } catch (IOException ex)
        {
            UI.println( ex.toString());
        }
        return images;
    }

    private static BaseImage LoadImage(String type, int bitdepth , int width ,int height , String data)
    {
        boolean binary = false;
        switch (type)
        {
            case ("P4"):
            binary = true;
            case ("P1"):
            //Load from black white
            PBMImage imgB = new PBMImage(width , height, bitdepth , binary);
            if (imgB.Load(data))
                return imgB; 
            break;
            case ("P5"):
            binary = true;
            case ("P2"):
            //Load From grayscale
            PGMImage imgG = new PGMImage(width , height, bitdepth , binary);
            if (imgG.Load(data))
                return imgG; 
            break;

            case ("P6"):
            binary = true;
            case ("P3"):
            PPMImage imgP = new PPMImage(width , height , bitdepth , binary);
            if (imgP.Load(data))
                return imgP;
            //Load from RGB class
            break;
        }
        return null;
    }
    
    // Code from
    // http://stackoverflow.com/questions/3976616/how-to-find-nth-occurrence-of-character-in-a-string
    public static int nthOccurrence(String str, char c, int n) {
        int pos = str.indexOf(c, 0);
        while (n-- > 0 && pos != -1)
            pos = str.indexOf(c, pos+1);
        return pos;
    }
}

