import java.io.*;
import java.util.*;
import java.awt.Point;
import ecs100.*;

public class Logger
{
    private FileOutputStream fs;

    public Logger(String filename , int bagSeed)
    {
        try {
            fs= new FileOutputStream(filename);
        } catch (Exception ex)
        {
            UI.println(ex.toString());
        }
    }
    
    public void Seed(int seed)
    {
        try {
            fs.write( ("seed:" + seed + "\n").getBytes() );
        }catch (Exception ex)
        {
            UI.println(ex.toString());
        }
    }
    
    public void Log( Map<Point,Tile> tiles )
    {
        String logEntry = "commit:";
        for (Point pt :  tiles.keySet())
        {
            logEntry += (int)pt.getX()+","+(int)pt.getY()+","+tiles.get(pt).getLetter()+";";
        }
        try {
            fs.write( (logEntry + "\n").getBytes() );
        }catch (Exception ex)
        {
            UI.println(ex.toString());
        }
    }
}
