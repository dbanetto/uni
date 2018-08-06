import java.io.*;
import java.util.*;
import java.awt.Point;
import ecs100.*;
import java.lang.*;

public class Load
{

    public static void Load(String saveFile, ScrabbleSolitaire scabble)
    {
        try {
            BufferedReader fs = new BufferedReader( new FileReader(saveFile));

            String line = "";
            while ( ( line  = fs.readLine() ) != null )
            {
                if (line.split(":").length != 2){
                    UI.println("Invaild line " + line);
                    continue;
                }
                String command = line.split(":")[0];
                String para = line.split(":")[1];
                switch (command)
                {
                    case("commit"):
                    String[] commits = para.split(";");
                    for (String tile : commits)
                    {
                        String[] items = tile.split(",");
                        if (items.length == 3)
                        {
                            //0 - x pois
                            //1 - y pos
                            //2 - name of tile
                            Tile t = scabble.getRack().pickup(items[2]);
                            int x = Integer.parseInt(items[0]);
                            int y = Integer.parseInt(items[1]);
                            scabble.getBoard().place(t, x, y);
                        }
                    }
                    scabble.commit();
                    break;
                    case ("seed"):
                    int seed = Integer.parseInt(para);
                    scabble.restart( seed );
                    break;
                }

                UI.println(line);
            }
        } catch(IOException ex)
        {
            UI.println(ex.toString());
        }

    }

    public static void PlayBack(String saveFile, ScrabbleSolitaire scabble , double delay)
    {
        try {
            BufferedReader fs = new BufferedReader( new FileReader(saveFile));

            String line = "";

            Board b = scabble.getBoard();
            Rack r = scabble.getRack();
            Bag bag = scabble.getBag();
            while ( ( line  = fs.readLine() ) != null )
            {
                if (line.split(":").length != 2){
                    UI.println("Invaild line " + line);
                    continue;
                }
                String command = line.split(":")[0];
                String para = line.split(":")[1];
                switch (command)
                {
                    case("commit"):
                    String[] commits = para.split(";");
                    for (String tile : commits)
                    {
                        String[] items = tile.split(",");
                        if (items.length == 3)
                        {
                            //0 - x pois
                            //1 - y pos
                            //2 - name of tile
                            Tile t = scabble.getRack().pickup(items[2]);
                            int x = Integer.parseInt(items[0]);
                            int y = Integer.parseInt(items[1]);
                            scabble.getBoard().place(t, x, y);
                        }
                    }
                    UI.sleep(1000);
                    scabble.commit();
                    break;
                    case ("seed"):
                    int seed = Integer.parseInt(para);
                    scabble.restart( seed );
                    break;
                }

                UI.println(line);
            }
            UI.sleep(delay);
            
            scabble.setBoard(b);
            scabble.setRack(r);
            scabble.setBag(bag);

        } catch(IOException ex)
        {
            UI.println(ex.toString());
        }
    }
}
