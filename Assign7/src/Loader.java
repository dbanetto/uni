import java.awt.Color;
import java.io.*;
import java.util.*;

public class Loader {
	
	public static void Load ( String file ,  DiagramUI ui  )
	{
		String balck = ColortoHex(Color.black);
		
		try {
			List<Line> lines = ui.getLines();
			List<IShape> shapes = ui.getShapes();
			
			BufferedReader fs = new BufferedReader( new FileReader(file));
			String line = "";
            while ( ( line  = fs.readLine() ) != null )
            {
            	if (line.split(":").length != 2){
                    System.out.println("Invaild line " + line);
                    continue;
                }
                String command = line.split(":")[0];
                String[] para = line.split(":")[1].split(",");
                
                if (command.equals("shape"))
                {
                	//Example 0,0,100,100,0,rect,Hello World,#ffff00,#ff00ff,#00ffff
                	//Layout x,y,w,h,ID,type,text,Colour fill , Colour Border, Colour Text
                	int x = 0, y = 0, w= 0, h= 0, id = 0;
                	String type = "", txt = "";
                	Color fill , border , text;
                	
                	x = Integer.parseInt(para[0]);
                	y = Integer.parseInt(para[1]);
                	w = Integer.parseInt(para[2]);
                	h = Integer.parseInt(para[3]);
                	id = Integer.parseInt(para[4]);
                	
                	type = para[5];
                	txt = para[6];
                	
                	fill = Color.decode(para[7]);
                	border = Color.decode(para[8]);
                	text = Color.decode(para[9]);
                	
                	switch (type)
                	{
                		case("rect"):
                			Rectangle rect = new Rectangle(id, x, y, w, h, border, fill);
                			rect.getText().setColor(text);
                			rect.setText(txt);
                			shapes.add(rect);
                			break;
                		case("oval"):
                			Oval oval = new Oval(id, x, y, w, h, border, fill);
                			oval.getText().setColor(text);
                			oval.setText(txt);
	            			shapes.add(oval);
                			break;
                	}
                	
                } else if (command.equals("line"))
                {
                	//Example 0,0,#ffffff
                	//Layout ID_A,ID_B,Line Colour
                	//Note : Requires for ID's to exist before hand, thus lines are written to file last
                	int idA , idB;
                	Color colour;
                	
                	IShape A = null;
                	IShape B = null;
                	
                	idA = Integer.parseInt(para[0]);
                	idB = Integer.parseInt(para[1]);
                	
                	colour = Color.decode(para[2]);
                	
                	for ( IShape i : shapes )
                	{
                		if ( idA == i.getID() )
                			A = i;
                		else if ( idB == i.getID() )
                			B = i;
                	}
                	
                	lines.add(new Line( A , B , colour ));
                	
                }
            }
            fs.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public static void Save ( String file , DiagramUI ui )
	{
		try {
			List<Line> lines = ui.getLines();
			List<IShape> shapes = ui.getShapes();
			
			BufferedWriter fs = new BufferedWriter( new FileWriter(file));
			for (IShape s : shapes)
			{
				int x = 0, y = 0, w= 0, h= 0, id = 0;
            	String type = "", txt = "";
            	String fill , border , text; //Colours
            	
            	x = (int)s.getPosition().getX();
            	y = (int)s.getPosition().getY();
            	w = s.getWidth();
            	h = s.getHeight();
            	
            	id = s.getID();
            	
            	txt = s.getText().getText();
            	
            	fill = ColortoHex (s.getFill() );
            	border = ColortoHex (s.getBorder() );
            	text = ColortoHex (s.getText().getColour() );
            	
            	if ( s instanceof Rectangle)
            	{
            		type = "rect";
            	} else if ( s instanceof Oval)
            	{
            		type = "oval";
            	}
            	fs.write("shape:");
            	fs.write( x + "," + y + "," + w + "," + h + "," + id + "," + type + "," + text + "," + fill + "," + border + "," + text );
            	fs.write("\n");
			}
			fs.write("\n");
			
			for (Line l : lines)
			{
				int idA , idB;
				idA = l.getShapes()[0].getID();
				idB = l.getShapes()[1].getID();
				
				String line = ColortoHex(l.getColour());
				
				fs.write("line:");
            	fs.write( idA + "," + idB + "," + line );
            	fs.write("\n");
			}
			
			
			fs.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}catch (IOException e) {
			e.printStackTrace();
		} 
	} 
	
	//Convert awt.Color's to HTML styled hex colours ( which are nicely converted by Color.decode() )
	public static String ColortoHex (Color in)
	{
		String r = Integer.toHexString( in.getRed() );
		String g = Integer.toHexString( in.getGreen() );
		String b = Integer.toHexString( in.getBlue() );
		//Make sure each colour bit takes up two chars
		return "#" + (r.length() == 1 ? "0"+ r : r ) + (g.length() == 1 ? "0"+ g : g ) + (b.length() == 1 ? "0"+ b : b );
	}
}
