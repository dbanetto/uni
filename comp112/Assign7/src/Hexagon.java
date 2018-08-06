import java.awt.Color;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import ecs100.UI;


public class Hexagon extends Shape {
	
	Point vertexs[] = new Point[6];
	
	public Hexagon(int ID, int x, int y, int w, int h, Color border, Color fill) {
		super(ID, x, y, w, h, border, fill);
		generateVertex();
	}

	@Override
	public void draw(int x_camera, int y_camera) {
		
		double vert_x[] = new double[6];
		double vert_y[] = new double[6];
		
		for (int i = 0; i < vertexs.length; i++)
		{
			//Point between each points
			//If the next point is outside of the range of the array, go to the 0th element (enclose the shape)
			vert_x[i] = vertexs[i].getX() - x_camera + this.getPosition().getX();
			vert_y[i] =	vertexs[i].getY() - y_camera + this.getPosition().getY();
			
		}
		UI.setColor(this.fill);
		UI.fillPolygon(vert_x, vert_y, 6);
		
		UI.setColor(this.border);
		UI.drawPolygon(vert_x, vert_y, 6);
		
		this.text.draw(x_camera, y_camera);
	}

	@Override
	public boolean select(int x, int y) {
		if ( x > this.x && x < this.x + this.w && 
			 y > this.y && y < this.y + this.h	)
		{
			return IsPointInsidePolygon(new Point(x - this.getPosition().x ,y  - this.getPosition().y) , this.vertexs );
		}
		return false;
	}
	
	@Override
	public void setWidth (int w)
	{
		super.setWidth(w);
		generateVertex();
	}
	
	@Override
	public void setHeight (int h)
	{
		super.setHeight(h);
		generateVertex();
	}
	
	public void generateVertex()
	{
		//Hmmm Java 7
		Map< String , ArrayList<Integer> > map = new HashMap< >();
		map = new HashMap<>(); //This is fine
		map.put( "Toyota" , new ArrayList<Integer>() );
		
		Point genVertex[] = new Point[6];
		
		int short_side = h , long_side = w;
		boolean width_long_side = true;
		if (this.w < this.h)
		{
			short_side = w;
			long_side = h;
			width_long_side = false;
		}
		
		// To find the amount the triangle digs into the other side
		// Assuming the triangle is always at 45 deg to the other side
		// And the length of the short side is 25% (or 0.25) of the length
		// of the short side, the amount the side diggs in should be const*length
		// 0.25*tan 45 (deg) = 0.25 (const value)
		
		double long_dig_in = 0.25 * short_side;
		
		genVertex[0] = new Point( 0 , (int)(short_side * 0.5) );
		genVertex[1] = new Point( (int)long_dig_in , short_side );
		genVertex[2] = new Point( (int)(long_side - long_dig_in) , short_side );
		genVertex[3] = new Point( long_side , (int)(short_side * 0.5) );
		genVertex[4] = new Point( (int)(long_side - long_dig_in) , 0 );
		genVertex[5] = new Point( (int)long_dig_in , 0 );
		
		if ( width_long_side == false )
		{
			for ( Point p :  genVertex)
			{
				p.setLocation( short_side - p.getY() ,  long_side - p.getX() );
			}
		}
		
		this.vertexs = Arrays.copyOf(genVertex, genVertex.length);
	}
	
	
	public static boolean IsPointInsidePolygon (Point point , Point points[])
	{
	    //Code converted from pseudo code from http://stackoverflow.com/questions/11716268/point-in-polygon-algorithm
	    //Credit for this snippet goes to http://stackoverflow.com/users/1830407/josh
	    int i, j, nvert = points.length;
	    boolean c = false;

	    for(i = 0, j = nvert - 1; i < nvert; j = i++) {
	        if( ( (points[i].y) >= point.y ) != (points[j].y >= point.y)  &&
	            (point.x <= (points[j].x - points[i].x) * (point.y - points[i].y) / (points[j].y - points[i].y) + points[i].x) )
	          c = !c;
	      }

	      return c;
	}
}
