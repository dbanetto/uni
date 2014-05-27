import java.awt.Color;
import java.awt.Point;

import ecs100.*;

public class Line {
	
	private IShape shapes[];
	private Color colour;
	
	public Line(IShape a, IShape b , Color colour )
	{
		shapes = new IShape[] {a , b};
		this.colour = colour;
	}
	
	public boolean draw (int x_cam , int y_cam )
	{
		if (shapes[0].disposed() || shapes[1].disposed())
		{
			return false;
		}
		
		if (!(shapes[0].getVisability() && shapes[1].getVisability()))
		{
			return true;
		}
		
		UI.setColor(this.colour);
		UI.drawLine(shapes[0].getCenter().getX() - x_cam, shapes[0].getCenter().getY() - y_cam,
				shapes[1].getCenter().getX() - x_cam, shapes[1].getCenter().getY() - y_cam );
		return true;
	}
	
	public boolean connectsTo (IShape shape)
	{
		for (IShape s : shapes)
		{
			if (s.equals(shape))
			{
				return true;
			}
		}
		return false;
	}
	
	public Color getColour()
	{
		return this.colour;
	}
	
	public void setColor(Color colour)
	{
		this.colour = colour;
	}
	
	public IShape[] getShapes()
	{
		return this.shapes;
	}
	
	public boolean select ( int x , int y )
	{
		Point a = this.shapes[0].getCenter();
		Point b = this.shapes[1].getCenter();
		
		// y = mx + c
		double m = ( a.getY() - b.getY() ) / ( a.getX() - b.getX() );
		double c = -m*a.getX() + a.getY(); 
		
		// x = ny + d
		double n = ( a.getX() - b.getX() ) / ( a.getY() - b.getY() );
		double d = -n*a.getY() + a.getX(); 
		
		// The difference between the expected and actual position
		double diffx = (m*x + c) - y;
		double diffy = (n*y + d) - x;
		
		//Allow an error of +/-16px
		if ( Math.abs( diffx ) < 16 || Math.abs( diffy ) < 16  )
		{
			return true;
		}
		return false;
	}
}
