import java.awt.Color;
import ecs100.*;

public class Line {
	
	private IShape shapes[];
	private Color colour;
	
	public Line(IShape a, IShape b)
	{
		shapes = new IShape[] {a , b};
	}
	
	public boolean draw ()
	{
		if (!(shapes[0].getVisability() && shapes[1].getVisability()))
		{
			return false;
		}
		
		UI.setColor(this.colour);
		UI.drawLine(shapes[0].getCenter().getX(), shapes[0].getCenter().getY(),
				shapes[1].getCenter().getX(), shapes[1].getCenter().getY());
		return true;
	}
}
