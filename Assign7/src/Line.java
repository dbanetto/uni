import java.awt.Color;
import ecs100.*;

public class Line {
	
	private IShape shapes[];
	private Color colour;
	
	public Line(IShape a, IShape b)
	{
		shapes = new IShape[] {a , b};
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
}
