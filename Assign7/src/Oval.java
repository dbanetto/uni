import java.awt.Color;
import java.awt.Point;

import ecs100.UI;


public class Oval extends Rectangle {
	public Oval( int ID , int x, int y, int w, int h , Color border , Color fill )
	{
		super (ID , x ,y ,w ,h , border , fill );
	}
	
	@Override
	public void draw(int x_camera, int y_camera) {
		// TODO Auto-generated method stub
		UI.setColor(fill);
		UI.fillOval(this.x - x_camera, this.y - y_camera, this.w , this.h);
		UI.setColor(border);
		UI.drawOval(this.x - x_camera, this.y - y_camera, this.w , this.h );
		if (text != null)
			text.draw(this.x - x_camera + this.w / 2 , this.y - y_camera + this.h / 2 );
	}

	@Override
	public boolean select(int x, int y) {
		if (super.select(x, y))
		{
			return ( Math.pow(x - this.getCenter().getX() , 2) / ((w/2)*(w/2)) + Math.pow(y - this.getCenter().getY() , 2) / ((h/2)*(h/2)) < 1 );
		}
		return false;
	}
}
