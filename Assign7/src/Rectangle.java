import java.awt.Color;
import java.awt.Point;

import ecs100.UI;


public class Rectangle extends Shape {
		
	public Rectangle(int ID, int x, int y, int w, int h, Color border,
			Color fill) {
		super(ID, x, y, w, h, border, fill);
	}

	@Override
	public void draw( int x_camera , int y_camera ) {
		// TODO Auto-generated method stub
		
		UI.setColor(fill);
		UI.fillRect(this.x - x_camera, this.y - y_camera, this.w , this.h);
		UI.setColor(border);
		UI.drawRect(this.x - x_camera, this.y - y_camera, this.w , this.h );
		if (text != null)
			text.draw(this.x - x_camera + this.w / 2 , this.y - y_camera + this.h / 2 );
	}
	
	@Override
	public void draw_outline ( int x_camera , int y_camera )
	{
		UI.invertRect(this.x - x_camera -1 , this.y - y_camera - 1, this.w  + 2, this.h +2);
	}
	
	@Override
	public boolean select(int x, int y) {
		if (disposed)
			return false;
		
		if ( x > this.x && x < this.x + this.w && 
			 y > this.y && y < this.y + this.h	)
		{
			return true;
		}
		return false;
	}
	
}
