import java.awt.*;

public interface IShape {
	
	//ID System
	int getID();
	
	//Render to UI
	void draw ( int x_camera , int y_camera );
	
	//Detect if the mouse has clicked it
	boolean select (int x ,int y);
	
	void setText (String text);
	
	// Current Position plus half of width/height
	Point getCenter ();
	
	void setPosition(Point pt);
	Point getPosition ();
	
	void setWidth (int w);
	void setHeight (int h);
	
	int getWidth();
	int getHeight();
	
	Color getFill();
	Color getBorder();
	
	Text getText();
	
	void setFill(Color fill);
	void setBorder(Color border);
	
	//Set/Get Visability
	void setVisability(boolean vis);
	boolean getVisability();
	
	void dispose();
	boolean disposed ();
}
