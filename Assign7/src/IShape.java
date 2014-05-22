import java.awt.*;

public interface IShape {
	
	//Render to UI
	void draw ( );
	
	//Detect if the mouse has clicked it
	boolean select (int x ,int y);
	
	void setText (String text);
	
	// Current Position plus half of width/height
	Point getCenter ();
	
	void setPosition(Point pt);
	
	//Set/Get Visability
	void setVisability(boolean vis);
	boolean getVisability();
}
