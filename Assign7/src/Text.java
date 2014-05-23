import java.awt.Color;
import java.awt.FontMetrics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import ecs100.UI;


public class Text {
	

	private IShape parent;

	private Color colour = Color.black;
	Point position = new Point();
	
	//Font rendering functions
	private int y_offset = 0;
	private int x_offset = 0;
	private int font_height = 0;
	private String Text = "";
	private List<String> segments = new ArrayList<String>(); //For line-wrapping
	
	public Text (String text , IShape parent )
	{
		this.Text = text;
		font_height = UI.getGraphics().getFontMetrics().getHeight();
		this.parent = parent;
		updateSegments();
	}
	
	public void draw (int cam_x , int cam_y)
	{
		UI.setColor(colour);
		for (int i = 0; i < segments.size(); i++ )
		{
			UI.drawString( segments.get(i) , cam_x + x_offset , cam_y + y_offset + font_height*i);
		}
	}
	
	public void setText(String text)
	{
		this.Text = text;
		updateSegments();
	}
	
	public void updateSegments()
	{
		FontMetrics fnt = UI.getGraphics().getFontMetrics();
		segments = new ArrayList<String>();
		y_offset = 0;
		x_offset = 0;
		String text = new String(Text).trim();
		int text_width = fnt.charsWidth( text.toCharArray() , 0, text.length() );
		x_offset = text_width;
		if ( text_width > parent.getWidth() )
		{
			
			//Using O as a "best fit for middle of the range char size in a non-monospaced font"
			int char_width = fnt.charWidth('O'); 
			x_offset = fnt.charsWidth( text.toCharArray() , 0, (int)(parent.getWidth() / char_width) );
			
			while ( text_width > parent.getWidth() )
			{
				//Get a nice guess of how far off the set is from being under the width of the shape
				int set = (int)(parent.getWidth() / char_width);
				if (set > text.length())
					break;
				
				while ( fnt.charsWidth( text.toCharArray() , 0, set ) > parent.getWidth() )
				{
					set--;
				}
				//Cut the line at the last space in the line
				int last_word = set - 1;
				while (text.charAt(last_word) != ' ')
				{
					last_word--;
					if (last_word < 0)
						break;
				}
				//If the last_word found the last word, set that as the set size
				// if not go with the set's length it is going to be full of text :( not pretty
				set = ( last_word < 0 ? set : last_word );
				
				segments.add( text.substring(0, set - 1 ).trim() );
				text = text.substring(set).trim() ;
			}
		}
		segments.add(text);
		
		x_offset = -(int)(x_offset / 2);
		y_offset = -(int)(font_height * 0.5 * segments.size() );
	}
	
	public Color getColour()
	{
		return this.colour;
	}
	
	public void setColor(Color colour)
	{
		this.colour = colour;
	}
	
}
