import java.awt.Color;
import java.awt.Point;


public abstract class Shape implements IShape {

	protected int x , y , w , h;
	protected Color border;
	protected Color fill;
	protected boolean visable = true;
	protected boolean disposed = false;
	protected int id;
	protected Text text;
	
	public Shape( int ID , int x, int y, int w, int h , Color border , Color fill )
	{
		this.id = ID;
		this.x = x;
		this.y = y;
		this.w = w;
		this.h = h;
		this.border = border;
		this.fill = fill;
		text = new Text( "" , this);
	}
	
	@Override
	public void setText(String txt) {
		this.text = new Text( txt , this);
	}

	@Override
	public Point getCenter() {
		return new Point( x + w/2 , y + h/2 );
	}

	@Override
	public void setPosition(Point pt) {
		this.x = (int)pt.getX();
		this.y = (int)pt.getY();
		
	}
	
	@Override
	public Point getPosition ()
	{
		return new Point(x , y);
	}
	
	@Override
	public void setVisability(boolean vis) {
		this.visable = vis;
	}

	@Override
	public boolean getVisability() {
		return visable;
	}
	@Override
	public
	void setWidth (int w)
	{
		this.w = w;
		if (text != null)
			text.updateSegments();
	}
	
	@Override
	public void setHeight (int h)
	{
		this.h = h;
		if (text != null)
			text.updateSegments();
	}
	
	@Override
	public Color getFill()
	{
		return this.fill;
	}
	@Override
	public Color getBorder()
	{
		return this.border;
	}
	
	@Override
	public void setFill(Color fill)
	{
		this.fill = fill;
	}
	
	@Override
	public void setBorder(Color border)
	{
		this.border = border;
	}
	
	@Override
	public void dispose()
	{
		disposed = true;
	}
	
	@Override
	public boolean disposed()
	{
		return disposed;
	}
	
	@Override
	public int getID()
	{
		return this.id;
	}
	
	@Override
	public int getWidth()
	{
		return this.w;
	}
	
	@Override
	public int getHeight()
	{
		return this.h;
	}
	
	@Override
	public Text getText()
	{
		return this.text;
	}

}
