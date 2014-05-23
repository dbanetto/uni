import java.awt.Color;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JColorChooser;

import ecs100.*;

public class DiagramUI {
	
	private IShape selected = null;
	private List<Line> lines;
	private List<IShape> shapes;
	private boolean sticky_mode = false;
	private int width = 256 , height = 256;
	private boolean shape_delta = false;
	
	private Color fill = Color.white;
	private Color border = Color.black;
	private Color fontcolour = Color.black;
	
	private int id_counter = 0;
	
	private Point camera = new Point(0,0);
	
	/**
	 * Mouse Modes
	 * 0 - Select
	 *-1 - Delete
	 * 5 - Line Select 1st Object
	 * 6 - Line Select Shape to attach to
	 * 10 - Create Rectangle
	 */
	private int mouse_mode = 0;
	
	private boolean render = true;
	
	public DiagramUI()
	{
		lines = new ArrayList<Line>();
		shapes = new ArrayList<IShape>();
	}
	
	public void start()
	{
		UI.initialise();
		UI.setImmediateRepaint(false);
		init();
		
		new Thread(new Runnable() {
			
			@Override
			public void run() {
				// TODO Auto-generated method stub
				while (render)
				{
					try {
						draw();
					} catch (NullPointerException ex)
					{
						// The Graphics pane in the UI class
						// Become null at one point ( most likely during clears )
						// This is catching that oddity
					}
					UI.sleep(10);
				}
			}
		}).start();
		
	}
	
	private boolean validSeclection ()
	{
		return (selected != null && selected.getVisability());
	}
	
	private void select( int x , int y)
	{
		boolean selected_one = false;
		for (IShape shape : shapes)
		{
			if ( shape.select(x, y) )
			{
				selected = shape;
				UI.println( "Shape : " + shape.getPosition().toString() );
				selected_one = true;
			}
		}
		if (!selected_one) {
			selected = null;
		} else {
			//Move the Selected to the top of the draw stack
			shapes.remove(selected);
			shapes.add(shapes.size(), selected);
			shape_delta = true;
		}
	}
	
	//UI Rendering
	private int old_lines_length = 0;
	private int old_shapes_length = 0;
	private void draw()
	{
		if (shapes.size() != old_shapes_length || lines.size() != old_lines_length || shape_delta )
		{
			UI.clearGraphics(false);
			for (int i = 0; i < lines.size(); i++)
			{
				if (lines.get(i).draw( (int)camera.getX() , (int)camera.getY() ) == false)
				{
					lines.remove(i--);
				}
			}
			
			for (IShape shape : shapes)
			{
				shape.draw( (int)camera.getX() , (int)camera.getY() );
			}
			
			UI.repaintGraphics();
			UI.drawString("Camera : " + (int)camera.getX() + ", " + (int)camera.getY(), 0, 11);
			shape_delta = false;
		}
		old_lines_length = lines.size();
		old_shapes_length = shapes.size();
	}
	
	//UI Functions
	private void init ()
	{
		//Give a UI Option for going into Selection mode
		UI.addButton("Select", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				mouse_mode = 0;
			}
		});
		
		UI.addButton("Add Line", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				mouse_mode = 5;
			}
		});
		
		//Ui Option to go into Rectangle creation mode
		UI.addButton("Add Rectangle", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				mouse_mode = 10;
			}
		});
		
		UI.addSlider("Width (px)", 1, 512, new UISliderListener() {
			@Override
			public void sliderPerformed(String name, double value) {
				width = (int)value;
				if (validSeclection()) {
					selected.setWidth((int)value);
					shape_delta = true;
				}
			}
		});
		
		UI.addSlider("Hieght (px)" , 1 , 512 , new UISliderListener() {
			@Override
			public void sliderPerformed(String name, double value) {
				height = (int)value;
				if (validSeclection()) {
					selected.setHeight((int)value);
					shape_delta = true;
				}
				
			}
		});
		
		UI.addButton("Fill Colour", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				if (validSeclection())
				{
					selected.setFill( JColorChooser.showDialog(null, "Select Shape's Fill Colour", selected.getFill() )  );
					shape_delta = true;
				} else
					fill = JColorChooser.showDialog(null, "Select Default Fill Colour", fill);
			}
		});
		
		
		UI.addButton("Border Colour", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				if (validSeclection())
				{
					selected.setBorder( JColorChooser.showDialog(null, "Select Shape's Border Colour", selected.getBorder() )  );
					shape_delta = true;
				} else
					border = JColorChooser.showDialog(null, "Select Default Fill Colour", border);
			}
		});
		
		UI.addButton("Font Colour", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				if (validSeclection())
				{
					selected.getText().setColor( JColorChooser.showDialog(null, "Select Shape's Font Colour", selected.getBorder() )  );
					shape_delta = true;
				} else
					fontcolour = JColorChooser.showDialog(null, "Select Default Font Colour", fontcolour);
			}
		});
		
		// HACK : Terrible UI Name get a better one
		UI.addButton("Toggle Sticky Shape", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				sticky_mode = !sticky_mode;
				if (sticky_mode)
				{
					UI.println("Sticky Shape Mode is enabled");
					UI.println("The cursor will retain the shape last used");
				} else {
					UI.println("Sticky Shape Mode is disabled");
					UI.println("The cursor will NOT retain the shape last used");
					UI.println("Mouse mode is set back to selector");
					mouse_mode = 0;
				}
			}
		});
		
		UI.addButton("Center Camera", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				// TODO Auto-generated method stub
				camera.setLocation(0,0);
			}
		});
		
		UI.addButton("Delete", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				// TODO Auto-generated method stub
				mouse_mode = -1;
			}
		});
		
		UI.addButton("Clear", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				// TODO Auto-generated method stub
				shapes.clear();
				lines.clear();
			}
		});
		
		//Difference between the top left and position clicked
		final Point offset = new Point();
		final Point press_start = new Point();
		final Point camera_start = new Point();
		final Point last_mouse = new Point();
		// Mouse Controller for clicks ( shape creation )
		UI.setMouseMotionListener(new UIMouseListener() {
			@Override
			public void mousePerformed(String action, double x, double y) {
				double cam_x = x + camera.getX();
				double cam_y = y + camera.getY();
				
				//Create new Objects
				if ( action.equals("clicked") )
				{
					if (mouse_mode == 10)
					{
						shapes.add(new Rectangle( id_counter++ , (int)cam_x - width/2, (int)cam_y - height/2, width, height, border , fill));
						shapes.get( shapes.size() - 1 ).getText().setColor( fontcolour );
						shapes.get( shapes.size() - 1 ).setText("WORDS WORDS WORDS WORDS WORDS WORDS WORDS WORDS WORDS WORDS WORDS WORDS WORDS WORDS WORDS WORDS WORDS WORDS WORDS WORDS WORDS ");
						if (!sticky_mode)
							mouse_mode = 0;
					}
					if (mouse_mode == -1)
					{
						select((int)cam_x , (int)cam_y);
						if (validSeclection()) {
							selected.dispose();
							shapes.remove(selected);
						}
						if (!sticky_mode)
							mouse_mode = 0;
					}
					
				} else if (action.equals("pressed"))
				{
					if (mouse_mode == 0)
					{
						select((int)cam_x , (int)cam_y);
						if (validSeclection()) {
							Point selected_pos = selected.getPosition();
							offset.setLocation( cam_x - selected_pos.getX() , cam_y - selected_pos.getY() );
						}
						press_start.setLocation(x , y);
						camera_start.setLocation(camera);
					} else if (mouse_mode == 5)
					{
						select((int)cam_x , (int)cam_y);
						if (validSeclection()) {
							mouse_mode = 6;
							last_mouse.setLocation( x , y );
							
							Point center = selected.getCenter();
							press_start.setLocation(center.getX() - camera.getX() , center.getY() - camera.getY());
							camera_start.setLocation(camera);
						}
					}
					

				} else if (action.equals("dragged") )
				{
					if (mouse_mode == 6)
					{
							//Remove last line
						UI.invertLine( press_start.getX()   ,  press_start.getY()  
									, last_mouse.getX()   , last_mouse.getY()  );
						//Draw a new line
						UI.invertLine( press_start.getX()   ,  press_start.getY()  
										, x   , y  );
						
						selected.draw( (int)camera.getX(), (int)camera.getY() );
						last_mouse.setLocation( x , y );
						UI.repaintGraphics();
					}
				} else if (action.equals("released") )
				{
					if (mouse_mode == 6)
					{
						//Remove last line
						UI.invertLine( press_start.getX()   ,  press_start.getY()  
								, last_mouse.getX()   , last_mouse.getY()  );
						selected.draw( (int)camera.getX(), (int)camera.getY() );
						UI.repaintGraphics();
						
						IShape old_selected = selected;
						select((int)cam_x , (int)cam_y );
						
						if (validSeclection()) {
							lines.add(new Line( old_selected , selected )  );
							selected = null;
						}
						
						if (!sticky_mode)
							mouse_mode = 0;
						else
							mouse_mode = 5;
					}
				}
				
				if (validSeclection())
				{
					if (action.equals("dragged") && selected.select( (int)cam_x, (int)cam_y) && mouse_mode == 0 )
					{
						selected.setPosition(new Point( (int)(cam_x - offset.getX())  ,
														(int)(cam_y - offset.getY()) ));
						shape_delta = true;
					}
				} else {
					if (action.equals("dragged") && mouse_mode == 0) {
						camera.move( (int)(camera_start.getX() -  (x - press_start.getX()) ) , 
									 (int)(camera_start.getY() -  (y - press_start.getY()) ) );
						shape_delta = true;
					}
				}
				
			}
		});
		
	}
	
}
