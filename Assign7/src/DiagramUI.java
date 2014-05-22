import java.util.ArrayList;
import java.util.List;

import ecs100.*;

public class DiagramUI {
	
	private IShape selected = null;
	private List<Line> lines;
	private List<IShape> shapes;
	
	public void start()
	{
		lines = new ArrayList<Line>();
		
		UI.initialise();
		init();
	}
	
	//Ui Rendering
	private void draw()
	{
		for (int i = 0; i < lines.size(); i++)
		{
			if (lines.get(i).draw() == false)
			{
				lines.remove(i--);
			}
		}
		
		for (IShape shape : shapes)
		{
			shape.draw();
		}
	}
	
	//UI Functions
	private void init ()
	{
		UI.addButton("Clear", new UIButtonListener() {
			@Override
			public void buttonPerformed(String name) {
				// TODO Auto-generated method stub
				
			}
		});
	}
	
}
