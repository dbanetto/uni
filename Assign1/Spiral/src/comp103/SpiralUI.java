/* Code for COMP 103 Assignment 1
 * Name: David Barnett
 * Usercode: barnetdavi
 * ID: 300313764
 */
package comp103;

import java.awt.Color;

import ecs100.*;

public class SpiralUI {
	
	private final int X_POS = 16;
	private final int Y_POS = 16;
	
	private final int BOX_WIDTH = 32;
	private final int BOX_HEIGHT = 32;
	
	private int[][] boxes;
	
	public SpiralUI() {
		init();
	}
	
	public void init() {
		UI.initialise();
		UI.setImmediateRepaint(false);
		UI.addButton("Line", new UIButtonListener() {
			
			@Override
			public void buttonPerformed(String name) {
				renderLine();
			}
		});
		
		UI.addButton("Sqaure", new UIButtonListener() {
			
			@Override
			public void buttonPerformed(String name) {
				renderSqaure();
			}
		});
		
		UI.addButton("Spiral", new UIButtonListener() {
			
			@Override
			public void buttonPerformed(String name) {
				renderSprial();
			}
		});
		
		UI.addButton("Clear", new UIButtonListener() {
			
			@Override
			public void buttonPerformed(String name) {
				clear(true);
			}
		});
	}
	
	public void clear(boolean redisplay) {
		UI.clearGraphics(redisplay);
	}
	
	public void renderLine() {
		clear(false);
		
		boxes = new int[10][1];
		for (int i = 0; i < boxes.length; i++)
		{
			boxes[i][0] = i + 1;
			UI.setColor(new Color( boxes[i][0] * 25 ));
			UI.fillRect(i * BOX_WIDTH + X_POS, Y_POS , BOX_WIDTH, BOX_HEIGHT);
			
			UI.setColor(Color.white);
			UI.setFontSize(10);
			FontUtil.drawCenteredString(X_POS + BOX_WIDTH * i, Y_POS, String.valueOf(boxes[i][0]), BOX_WIDTH, BOX_HEIGHT);
		}
		UI.repaintGraphics();
	}
	
	public void renderSqaure()
	{
		clear(false);
		
		boxes = new int[10][10];
		for (int y = 0; y < boxes.length; y++)
		{
			for (int x = 0; x < boxes[y].length; x++)
			{
				boxes[y][x] = y * boxes.length + x + 1;
				UI.setColor(new Color( boxes[y][x] * 2 ));
				UI.fillRect(x * BOX_WIDTH + X_POS, y * BOX_HEIGHT + Y_POS, BOX_WIDTH, BOX_HEIGHT);
				
				UI.setColor(Color.white);
				UI.setFontSize(10);
				FontUtil.drawCenteredString(x * BOX_WIDTH + X_POS, y * BOX_HEIGHT + Y_POS, String.valueOf(boxes[y][x]), BOX_WIDTH, BOX_HEIGHT);
			}
		}
		UI.repaintGraphics();
	}
	
	public void renderSprial()
	{
		boxes = new int[10][10];
		
		//Starting X position for each loop
		int[] sx = new int[] {0 , boxes.length - 1 };
		
		// Starting Y position for each loop
		int[] sy = new int[] {0 , boxes.length - 1 };
		
		// starting direction in each loop
		boolean[] sdir = new boolean[] {true , false};
		
		// Counter 
		int n = 1;
		// Needs to run twice for Top Left spiral and bottom right spiral
		for (int i = 0; i < 2; i++)
		{
			int count = 0;
			int upper = boxes.length - 1;
			int lower = 0;
			int xb = sx[i] , yb = sy[i];
			boolean xaxis = true;
			boolean direction = sdir[i];
			
			while (count < (boxes.length * boxes[0].length/2)) {
				boxes[yb][xb] = n++;
				count ++;
				
				if (xaxis) 
					xb += (direction ? 1 : -1);
				 else 
					yb += (direction ? 1 : -1);
				
				if (xaxis && ((xb >= upper && direction) || (xb <= lower && !direction)) )
				{
					xaxis = false;
					if (direction) {
						upper--;
					} else {
						lower += 1;
					}
				} else if (!xaxis && ((yb >= upper&& direction) || (yb <= lower&& !direction)))
				{
					xaxis = true;
					if (direction) {
						lower++;
					} else {
						upper -= 1;
					}
					
					direction = !direction;
				}
			}
		}
		
		for (int y = 0; y < boxes.length; y++)
		{
			for (int x = 0; x < boxes[y].length; x++)
			{
				UI.setColor(new Color( boxes[y][x] * 2 ));
				UI.fillRect(x * BOX_WIDTH + X_POS, y * BOX_HEIGHT + Y_POS, BOX_WIDTH, BOX_HEIGHT);
				
				
				UI.setColor(Color.white);
				UI.setFontSize(10);
				FontUtil.drawCenteredString(x * BOX_WIDTH + X_POS, y * BOX_HEIGHT + Y_POS, String.valueOf(boxes[y][x]), BOX_WIDTH, BOX_HEIGHT);
			}
		}
		UI.repaintGraphics();
	}
}
