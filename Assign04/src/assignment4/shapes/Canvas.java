package assignment4.shapes;

import java.awt.*;
import java.awt.image.BufferedImage;

import javax.swing.JFrame;

/**
 * A canvas represents a place on which one can draw points. A canvas will
 * automatically resize to ensure that it is big enough for any point to be
 * drawn.
 *
 * @author djp
 *
 */
public class Canvas {
	private int width;
	private int height;
	private Color[][] grid;
	private JFrame window = null;
	private java.awt.Canvas jcanvas = null;

	/**
	 * Construct an empty Canvas.
	 */
	public Canvas() {
		grid = null;
		width = 0;
		height = 0;
	}

	/**
	 * Create a canvas with initial dimensions and colour
	 * @param width width of canvas
	 * @param height height of canvas
	 * @param defaultColor Colour for whole canvas
	 */
	public Canvas(int width, int height, Color defaultColor) {
		this.width = width;
		this.height = height;
		grid = new Color[width][height];
		for(int x=0;x!=width;++x) {
			for(int y=0;y!=height;++y) {
				grid[x][y] = new Color(defaultColor);
			}
		}
	}

	/**
	 * Construct a canvas from an existing canvas.
	 * @param c To copy from
	 */
	public Canvas(Canvas c) {
		width = c.width;
		height = c.height;
		grid = new Color[width][height];
		for(int x=0;x!=width;++x) {
			for(int y=0;y!=height;++y) {
				grid[x][y] = c.grid[x][y];
			}
		}
	}

	/**
	 * Return the current width of this canvas.
	 * @return width of canvas
	 */
	public int width() {
		return width;
	}

	/**
	 * Return the current height of this canvas
	 * @return height of canvas
	 */
	public int height() {
		return height;
	}

	/**
	 * Return the color at the given x and y co-ordinate. Observe that this
	 * co-ordinate must be within the bounds of the canvas, otherwise an
	 * exception will occur.
	 *
	 * @return Color at point (x,y)
	 */
	public Color colorAt(int x, int y) {
		if (x < 0) {
			throw new IllegalArgumentException(
					"x position cannot be negative!");
		}
		if (y < 0) {
			throw new IllegalArgumentException(
					"y position cannot be negative!");
		}
		if (x >= width) {
			throw new IllegalArgumentException(
					"x position exceeds canvas width!");
		}
		if (y >= height) {
			throw new IllegalArgumentException(
					"y position exceeds canvas height!");
		}
		return grid[x][y];
	}

	/**
	 * Draw a point of the specific color within the canvas.
	 *
	 * If the x or y is greater than the width or height the canvas is
	 * expanded
	 *
	 * @param x x coordinate to draw at
	 * @param y y coordinate to draw at
	 * @param color colour to be drawn at (x,y)
	 */
	public void draw(int x, int y, Color color) {
		if (x < 0) {
			throw new IllegalArgumentException(
					"x position cannot be negative!");
		}
		if (y < 0) {
			throw new IllegalArgumentException(
					"y position cannot be negative!");
		}

		int newWidth = Math.max(width, x + 1);
		int newHeight = Math.max(height, y + 1);

		if (newWidth != width || newHeight != height) {
			// In this case, the canvas is not big enough.
			// Therefore, we must automatically resize it.
			Color[][] ngrid = new Color[newWidth][newHeight];
			for(int i=0;i!=newWidth;++i) {
				for(int j=0;j!=newHeight;++j) {
					if(i < width && j < height) {
						// copy old color
						ngrid[i][j] = grid[i][j];
					} else {
						// put in default color in.
						ngrid[i][j] = Color.WHITE;
					}
				}
			}

			grid = ngrid;
			width = newWidth;
			height = newHeight;
		}
		grid[x][y] = color;
	}

	/**
	 * Show the contents of the canvas using a simple GUI Window. This method is
	 * for debugging purposes or interactive mode.
	 */
	public void show() {
		final BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
		for(int x=0;x!=width;++x) {
			for(int y=0;y!=height;++y) {
				img.setRGB(x, y, grid[x][y].toRGB());
			}
		}
		if (window == null) {
			window = new JFrame("Assignment 3, Canvas Viewer");
			jcanvas = new java.awt.Canvas() {
				public void paint(Graphics g) {
					g.drawImage(img, 10, 10, null);
				}
			};
			window.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
			window.setBounds(0, 0, width + 30, height + 50);
			window.add(jcanvas);
			window.pack();
			window.setVisible(true);
			window.setMinimumSize(new Dimension(width + 30, height + 50));
		} else {
			// redraw for interactive mode
			window.remove(jcanvas);
			jcanvas = new java.awt.Canvas() {
				public void paint(Graphics g) {
					g.drawImage(img, 10, 10, null);
				}
			};
			window.setBounds(0, 0, width + 30, height + 50);
			window.setMinimumSize(new Dimension(width + 30, height + 50));
			window.add(jcanvas);
			window.pack();
		}
	}

	/**
	 * Convert the contents of the canvas into a string format, where each point
	 * is given using as a 6 digit hexadecimal string.
	 */
	public String toString() {
		StringBuilder r = new StringBuilder();

		// now write out the canvas
		for(int y = 0; y < height; ++y) {
			for(int x = 0; x < width ; ++x) {
				r.append(colorAt(x,y));
			}
			r.append('\n');
		}
		return r.toString();
	}
}
