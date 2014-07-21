package comp103;

import java.awt.FontMetrics;
import java.awt.geom.Rectangle2D;

import ecs100.*;

public class FontUtil {
	public static void drawCenteredString(int x, int y, String text, int width, int height )
	{
		FontMetrics fm = UI.getGraphics().getFontMetrics();
		Rectangle2D bounds = fm.getStringBounds(text, UI.getGraphics());
		
		int render_x = (int)(x + (width / 2) - bounds.getWidth() / 2);
		int render_y = (int)(y + (height / 2) + fm.getDescent());
		
		UI.drawString(text, render_x, render_y);
	}
}
