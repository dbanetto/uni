
import javax.swing.JFrame;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;

import java.awt.BorderLayout;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;


public class ChatWindow implements KeyListener {
	
	JFrame window = null;
	JTextArea log;
	JTextArea msg;
	IRCClient client;
	JScrollPane logscroll;
	
	public ChatWindow (IRCClient client)
	{
		//Create UI
		this.client = client;
		this.init();
	}
	
	private void init()
	{
		window = new JFrame("Output");    // make a frame
		window.setSize(200,300);// set its size
        window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // make it close properly
     
        log = new JTextArea(40,60);  // text area (lines, chars per line)
        msg = new JTextArea(2,60);  // text area (lines, chars per line)
        
        msg.addKeyListener( this );
        
        logscroll = new JScrollPane(log); // put scrollbars around it
        window.add(logscroll, BorderLayout.NORTH);              // add it to the frame.
        window.add(msg, BorderLayout.SOUTH);                    // add it to the frame.
        
        window.pack();                                        // pack things in to the frame
        window.setVisible(true);                             // make it visible.

	}

	public void keyPressed(KeyEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	public void keyReleased(KeyEvent arg0) {
		// TODO Auto-generated method stub
		if (arg0.getKeyCode() == arg0.VK_ENTER)
		{
			if (this.msg.equals( arg0.getComponent() ) )
			{
				this.client.send( this.msg.getText() );
				this.msg.setText("");
			}
		}
	}

	public void keyTyped(KeyEvent arg0) {
		// TODO Auto-generated method stub
		
	}
	
	
}
