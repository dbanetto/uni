
import javax.swing.JFrame;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;

import java.awt.BorderLayout;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;


public class ChatWindow {
	
	JFrame window = null;
	JTextArea log;
	JTextArea msg;
	IRCClient client;
	JScrollPane logscroll;
	private String channel;
	
	public ChatWindow (IRCClient client , String channel)
	{
		//Create UI
		this.client = client;
		this.channel = channel;
		this.init();
		if (channel.charAt(0) == '#')
			this.client.send( "JOIN " + channel );
	}
	
	private void init()
	{
		window = new JFrame("Talking on " + this.channel );    // make a frame
		window.setSize(200,300);// set its size
        window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // make it close properly
     
        log = new JTextArea(40,60);  // text area (lines, chars per line)
        msg = new JTextArea(2,60);  // text area (lines, chars per line)
        
        log.addPropertyChangeListener(new PropertyChangeListener() {
			
			public void propertyChange(PropertyChangeEvent evt) {
				window.repaint();
				
			}
		});
        
        msg.addKeyListener( new KeyListener() {
			
			public void keyTyped(KeyEvent e) {
				// TODO Auto-generated method stub
				
			}
			
			public void keyReleased(KeyEvent e) {
				if (e.getKeyCode() == KeyEvent.VK_ENTER)
				{
						String msgout = msg.getText();
						if (msgout.length() == 0)
							return;
						
						if (msgout.charAt(0) == '/')
						{
							msgout = msgout.substring(1);
						} else {
							log.append( client.getUsername() + " : " + msgout.trim() + "\n");
							msgout = "PRIVMSG " + channel + " :" + msgout;
						}
					
						client.send( msgout.trim() );
						msg.setText("");
				}
			}
			
			public void keyPressed(KeyEvent e) {
				return;
				
			}
		} );
        
        logscroll = new JScrollPane(log); // put scrollbars around it
        window.add(logscroll, BorderLayout.NORTH);              // add it to the frame.
        window.add(msg, BorderLayout.SOUTH);                    // add it to the frame.
        
        window.pack();                                        // pack things in to the frame
        window.setVisible(true); // make it visible.
        
        this.client.addCommand( "PRIVMSG", new IRCCommand() {
			
			public void command(IRCClient client , String cmd , String[] args) {
				if (args.length >= 3)
				{
					final String sender = ( args[1].charAt(0) == '#' ? args[1] : args[0].substring(1 , args[0].indexOf('!')) ).trim();
	    			if (sender.equals(channel))
					{
	    				appendLog( args[0].substring( 1 , args[0].indexOf('!')) + " : " +  args[2].substring(0) + "\n" );
					}
				}
			}
		});
	}
	
	public void appendLog (String msg)
	{
		log.setText( log.getText() + msg );
	}
}
