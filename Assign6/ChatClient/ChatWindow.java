
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;


public class ChatWindow {

	JFrame window = null;
	JTextArea log;
	JTextArea msg;
	JTextArea users;
	JButton btnPrivateMsg;
	IRCClient client;
	JScrollPane logscroll;
	private String channel;
	private boolean newList = true;

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
        window.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE); // make it close properly

        log = new JTextArea(40,80);  // text area (lines, chars per line)
        msg = new JTextArea(2,80);  // text area (lines, chars per line)
        users = new JTextArea(40,8);

        if (channel.charAt(0) == '#')
        {
	    	btnPrivateMsg = new JButton("Part Channel");
	    	final JFrame fwindow = window;
	        btnPrivateMsg.addMouseListener(new MouseListener() {

				@Override
				public void mouseReleased(MouseEvent e) {}
				@Override
				public void mousePressed(MouseEvent e) {}
				@Override
				public void mouseExited(MouseEvent e) {}
				@Override
				public void mouseEntered(MouseEvent e) {}

				@Override
				public void mouseClicked(MouseEvent e) {
					client.send( String.format ("PART %s :%s",
							new Object[] { channel , "Bye bye" }) );
					fwindow.setVisible(false);
				}
			});
        }

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

						if (!msgout.trim().equals(""))
							client.send( msgout.trim() );

						msg.setText("");
				}
			}

			public void keyPressed(KeyEvent e) {
				return;

			}
		} );

        logscroll = new JScrollPane(log); // put scrollbars around it

        log.setEditable(false);
        users.setEditable(false);

        window.add(logscroll , BorderLayout.WEST);              // add it to the frame.
        window.add(new JScrollPane(users) , BorderLayout.EAST);
        window.add(msg , BorderLayout.SOUTH);                    // add it to the frame.

        if (channel.charAt(0) == '#')
        	window.add(btnPrivateMsg, BorderLayout.NORTH );



        window.pack();                                        // pack things in to the frame
        window.setVisible(true); // make it visible.
        window.setResizable(false); //If not Things break bad UI-wise

        this.client.addCommand( "PRIVMSG", new IRCCommand() {

			public void command(IRCClient client , String cmd , String[] args) {
				if (args.length >= 3)
				{
					final String sender = ( args[1].charAt(0) == '#' ? args[1] : args[0].substring(1 , args[0].indexOf('!')) ).trim();
	    			if (sender.equals(channel))
					{
	    				appendLog( args[0].substring( 1 , args[0].indexOf('!')) + " : " +  args[2].substring(0) );
					}
				}
			}
		});

        this.client.addCommand ( "JOIN" , new IRCCommand() {
			public void command(IRCClient client, String command, String[] args) {
				// TODO Auto-generated method stub
				if (args[1].charAt(0) != '#')
					return;

				if ( args[1].equals(channel))
				{
					if (args[0].trim().equals(client.getUsername()))
					{
						appendLog( "You have joined " + channel );

					} else {
						appendLog( args[0].substring( 1 , args[0].indexOf('!')) + " has joined " + channel);
						users.append( args[0].substring( 1 , args[0].indexOf('!')) + "\n" );
						client.send("NAMES " + channel);
					}
				}
			}
		});


        this.client.addCommand("353", new IRCCommand() {

			@Override
			public void command(IRCClient client, String command, String[] args) {
				if (!args[3].equals(channel))
					return;

				if ( newList ) {
					users.setText("User List\n");
					newList = false;
				}
				for ( String usr :  args[4].split(" ") )
				{
					users.append(usr + "\n");
				}
			}
		});

        this.client.addCommand("366", new IRCCommand() {

			@Override
			public void command(IRCClient client, String command, String[] args) {
				if (!args[2].equals(channel))
					return;

				newList = true;
			}
		});
	}

	public void appendLog (String msg)
	{
		log.setText( log.getText() + msg + "\n" );
	}

	public void setVisable (boolean val)
	{
		this.window.setVisible(val);
	}
}
