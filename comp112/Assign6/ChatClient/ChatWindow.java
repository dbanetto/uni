// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP 112 Assignment
 * Name: David Barnett
 * Usercode: barnda
 * ID: 300313764
 */

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Random;


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
		{	this.client.send( "JOIN " + channel );
			this.client.send("NAMES " + channel );
		}
	}

	private void init()
	{
		if (channel.charAt(0) == '#')
			window = new JFrame("Talking on " + this.channel );    //Channel
		else
			window = new JFrame("Talking to " + this.channel ); //A Person
		window.setSize(200,300);// set its size
        window.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE); // make it close properly

        log = new JTextArea(40,80);;  // text area (lines, chars per line)
        msg = new JTextArea(2,80);  // text area (lines, chars per line)

        if (channel.charAt(0) == '#')
        {
        	users = new JTextArea(40,8);
	    	btnPrivateMsg = new JButton("Leave Channel");
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
							logMessage( client.getUsername(), msgout.trim());
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



        window.add(new JScrollPane(users) , BorderLayout.EAST);
        window.add(msg , BorderLayout.SOUTH);                    // add it to the frame.

        if (channel.charAt(0) == '#') {
        	users.setEditable(false);
        	window.add(logscroll , BorderLayout.WEST);
        	window.add(btnPrivateMsg, BorderLayout.NORTH );
        	window.add(new JScrollPane(users) , BorderLayout.EAST);
            window.add(msg , BorderLayout.SOUTH);
        } else {
        	window.add(logscroll , BorderLayout.CENTER);
            window.add(msg , BorderLayout.SOUTH);
        }


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
	    				logMessage( args[0].substring( 1 , args[0].indexOf('!')) ,  args[2].substring(0) );
					}
				}
			}
		});

        //Display JOIN commands
        this.client.addCommand ( "JOIN" , new IRCCommand() {
			public void command(IRCClient client, String command, String[] args) {
				// TODO Auto-generated method stub
				if (args[1].charAt(0) != '#')
					return;

				if ( args[1].equals(channel))
				{
					if (args[0].trim().equals(client.getUsername()))
					{
						logAppend( "You have joined " + channel );

					} else {
						logAppend( args[0].substring( 1 , args[0].indexOf('!')) + " has joined " + channel);
						users.append( args[0].substring( 1 , args[0].indexOf('!')) + "\n" );
						client.send("NAMES " + channel);
					}
				}
			}
		});

        //Display part messages
        this.client.addCommand("PART", new IRCCommand() {

			@Override
			public void command(IRCClient client, String command, String[] args) {
				// TODO Auto-generated method stub
				if (args[1].charAt(0) != '#')
					return;

				if ( args[1].equals(channel))
				{
					logAppend( args[0].substring( 1 , args[0].indexOf('!')) + " has left " + channel + " : " + args[2]);
				}
			}
		});

        //List of Users in a channel
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

        //End of list
        this.client.addCommand("366", new IRCCommand() {

			@Override
			public void command(IRCClient client, String command, String[] args) {
				if (!args[2].equals(channel))
					return;

				newList = true;
			}
		});

        //Channel Topic
        this.client.addCommand("332", new IRCCommand() {

			@Override
			public void command(IRCClient client, String command, String[] args) {
				if (!args[2].equals(channel))
					return;

				logAppend("Channel Topic" + args[args.length - 1]);
			}
		});
	}

	public void logMessage (String name  , String msg)
	{
		//Get a colour for the User that sent the message
		//Random rnd = new Random(name.hashCode());
		//Color c = new Color( rnd.nextInt(128) , rnd.nextInt(128)  , rnd.nextInt(128) , 255);

		log.append(name);


		log.append(" : " + msg + "\n");

	}

	public void logAppend (String text)
	{
		log.setText( log.getText() + text + "\n" );
	}

	public void setVisable (boolean val)
	{
		this.window.setVisible(val);
	}
}
