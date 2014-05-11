// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP 112 Assignment
 * Name: David Barnett
 * Usercode: barnda
 * ID: 300313764
 */

import ecs100.*;

import java.awt.BorderLayout;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.*;
import java.lang.*;
import java.net.Socket;
import java.util.*;
import java.net.Socket;
import java.io.*;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

/**
 * Basic IRC Chat Client
 */

public class ChatClient implements UIButtonListener, UITextFieldListener {
    private String server = "irc.ecs.vuw.ac.nz";  // default IRC server for testing.
    private int port = 6667;     // The standard IRC port number.
    private String username = "";
    private String realname = "";
    private IRCClient client;
    private Thread listner;
    private Hashtable<String , ChatWindow > windows = new Hashtable<String , ChatWindow>();

    
    public static void main(String[] args) throws IOException {
        new ChatClient( "irc.ecs.vuw.ac.nz" , (((((((6667)))))))); //Brakcets for Pondy
    }

    /*
     * Sets up the user interface.
     */
    public ChatClient ( String server , int port ){
        UI.addButton("Connect", this);
        UI.addButton("Connect to Channel", this);
        UI.addButton("Private Message", this);

        UI.addButton("Disconnect", this);
        /*# YOUR CODE HERE */
        this.server = server;
        this.port = port;
        this.login();
    }


    /**
     * Respond to the buttons
     */
    public void buttonPerformed(String button){
        if (button.equals("Connect") && (client == null || !client.isConnected()) ) {
            this.connect();
        }else if (button.equals("Disconnect") && client.isConnected()) {
            this.closeConnection();
        } else if (button.equals("Connect to Channel") && client.isConnected())
        {
        	final JDialog dialog = new JDialog();
			JLabel label = new JLabel("Enter Channel: ");
			final JTextField channel = new JTextField();
			final JTextArea channellist = new JTextArea(32,8);
			channellist.setText("Channel List\n");
			channellist.setEditable(false);
			JScrollPane scoll = new JScrollPane(channellist);
			scoll.setSize(80, 160);
			JButton commit = new JButton("Join");

			commit.addMouseListener( new MouseListener() {

				@Override
				public void mouseReleased(MouseEvent e) {
					// TODO Auto-generated method stub

				}

				@Override
				public void mousePressed(MouseEvent e) {
					// TODO Auto-generated method stub

				}

				@Override
				public void mouseExited(MouseEvent e) {
					// TODO Auto-generated method stub

				}

				@Override
				public void mouseEntered(MouseEvent e) {
					// TODO Auto-generated method stub

				}

				@Override
				public void mouseClicked(MouseEvent e) {
					// TODO Auto-generated method stub
					String ch = channel.getText();
					if (ch.charAt(0) != '#')
					{
						ch = "#" + ch;
					}
					client.send( "JOIN " + ch);
					dialog.setVisible(false);
				}
			});

			dialog.add( label , BorderLayout.NORTH);
			dialog.add( channel , BorderLayout.CENTER);
			dialog.add ( scoll , BorderLayout.EAST);
			dialog.add (commit , BorderLayout.SOUTH);
			
			dialog.setSize( 480 , 160);
			dialog.setResizable(false);
			dialog.setVisible(true);
			
			client.addCommand("322", new IRCCommand() {
				
				@Override
				public void command(IRCClient client, String command, String[] args) {
					channellist.append(args[2] + " (" + args[3] + ")\n");
				}
			});
			client.send("LIST");
			
		} else if (button.equals("Private Message") && client.isConnected())
		{
			//Create A dialog to send a message to someone
        	final JDialog dialog = new JDialog();
			final JTextField to = new JTextField("To");
			final JTextField mesg = new JTextField("Message");
			final JTextArea userlist = new JTextArea(32,8);
			userlist.setText("User List");
			userlist.setEditable(false);
			JScrollPane scoll = new JScrollPane(userlist);
			JButton commit = new JButton("Send");

			commit.addMouseListener( new MouseListener() {

				@Override
				public void mouseReleased(MouseEvent e) {
					// TODO Auto-generated method stub

				}

				@Override
				public void mousePressed(MouseEvent e) {
					// TODO Auto-generated method stub

				}

				@Override
				public void mouseExited(MouseEvent e) {
					// TODO Auto-generated method stub

				}

				@Override
				public void mouseEntered(MouseEvent e) {
					// TODO Auto-generated method stub

				}

				@Override
				public void mouseClicked(MouseEvent e) {
					// TODO Auto-generated method stub
					client.send( "PRIVMSG " + to.getText() + " :" + mesg.getText());
					dialog.setVisible(false);
				}
			});

			dialog.add( to , BorderLayout.NORTH);
			dialog.add( mesg , BorderLayout.CENTER);
			dialog.add( userlist , BorderLayout.EAST);
			dialog.add (commit , BorderLayout.SOUTH);
			
			
			
			dialog.setSize( 480 , 160);
			dialog.setResizable(false);
			dialog.setVisible(true);
		}
    }

    /**
     * Respond to the text field
     */
    public void textFieldPerformed(String name , String text){
        /*# YOUR CODE HERE */


    }

    /*
     * If there is currently an active socket, it should close the
     *  connection and set the socket to null.
     * Creates a socket connected to the server.
     * Creates a Scanner on the input stream of the socket,
     *  and a PrintStream on the output stream of the socket.
     * Logs in to the server (calling the loginToServer Message)
     * Once login is successful, starts a separate thread to
     *  listen to the server and process the messages from it.
     */
    public void connect()
    {
    	if (this.client != null && this.client.isConnected())
    		this.closeConnection();
    	
        this.client = new IRCClient ( this.username , this.realname );
        this.client.connect(this.server , this.port );
        windows = new Hashtable<String , ChatWindow>();
        
        this.listner = new Thread ( this.client );
        this.initListners();
        this.listner.start();
    }
    
    /*
     * Method run in the the thread that is listening to the server.
     * Loop as long as there is anything in the serverIn scanner:
     *   Get and process the next line of input from the scanner
     *   Simple version:
     *    prints the line out for the user
     *    Checks if the line contains "SQUIT",
     *       if so, close the socket, set serverIn and serverOut set the quit the program.
     *      if the line contains "PING", send a PONG message back
     *        (must be identical to the line, but with "PING" replaced by "PONG")
     *   Better version parses the line into source, command, params, finalParam
     *    (where the source and finalParam are optional, and params may be empty)
     *    Then deals with the message appropriately.
     */
    /*
     * Attempt to log in to the Server and return true if successful, false if not.
     *  Ask user for username and real name
     *  Send info to server (NICK command and USER command)
     *  Read lines from server until get a message containing 004 (success) or
     *   a message containing 433 (failure - nickname in use)
     *  (For debugging, at least, print out all lines from the server)
     */
    private void login()
    {
        this.username = UI.askToken("Enter your usercode: ");
        this.realname = UI.askString("Enter your real name: ");
    }

    private void initListners()
    {
    	//Open Dialog to ask for a new username
    	this.client.addCommand( "433", new IRCCommand() {

			public void command(IRCClient client, String cmd, String[] args) {
				getNewUsername();
			}
		});
    	
    	this.client.addCommand("432", new IRCCommand() {
			
			@Override
			public void command(IRCClient client, String command, String[] args) {
				// TODO Auto-generated method stub
				getNewUsername();
			}
		});

    	this.client.addCommand( "*", new IRCCommand() {

			public void command(IRCClient client, String cmd, String[] args) {
				UI.print( cmd );
				for (String a : args)
				{
					UI.print( " " +  a );
				}
				UI.println("");
			}
		});

    	this.client.addCommand( "PRIVMSG", new IRCCommand() {
    		public void command(final IRCClient client, String command, final String[] args) {
				final String sender = ( args[1].charAt(0) == '#' ? args[1] : args[0].substring(1 , args[0].indexOf('!')) ).trim();
    			if ( !windows.containsKey(sender) )
				{
					System.out.println("Creating window for " + sender );
					new Thread ( new Runnable() {
						public void run() {
							windows.put( sender , new ChatWindow( client , sender ) );
							windows.get( sender ).logMessage( args[0].substring( 1 , args[0].indexOf('!')),  args[2].substring(0) );
						}
					}).start();
				}
			}
		});


    	this.client.addCommand("JOIN", new IRCCommand() {

			@Override
			public void command(final IRCClient client, String command, final String[] args) {
				final String sender = ( args[1].charAt(0) == '#' ? args[1] : args[0].substring(1 , args[0].indexOf('!')) ).trim();
    			if ( !windows.containsKey(sender) )
				{
					System.out.println("Creating window for " + sender );
					new Thread ( new Runnable() {
						public void run() {
							windows.put( sender , new ChatWindow( client , sender ) );
							windows.get( sender ).logAppend( args[0].substring( 1 , args[0].indexOf('!')) + " has joined." );
						}
					}).start();
				} else {
					windows.get(sender).setVisable(true);
				}
			}
		});
    }

    /**
     * Close the connection:
     *  - close the socket,
     *  - set the serverIn and serverOut to null
     *  - print a message to the user
     */
    public void closeConnection(){
    	this.client.disconnect();
    }

    public void setUsername (String newNick , boolean tellServer)
    {
    	if (tellServer) {
    		client.send(String.format("NICK %s",
				new Object[] { newNick }) );
    	}
    	this.username = newNick;
    }
    
    private void getNewUsername()
    {
    	final JDialog dialog = new JDialog();
		JLabel label = new JLabel("Enter new Username: ");
		final JTextField name = new JTextField();
		JButton commit = new JButton("Commit");

		commit.addMouseListener( new MouseListener() {

			@Override
			public void mouseReleased(MouseEvent e) {
				// TODO Auto-generated method stub

			}

			@Override
			public void mousePressed(MouseEvent e) {
				// TODO Auto-generated method stub

			}

			@Override
			public void mouseExited(MouseEvent e) {
				// TODO Auto-generated method stub

			}

			@Override
			public void mouseEntered(MouseEvent e) {
				// TODO Auto-generated method stub

			}

			@Override
			public void mouseClicked(MouseEvent e) {
				// TODO Auto-generated method stub
				setUsername( name.getText() , true );
				dialog.setVisible(false);
			}
		});

		dialog.add( label , BorderLayout.NORTH);
		dialog.add( name , BorderLayout.CENTER);

		dialog.add (commit , BorderLayout.SOUTH);

		dialog.setSize( 200 , 150);
		dialog.setResizable(false);
		dialog.setVisible(true);
    }

}
