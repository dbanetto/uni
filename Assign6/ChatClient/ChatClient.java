// This program is copyright VUW.
// You are granted permission to use it to construct your answer to a COMP112 assignment.
// You may not distribute it in any other way without permission.

/* Code for COMP 112 Assignment
 * Name: David Barnett
 * Usercode: barnda
 * ID: 300313764
 */

import ecs100.*;

import java.io.*;
import java.lang.*;
import java.net.Socket;
import java.util.*;
import java.net.Socket;
import java.io.*;

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

    /**
     * main: construct a new ChatClient
     */
    public static void main(String[] args) throws IOException {
        new ChatClient( "irc.ecs.vuw.ac.nz" , 6667 );
    }

    /*
     * Sets up the user interface.
     */
    public ChatClient ( String server , int port ){
        UI.addButton("Connect", this);
        /*# YOUR CODE HERE */
        this.server = server;
        this.port = port;
        this.login();
    }


    /**
     * Respond to the buttons
     */
    public void buttonPerformed(String button){
        if (button.equals("Connect")) {
            this.connect();
        }
        /*# YOUR CODE HERE */


    }

    /**
     * Respond to the text feild
     */
    public void textFieldPerformed(String name , String text){
        /*# YOUR CODE HERE */


    }

    /**
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
        /*# YOUR CODE HERE */

        this.client = new IRCClient ( this.username , this.realname );
        this.client.connect(this.server , this.port );
        this.listner = new Thread ( this.client );
        this.initListners();
        this.listner.run();
        
    }

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


    /**
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
    private void listenToServer() {
        /*# YOUR CODE HERE */

    }
    
    private void initListners()
    {
    	this.client.addCommand( this ,"PING", new IRCCommand() {
			
			public void command(Object owner , IRCClient client, String[] args) {
				client.send( String.format( "PONG :%s" , new Object[] { args[0] } ) );
			}
		});
    	
    	this.client.addCommand(this , "433", new IRCCommand() {
			
			public void command(Object owner, IRCClient client, String[] args) {
				// TODO Auto-generated method stub
				ChatClient ui = (ChatClient)(owner);
				ui.setUsername(UI.askToken("Enter your usercode: "), true);
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
        /*# YOUR CODE HERE */
    }

    public void setUsername (String newNick , boolean tellServer)
    {
    	if (tellServer) {
    		client.send(String.format("NICK %s",
				new Object[] { newNick }) );
    	}
    	this.username = newNick;
    }

}
