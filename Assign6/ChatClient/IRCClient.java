import java.net.Socket;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.io.*;

import ecs100.UI;

public class IRCClient implements Runnable {
	private Socket client = null;
	private PrintStream outstream = null;
	private Scanner instream = null;
	private String nickname = "nick";
	private String realname = "IRC Client";
	private String servername = "";

	private Hashtable< String , IRCCommand > commands;
	private Hashtable< String , Object > owners;
	
	private boolean send_lock = false;

	public IRCClient(String Nickname, String Realname) {
		this.nickname = Nickname;
		this.realname = Realname;
		this.commands = new Hashtable<String,IRCCommand>();
		this.owners = new Hashtable<String , Object>();
	}

	public boolean connect(String hostname, int port) {
		try {
			this.client = new Socket(hostname, port);
			if (this.client.isConnected()) {
				UI.println("Connected");
				this.outstream = new PrintStream(client.getOutputStream(), true, "UTF-8");
				this.instream = new Scanner(client.getInputStream(), "UTF-8");
				this.servername = hostname; // This not always going to be true
				// Send Request for Nick Name
				this.send(String.format("NICK %s",
						new Object[] { this.nickname }) );
				this.send(String.format("USER 0 unused %s :%s",
										new Object[] { this.nickname, this.realname }) );
			}
		} catch (Exception ex) {
			UI.println("An Error has occured");
			UI.println(ex.toString());
		}
		return true;
	}

	/**
	 * Send a message to the current server: - check that the socket and the
	 * serverOut are not null - print the message with a \r\n at the end to
	 * serverOut - flush serverOut (to ensure the message is sent)
	 */

	public boolean send(String msg) {
		// Wait for the a send on another thread to finish
		while (send_lock) {
			try {
				Thread.sleep(10);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}

		// Stop other send commands
		send_lock = true;

		try {
			this.outstream.print(msg);
			System.out.println("SENT: " + msg);
			this.outstream.print("\r\n");
			this.outstream.flush();
		} catch (Exception ex) {
			UI.println("Error while sending message");
			UI.println(ex.toString());
			send_lock = false;
			return false;
		}

		// Allow another Send command
		send_lock = false;
		return true;
	}

	/**
	 * Method run in the the thread that is listening to the server. Loop as
	 * long as there is anything in the serverIn scanner: Get and process the
	 * next line of input from the scanner Simple version: prints the line out
	 * for the user Checks if the line contains "SQUIT", if so, close the
	 * socket, set serverIn and serverOut set the quit the program. if the line
	 * contains "PING", send a PONG message back (must be identical to the line,
	 * but with "PING" replaced by "PONG") Better version parses the line into
	 * source, command, params, finalParam (where the source and finalParam are
	 * optional, and params may be empty) Then deals with the message
	 * appropriately.
	 */
	private void listen() {
		while (this.client.isConnected()) {
			if (this.instream.hasNext()) {
				String line = this.instream.nextLine();
				System.out.println("REV: " + line);
				List<String> parts = new ArrayList<String>();
				
				Pattern comPattern = Pattern.compile(" [A-Z0-9]* ");
				Matcher m = comPattern.matcher(line);
				
				
				
				if (m.find()) {
					String cmd = m.group(0).trim();
					String argss = line.substring( line.indexOf( cmd ) + cmd.length() );
					System.out.println("CMD: " + cmd);
					
					int index;
					String last= "";
					if ( (index = argss.indexOf(':')) > 0)
						last = argss.substring( index );
					for (String word : argss.replace( last , "" ).split(" ") )
					{	
						word = word.trim();
						if (word.equals(""))
							continue;
						
						parts.add(word);
						System.out.println("ARG: " + word);
					}
					if (last.length() > 1 && last.charAt(0) == ':')
						last = last.substring(1);
					parts.add(last);
					System.out.println("ARG: " + last);
					
				    if (this.commands.containsKey(cmd))
				    {
				    	this.commands.get(cmd).command( this.owners.get(cmd) , this, (String[])parts.toArray());
				    }
				}
				
			}
		}
	}

	public void run() {
		this.addCommand( this , "MODE", new IRCCommand() {
			
			public void command(Object owner, IRCClient client, String[] args) {
				// TODO Auto-generated method stub
				System.out.println(String.format("Mode now set to %s" 
						, new Object[] { args[0] } ));
			}
		});
		
		if (this.client.isConnected())
			this.listen();
	}

	public void disconnect ()
	{
		this.send("QUIT :Bye bye");

		try {
			this.client.close();
		} catch (IOException e) {
			e.printStackTrace();
		}

		this.outstream = null;
		this.instream = null;
		this.client = null;
	}

	public boolean isConnected() {
		if (this.client != null)
			return this.client.isConnected();
		else
			return false;
	}

	public void addCommand ( Object owner , String Command , IRCCommand logic)
	{
	    if (!this.commands.containsKey(Command))
	    {
	        this.commands.put(Command, (IRCCommand)logic);
	        this.owners.put(Command, owner);
	    }
	}
}
