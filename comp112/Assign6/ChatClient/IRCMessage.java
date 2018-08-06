import java.util.List;

public class IRCMessage {
	private String[] args;
	private String command;

	public IRCMessage (String command , List<String> args )
	{
		this.command = command;
		args.toArray( this.args );
	}

	public String getCommand()
	{
		return this.command;
	}

	public String[] getArgs ()
	{
		return this.args;
	}
}
