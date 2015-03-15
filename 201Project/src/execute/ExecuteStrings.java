package execute;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import com.jcraft.jsch.Channel;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;

import framework.User;

public class ExecuteStrings {

	JSch jsch = new JSch();

	public void executeStrings(List<String> cmd, User user)
			throws JSchException, IOException, InterruptedException {
		Session session = jsch.getSession(user.getUserName(),
				"athena.ecs.csus.edu", 22);
		session.setPassword(user.getPassword());
		java.util.Properties config = new java.util.Properties();
        config.put("StrictHostKeyChecking", "no");
        session.setConfig(config);
		session.connect();
		Channel channel = session.openChannel("shell");// only shell
		channel.setOutputStream(System.out);
		PrintStream shellStream = new PrintStream(channel.getOutputStream()); // printStream
																				// for
																				// convenience
		channel.connect();
		for (String command : cmd) {
			shellStream.println(command);
			shellStream.flush();
		}

		Thread.sleep(5000);

		channel.disconnect();
		session.disconnect();
	}
	
	public List<String> populateCommands(String path) throws IOException{
		List<String> commands = new ArrayList<String>();
		BufferedReader reader = new BufferedReader(new FileReader(path));
		commands.add("sml");
		String temp ;
		while((temp = reader.readLine()) != null){
			commands.add(temp);
		}
		return commands;
	}

}
