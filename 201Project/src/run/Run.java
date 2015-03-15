package run;

import java.io.IOException;

import com.jcraft.jsch.JSchException;

import execute.ExecuteStrings;
import framework.User;

public class Run {

	public static void main(String[] args) {
		try {

			User user = new User();
			user.setUserName("");
			user.setPassword("");
			ExecuteStrings exe = new ExecuteStrings();
			exe.executeStrings(exe.populateCommands(System.getProperty("user.dir")+"/smlFile/project1.sml"), user);

		} catch (JSchException | IOException | InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
