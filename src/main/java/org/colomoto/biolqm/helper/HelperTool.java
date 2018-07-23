package org.colomoto.biolqm.helper;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Locate and call external tools.
 * It currently searches for the command into the PATH and /opt/bin.
 * A future version will facilitate handling of binary helpers packaged with bioLQM.
 * 
 * @author Aurelien Naldi
 */
public class HelperTool {

	private final File binary;
	
	
	private static List<String> PATHS;
	private static String[] MY_PATHS = {
			"/opt/bin"
	};
	
	static {
		String[] paths = System.getenv("PATH").split(":");
		PATHS = new ArrayList<String>(paths.length + MY_PATHS.length);
		for (String s: MY_PATHS) {
			PATHS.add(s);
		}
		for (String s: paths) {
			PATHS.add(s);
		}
	}
	
	private static File lookup(String command) {
		return lookup(command, PATHS);
	}
	
	private static File lookup(String command, List<String> paths) {
		for (String folder: paths) {
			File f = new File(folder, command);
			if (f.exists()) {
				return f;
			}
		}
		return null;
	}
	
	public boolean isAvailable() {
		return binary != null && binary.canExecute();
	}
	
	public HelperTool(String command) {
		this.binary = lookup(command);
	}
	
	
	public ProcessBuilder getProcessBuilder(String... args) {
		if (binary == null) {
			return null;
		}
		List<String> cmd = new ArrayList<String>(args.length+1);
		cmd.add(binary.getAbsolutePath());
		for (String a: args) {
			cmd.add(a);
		}

		return new ProcessBuilder(cmd);
	}
}
