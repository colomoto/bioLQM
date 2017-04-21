package org.colomoto.biolqm.helper.clingo;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;

public class ClingoLauncher {

	/**
	 * Ugly hack for now: enforce clingo path
	 */
	public static final String CLINGO = "/opt/bin/clingo";
	private static final File FCLINGO = new File(CLINGO);
	
	ProcessBuilder pb = new ProcessBuilder(CLINGO, "-n", "0");
	
	private final String program;
	private final ClingoResultHandler handler;
	
	public ClingoLauncher(ClingoResultHandler handler, String program) {
		this.program = program;
		this.handler = handler;
	}

	public void run() throws IOException {
		if (!FCLINGO.exists()) {
			System.out.println("% Clingo not found: print the code on stdout");
			System.out.println(program);
			return;
		}
		
		Process proc = pb.start();
		OutputStream stdin = proc.getOutputStream();
		BufferedReader reader = new BufferedReader( new InputStreamReader( proc.getInputStream()));
		stdin.write(program.getBytes());
		stdin.flush();
		stdin.close();

		boolean next_is_solution = false;
		while (true) {
			String line = reader.readLine();
			if (line == null) {
				break;
			}
			line = line.trim();
			if (next_is_solution) {
				next_is_solution = false;
				ClingoResult result = new ClingoResult(line);
				handler.handle(result);
				continue;
			}
			if (line.startsWith("Answer:")) {
				next_is_solution = true;
			}
		}
	}
}

