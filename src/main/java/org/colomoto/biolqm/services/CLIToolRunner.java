package org.colomoto.biolqm.services;

import java.io.File;
import java.io.IOException;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.tool.LogicalModelTool;

public class CLIToolRunner {

	private final LogicalModelTool tool;
	private LogicalModelFormat defaultFormat = null;
	
	public CLIToolRunner(String toolID) {
		
		tool = ServiceManager.getManager().getTool(toolID);
		if (tool == null) {
			throw new RuntimeException("Unknown tool: "+toolID);
		}
	}

	public void setFormat(String formatID) {
		this.defaultFormat = CLIConverter.getFormat(formatID);
		if (defaultFormat == null) {
			throw new RuntimeException("Unknown format: "+formatID);
		}
	}

	public void run(String s_in) {
		LogicalModelFormat format = defaultFormat;
		if (format == null) {
			format = CLIConverter.getFormat(s_in);
		}
		
		try {
			LogicalModel model = format.importFile(new File(s_in));
			tool.run(model);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
