package org.colomoto.biolqm.tool;

import java.io.IOException;

import org.colomoto.biolqm.LogicalModel;

/**
 * Simple tool description interface.
 * Implement this interface to integrate a tool in the command line interface.
 * 
 * @author Aurelien Naldi
 */
public interface LogicalModelTool {

	/**
	 * get the ID of the tool.
	 * @return the tool ID
	 */
	String getID();
	
	/**
	 * Get a longer name for the tool.
	 * This is descriptive only and has no real role.
	 * @return the tool name
	 */
	String getName();

	/**
	 * Does this tool handle multivalued models?
	 * 
	 * @return true if it supports multivalued models, false for Boolean only
	 */
	boolean supportsMultivalued();
	
	/**
	 * Run the tool on a logical model.
	 * 
	 * @param model the model to use
	 */
	void run(LogicalModel model) throws IOException;

}
