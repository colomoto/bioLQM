package org.colomoto.biolqm.tool;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.Service;

/**
 * Simple tool description interface.
 * Implement this interface to integrate a tool in the command line interface.
 * 
 * @author Aurelien Naldi
 */
public interface LogicalModelTool extends Service {

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
	 * @param parameters the raw command line parameters
	 */
	void run(LogicalModel model, String parameters);

	/**
	 * Run the tool on a logical model.
	 * 
	 * @param model the model to use
	 * @param parameters the list of provided command line parameters
	 */
	void run(LogicalModel model, String[] parameters);

	/**
	 * Run the tool on a logical model, without parameters
	 * 
	 * @param model the model to use
	 */
	void run(LogicalModel model);

}
