package org.colomoto.biolqm.tool;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.Service;

/**
 * Simple tool description interface.
 * Implement this interface to integrate a tool in the command line interface.
 * 
 * @author Aurelien Naldi
 */
public interface LogicalModelTool<R,S extends ToolSettings> extends Service {

	/**
	 * Does this tool handle multivalued models?
	 * 
	 * @return true if it supports multivalued models, false for Boolean only
	 */
	boolean supportsMultivalued();
	
	/**
	 * Construct a default setting object.
	 *
	 * @param model the source model
	 * @return the custom setting object
	 */
	S getSettings(LogicalModel model);
	
	/**
	 * Construct a parsed setting object.
	 *
	 * @param model the source model
	 * @param parameters optional command line settings
	 * @return the custom setting object
	 */
	S getSettings(LogicalModel model, String parameters);
	
	/**
	 * Construct a default setting object.
	 *
	 * @param model the source model
	 * @param parameters optional command line settings
	 * @return the custom setting object
	 */
	S getSettings(LogicalModel model, String ... parameters);
	
	/**
	 * Get the analysis result with a custom setting.
	 * 
	 * @param settings a custom setting object
	 * @return the result object
	 * @throws Exception when the analysis could not be performed
	 */
	R getResult(S settings) throws Exception;
	
	/**
	 * Get the analysis result.
	 * 
	 * @param model the source model
	 * @return the result object
	 * @throws Exception when the analysis could not be performed
	 */
	R getResult(LogicalModel model) throws Exception;

	/**
	 * Get the analysis result.
	 * 
	 * @param model the source model
	 * @param parameters optional command line settings (space separated)
	 * @return the result object
	 * @throws Exception when the analysis could not be performed
	 */
	R getResult(LogicalModel model, String parameters) throws Exception;
	
	/**
	 * Get the analysis result.
	 * 
	 * @param model the source model
	 * @param parameters optional list of command line settings
	 * @return the result object
	 * @throws Exception when the analysis could not be performed
	 */
	R getResult(LogicalModel model, String ... parameters) throws Exception;
	
	/**
	 * Run the tool on a logical model.
	 * 
	 * @param model the model to use
	 * @param parameters the raw command line parameters
	 */
	void run(LogicalModel model, String ... parameters);

}
