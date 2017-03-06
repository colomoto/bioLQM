package org.colomoto.biolqm.services;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.colomoto.biolqm.tool.LogicalModelTool;

/**
 * Wrap common API to provide convenient access in scripts.
 *
 * @author Aurelien Naldi
 */
public class ScriptLauncher {

    public final String[] args;

    /**
     * Create a scripting environment with access to the command line arguments
     * 
     * @param args the command line arguments
     */
    public ScriptLauncher(String[] args) {
        this.args = args;
    }

    /**
     * Load a model guessing the format.
     * 
     * @param filename the path to the file to load (extension gives the format)
     * @return the loaded model
     */
    public LogicalModel loadModel(String filename) {
        return loadModel(filename, null);
    }

    /**
     * Load a model from a file.
     * If a format is provided, enforce its use, otherwise use the file extension as format name.
     * 
     * @see Colomoto#loadModel(String,String)
     * 
     * @param filename the path to the file to load (extension gives the format)
     * @param format enforced format or null to guess from file extension
     * @return the loaded model
     */
    public LogicalModel loadModel(String filename, String format) {
    	return Colomoto.loadModel(filename, format);
    }

    /**
     * Save a model to a file.
     * @see Colomoto#saveModel(LogicalModel,String,String)
     * 
     * @param model the model to save
     * @param filename the path to the output file 
     * @param format enforced format or null to guess from file extension
     * @return true if success
     */
    public boolean saveModel(LogicalModel model, String filename, String format) {
        return Colomoto.saveModel(model, filename, format);
    }

    /**
     * Construct a modified model.
     * @see #modifyModel(LogicalModel,String,String)
     * 
     * @param model the original model
     * @param name the name of a modifier
     * @return a modified model
     */
    public LogicalModel modifyModel(LogicalModel model, String name) {
    	return modifyModel(model, name, "");
    }
    
    /**
     * Construct a modified model.
     * @see ModelModifierService#getModifiedModel(LogicalModel,String)
     * 
     * @param model the original model
     * @param name the name of a modifier
     * @param parameters parameters for the model modifier
     * @return a modified model
     */
    public LogicalModel modifyModel(LogicalModel model, String name, String parameters) {
    	return getModifier(name).getModifiedModel(model, parameters);
    }

    /**
     * Retrieve a format descriptor.
     * @param name the ID of the format
     * @return the format descriptor
     * @see LQMServiceManager#getFormat(String)
     */
    public LogicalModelFormat getFormat(String name) {
    	return LQMServiceManager.getFormat(name);
    }

    /**
     * Retrieve a modifier service.
     * @param name the ID of the modifier service
     * @return the corresponding modifier service
     * @see LQMServiceManager#getModifier(String)
     */
    public ModelModifierService getModifier(String name) {
    	return LQMServiceManager.getModifier(name);
    }
    
    
    /**
     * Retrieve a service.
     * @param name the ID of the service
     * @return the corresponding service
     * @see LQMServiceManager#getService(String)
     */
    public Service getService(String name) {
    	return LQMServiceManager.getService(name);
    }
    
    /**
     * Retrieve a tool service.
     * @param name the ID of the tool
     * @return the corresponding tool service
     * @see LQMServiceManager#getTool(String)
     */
    public LogicalModelTool getTool(String name) {
    	return LQMServiceManager.getTool(name);
    }
}
