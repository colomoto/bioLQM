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

    public ScriptLauncher(String[] args) {
        this.args = args;
    }

    public LogicalModel load(String filename) {
    	return loadModel(filename);
    }

    public LogicalModel load(String filename, String format) {
    	return loadModel(filename, format);
    }

    public boolean save(LogicalModel model, String filename, String format) {
        return saveModel(model, filename, format);
    }
    
    public LogicalModel modify(LogicalModel model, String name) {
    	return modifyModel(model, name);
    }
    
    public LogicalModel modify(LogicalModel model, String name, String parameters) {
    	return modifyModel(model, name, parameters);
    }

    
    public LogicalModel loadModel(String filename) {
        return loadModel(filename, null);
    }

    public LogicalModel loadModel(String filename, String format) {
    	return Colomoto.loadModel(filename, format);
    }

    public boolean saveModel(LogicalModel model, String filename, String format) {
        return Colomoto.saveModel(model, filename, format);
    }
    
    public LogicalModel modifyModel(LogicalModel model, String name) {
    	return modifyModel(model, name, "");
    }
    
    public LogicalModel modifyModel(LogicalModel model, String name, String parameters) {
    	return getModifier(name).getModifiedModel(model, parameters);
    }
    
    public LogicalModelFormat getFormat(String name) {
    	return LQMServiceManager.getFormat(name);
    }

    public ModelModifierService getModifier(String name) {
    	return LQMServiceManager.getModifier(name);
    }
    
    public Service getService(String name) {
    	return LQMServiceManager.getService(name);
    }
    
    public LogicalModelTool getTool(String name) {
    	return LQMServiceManager.getTool(name);
    }
}
