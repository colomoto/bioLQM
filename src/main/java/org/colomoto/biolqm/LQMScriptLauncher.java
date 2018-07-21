package org.colomoto.biolqm;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.modifier.ModelModifierService;
import org.colomoto.biolqm.tool.ModelToolService;

/**
 * Wrap common API to provide convenient access in scripts.
 *
 * @author Aurelien Naldi
 */
public class LQMScriptLauncher {

	/**
	 * Detect and load script engines
	 * 
	 * @param scriptname path to the script file
	 * @return the associated engine
	 * @throws Exception if no engine was found or loading failed
	 */
    public static ScriptEngine loadEngine(String scriptname) throws Exception {
        File f = new File(scriptname);
        if (!f.exists()) {
            throw new RuntimeException("Unable to find the script file");
        }

        int lastDot = scriptname.lastIndexOf('.');
        if (lastDot < 0) {
            throw new RuntimeException("No extension: unable to guess the scripting language");
        }
        String extension = scriptname.substring(lastDot+1);

        // create JavaScript engine
        ScriptEngineManager manager = new ScriptEngineManager(ExtensionLoader.getClassLoader());
        ScriptEngine engine = manager.getEngineByExtension(extension);

        if (engine == null) {
            throw new RuntimeException("No engine found for "+extension);
        }

        return engine;
    }

    public final String[] args;

    /**
     * Create a scripting environment with access to the command line arguments
     * 
     * @param args the command line arguments
     */
    public LQMScriptLauncher(String[] args) {
        this.args = args;
    }

    /**
     * Load a model guessing the format.
     * 
     * @param filename the path to the file to load (extension gives the format)
     * @return the loaded model
     */
    public LogicalModel load(String filename) {
        return load(filename, null);
    }

    /**
     * Load a model from a file.
     * If a format is provided, enforce its use, otherwise use the file extension as format name.
     *
     * @see LQMLauncher#load(String,String)
     *
     * @param filename the path to the file to load (extension gives the format)
     * @param format enforced format or null to guess from file extension
     * @return the loaded model
     */
    public LogicalModel load(String filename, String format) {
        return LQMLauncher.load(filename, format);
    }

    /**
     * Save a model to a file.
     * @see LQMLauncher#save(LogicalModel,String,String)
     * 
     * @param model the model to save
     * @param filename the path to the output file 
     * @param format enforced format or null to guess from file extension
     * @return true if success
     */
    public boolean save(LogicalModel model, String filename, String format) {
        return LQMLauncher.save(model, filename, format);
    }

    /**
     * Construct a modified model.
     * @see #modifyModel(LogicalModel,String,String)
     * 
     * @param model the original model
     * @param name the name of a modifier
     * @return a modified model
     */
    public LogicalModel modify(LogicalModel model, String name) {
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
    public LogicalModel modify(LogicalModel model, String name, String parameters) {
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
     * Retrieve a tool service.
     * @param name the ID of the tool
     * @return the corresponding tool service
     * @see LQMServiceManager#getTool(String)
     */
    public ModelToolService getTool(String name) {
    	return LQMServiceManager.getTool(name);
    }

    /**
     * Open a file for writing.
     * A simple wrapper for FileWrapper constructor, to be used in scripts
     * 
     * @param path the path to the file to open
     * @return a FileWriter object
     * @throws IOException
     */
    public FileWriter fileWriter(String path) throws IOException {
		return new FileWriter(path);
    }


    /**
     * @deprecated replaced by {@link #load(String)}
     * @param filename the path to the file to load (extension gives the format)
     * @return the loaded model
     */
    @Deprecated
    public LogicalModel loadModel(String filename) {
        return load(filename, null);
    }

    /**
     * @deprecated replaced by {@link #load(String,String)}
     * @param filename the path to the file to load (extension gives the format)
     * @param format enforced format or null to guess from file extension
     * @return the loaded model
     */
    @Deprecated
    public LogicalModel loadModel(String filename, String format) {
        return LQMLauncher.load(filename, format);
    }

    /**
     * @deprecated replaced by {@link #save(LogicalModel, String,String)}
     * @param model the model to save
     * @param filename the path to the output file
     * @param format enforced format or null to guess from file extension
     * @return true if success
     */
    @Deprecated
    public boolean saveModel(LogicalModel model, String filename, String format) {
        return LQMLauncher.save(model, filename, format);
    }

    /**
     * @deprecated replaced by {@link #modify(LogicalModel, String)}
     * @param model the original model
     * @param name the name of a modifier
     * @return a modified model
     */
    @Deprecated
    public LogicalModel modifyModel(LogicalModel model, String name) {
        return modify(model, name, "");
    }

    /**
     * @deprecated replaced by {@link #modify(LogicalModel, String, String)}
     * @param model the original model
     * @param name the name of a modifier
     * @param parameters parameters for the model modifier
     * @return a modified model
     */
    @Deprecated
    public LogicalModel modifyModel(LogicalModel model, String name, String parameters) {
        return getModifier(name).getModifiedModel(model, parameters);
    }
}
