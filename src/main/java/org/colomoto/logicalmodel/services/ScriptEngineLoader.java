package org.colomoto.logicalmodel.services;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import java.io.File;

/**
 * Detect and load script engines
 *
 * @author Aurelien Naldi
 */
public class ScriptEngineLoader {

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

}
