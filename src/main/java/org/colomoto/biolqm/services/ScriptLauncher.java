package org.colomoto.biolqm.services;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.io.LogicalModelFormat;
import org.colomoto.biolqm.io.OutputStreamProvider;

import java.io.File;
import java.io.IOException;

/**
 * wrap common API to provide convenient access in scripts.
 *
 * @author Aurelien Naldi
 */
public class ScriptLauncher {

    public ServiceManager srv = ServiceManager.getManager();
    public final String[] args;

    public ScriptLauncher(String[] args) {
        this.args = args;
    }

    public LogicalModel loadModel(String filename) {
        return loadModel(filename, null);
    }

    public LogicalModel loadModel(String filename, String format) {
        if (format == null) {
            format = filename.substring(filename.lastIndexOf(".")+1);
        }
        LogicalModelFormat inputFormat = CLIConverter.getFormat(format);
        if (inputFormat == null) {
            System.err.println("Format not found: " + format);
            return null;
        }

        if (!inputFormat.canImport()) {
            throw new RuntimeException(inputFormat.getID() +" Format does not support import");
        }

        try {
            LogicalModel model = inputFormat.importFile(new File(filename));
            return model;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    public boolean saveModel(LogicalModel model, String filename, String format) {
        if (format == null) {
            format = filename.substring(filename.lastIndexOf(".")+1);
        }
        LogicalModelFormat outputFormat = CLIConverter.getFormat(format);
        if (outputFormat == null) {
            System.err.println("Format not found: "+format);
            return false;
        }

        if (!outputFormat.canExport()) {
            throw new RuntimeException(outputFormat.getID() +" Format does not support export");
        }

        try {
            outputFormat.export(model, new OutputStreamProvider( filename));
            return true;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return false;

    }

}
