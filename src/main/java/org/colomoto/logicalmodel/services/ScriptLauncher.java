package org.colomoto.logicalmodel.services;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.io.LogicalModelFormat;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

/**
 * wrap common API to provide convenient access in scripts.
 *
 * @author Aurelien Naldi
 */
public class ScriptLauncher {

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
            outputFormat.export(model, new FileOutputStream( filename));
            return true;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return false;

    }

}
