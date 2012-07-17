package org.colomoto.logicalmodel.io;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import org.colomoto.logicalmodel.LogicalModel;

public interface LogicalModelFormat {

	String getID();
	
	String getName();
	
	boolean canExport();
	
	boolean canImport();
	
	void export(LogicalModel model, OutputStream out) throws IOException;

	LogicalModel importFile(File f) throws IOException;
	
}
