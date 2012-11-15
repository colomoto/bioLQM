package org.colomoto.logicalmodel.io;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import org.colomoto.logicalmodel.LogicalModel;

/**
 * Description of an available file format.
 * Implement this interface to integrate new formats in the conversion tool.
 * 
 * @author Aurelien Naldi
 */
public interface LogicalModelFormat {

	/**
	 * get the ID of the format. It will be used to retrieve the format
	 * @return the format ID
	 */
	String getID();
	
	/**
	 * Get a longer name for the format.
	 * This is descriptive only and has no real role.
	 * @return the format name
	 */
	String getName();

	/**
	 * Does this format supports export operation?
	 * @return true if this format implements export.
	 */
	boolean canExport();
	
	/**
	 * Does this format supports import operation?
	 * @return true if this format implements import.
	 */
	boolean canImport();

	/**
	 * Does this format handle multivalued models?
	 * 
	 * @return true if it supports multivalued models, false for Boolean formats
	 */
	boolean supportsMultivalued();
	
	/**
	 * Export a logical model to this format.
	 * 
	 * @param model the model to export
	 * @param out an outputstream to write the exported text
	 * @throws IOException
	 */
	void export(LogicalModel model, OutputStream out) throws IOException;

	/**
	 * Import a file in this format and build a logical model for it.
	 * 
	 * @param f the file to import
	 * @return a new LogicalModel containing the imported data.
	 * @throws IOException
	 */
	LogicalModel importFile(File f) throws IOException;
	
}
