package org.colomoto.logicalmodel.io;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import org.colomoto.logicalmodel.LogicalModel;

public interface FormatMultiplexer<T extends Enum> extends LogicalModelFormat {

	/**
	 * Export a logical model to a specific subformat.
	 * 
	 * @param model the model to export
	 * @param out an outputstream to write the exported text
	 * @param subformat the identifier of the subformat
	 * @throws IOException
	 */
	void export(LogicalModel model, OutputStream out, T subformat) throws IOException;

	/**
	 * Import a file in a specific subformat and build a logical model for it.
	 * 
	 * @param f the file to import
	 * @param subformat the identifier of the subformat
	 * @return a new LogicalModel containing the imported data.
	 * @throws IOException
	 */
	LogicalModel importFile(File f, T subformat) throws IOException;

	T[] getSubformats();
	
	T getSubformat(String id);
}
