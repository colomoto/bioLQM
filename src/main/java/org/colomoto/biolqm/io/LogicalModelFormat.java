package org.colomoto.biolqm.io;

import java.io.IOException;

import org.colomoto.MultivaluedSupport;
import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.Service;

/**
 * Description of an available file format.
 * Implement this interface to integrate new formats in the conversion tool.
 * 
 * @author Aurelien Naldi
 */
public interface LogicalModelFormat extends Service {

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
	 * Precise type of support for multivalued models.
	 *
	 * @return the type of support: native, booleanized or none
	 */
	MultivaluedSupport getMultivaluedSupport();
	
	/**
	 * Export a logical model to this format.
	 * 
	 * @param model the model to export
	 * @param outputProvider an object providing output streams on demand for saving to one or multiple files
	 * @throws UnsupportedOperationException if this format does not support export
	 * @throws IOException if writing failed
	 */
	void export(LogicalModel model, OutputStreamProvider outputProvider) throws IOException, UnsupportedOperationException;

	/**
	 * Load a file in this format and build a logical model for it.
	 * 
	 * @param inputProvider an object providing input streams on demand, allowing to load from one or multiple files
	 * @return a new LogicalModel containing the imported data.
	 * @throws UnsupportedOperationException if this format does not support loading
	 * @throws IOException if loading failed
	 */
	LogicalModel load(InputStreamProvider inputProvider) throws IOException;
	
}
