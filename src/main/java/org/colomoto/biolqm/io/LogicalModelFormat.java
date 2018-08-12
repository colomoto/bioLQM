package org.colomoto.biolqm.io;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.Service;

import java.io.IOException;

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
	 * Does this format supports loading models?
	 * @return true if this model loading is implemented.
	 */
	boolean canLoad();


	/**
	 * Get a ready-to-use model loader from this format.
	 *
	 * @param streams an object providing input streams on demand, allowing to load from one or multiple files
	 * @throws UnsupportedOperationException if this format does not support loading
	 * @return a new model loader, or null if loading is not supported;
	 */
	default ModelLoader getLoader(StreamProvider streams) {
		throw new UnsupportedOperationException("Saving model is not implemented for format " + getID());
	}

	/**
	 * Get a ready-to-use model exporter to this format.
	 *
	 * @param model the model to export
	 * @param streams an object providing output streams on demand for saving to one or multiple files
	 * @throws UnsupportedOperationException if this format does not support export
	 */
	default ModelExporter getExporter(LogicalModel model, StreamProvider streams) {
		throw new UnsupportedOperationException("Saving model is not implemented for format " + getID());
	}

	/**
	 * Export a logical model to this format.
	 * 
	 * @param model the model to export
	 * @param streams an object providing output streams on demand for saving to one or multiple files
	 * @throws UnsupportedOperationException if this format does not support export
	 * @throws Exception if writing failed
	 */
	default void export(LogicalModel model, StreamProvider streams) throws Exception {
		getExporter(model, streams).call();
    }

	/**
	 * Load a file in this format and build a logical model for it.
	 * 
	 * @param streams an object providing input streams on demand, allowing to load from one or multiple files
	 * @return a new LogicalModel containing the imported data.
	 * @throws UnsupportedOperationException if this format does not support loading
	 * @throws Exception if loading failed
	 */
	default LogicalModel load(StreamProvider streams) throws Exception {
		return getLoader(streams).call();
    }

}
