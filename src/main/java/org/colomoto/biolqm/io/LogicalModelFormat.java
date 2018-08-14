package org.colomoto.biolqm.io;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.service.Service;

import java.io.File;

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
     * Get a new model loader from this format.
     *
     * @throws UnsupportedOperationException if this format does not support loading
     * @return a new model loader
     */
    default ModelLoader getLoader() {
        throw new UnsupportedOperationException("Saving model is not implemented for format " + getID());
    }

    /**
     * Get a new model exporter to this format.
     *
     * @param model the model to export
     * @return a new model exporter
     * @throws UnsupportedOperationException if this format does not support export
     */
    default ModelExporter getExporter(LogicalModel model) {
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
        ModelExporter exporter = getExporter(model);
        exporter.setDestination(streams);
        exporter.call();
    }

    /**
     * Export a logical model to this format.
     *
     * @param model the model to export
     * @param destination the destination file. Some formats may generate multiple output files based on it's path
     * @throws UnsupportedOperationException if this format does not support export
     * @throws Exception if writing failed
     */
    default void export(LogicalModel model, File destination) throws Exception {
        ModelExporter exporter = getExporter(model);
        exporter.setDestination(destination);
        exporter.call();
    }

    /**
     * Export a logical model to this format.
     *
     * @param model the model to export
     * @param filename the destination file. Some formats may generate multiple output files based on it's path
     * @throws UnsupportedOperationException if this format does not support export
     * @throws Exception if writing failed
     */
    default void export(LogicalModel model, String filename) throws Exception {
        ModelExporter exporter = getExporter(model);
        exporter.setDestination(filename);
        exporter.call();
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
        ModelLoader loader = getLoader();
        loader.setSource(streams);
        return loader.call();
    }

    /**
     * Load a file in this format and build a logical model for it.
     *
     * @param source the source file. Some formats may read multiple files
     * @return a new LogicalModel containing the imported data.
     * @throws UnsupportedOperationException if this format does not support loading
     * @throws Exception if loading failed
     */
    default LogicalModel load(File source) throws Exception {
        ModelLoader loader = getLoader();
        loader.setSource(source);
        return loader.call();
    }

    /**
     * Load a file in this format and build a logical model for it.
     *
     * @param filename the source file name. Some formats may read multiple files
     * @return a new LogicalModel containing the imported data.
     * @throws UnsupportedOperationException if this format does not support loading
     * @throws Exception if loading failed
     */
    default LogicalModel load(String filename) throws Exception {
        ModelLoader loader = getLoader();
        loader.setSource(filename);
        return loader.call();
    }

}
