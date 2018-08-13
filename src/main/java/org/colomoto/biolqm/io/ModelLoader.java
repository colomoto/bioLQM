package org.colomoto.biolqm.io;

import org.colomoto.biolqm.service.ModelTask;

import java.io.File;

/**
 * Model loaders are tasks dedicated to loading a model from a specific format.
 *
 * <ul>
 *     <li>Use the call() method to retrieve the loaded model directly.</li>
 *     <li>Use the background(listener) method to load it in the background.</li>
 * </ul>
 *
 * @author Aurelien Naldi
 */
public interface ModelLoader extends ModelTask {

    /**
     * Define the source from which the model will be loaded.
     * One of the setSource methods MUST be called before calling the loader.
     * @param streams a ready-to-use StreamProvider
     */
    void setSource(StreamProvider streams);

    /**
     * Define the source file from which the model will be loaded.
     * A StreamProvider based on this file will be created.
     * One of the setSource methods MUST be called before calling the loader.
     * @param f the source file
     */
    default void setSource(File f) {
        setSource( new StreamProviderFileImpl(f));
    }

    /**
     * Define the source from which the model will be loaded.
     * A StreamProvider based on this file name will be created.
     * One of the setSource methods MUST be called before calling the loader.
     * @param filename the filename of the source
     */
    default void setSource(String filename) {
        setSource( new StreamProviderFileImpl(filename));
    }
}
