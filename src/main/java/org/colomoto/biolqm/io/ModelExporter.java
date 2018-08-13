package org.colomoto.biolqm.io;

import org.colomoto.common.task.Task;

import java.io.File;

/**
 * Model exporters are tasks dedicated to exporting a model in a specific format.
 *
 * <ul>
 *     <li>Use the call() method to wait for the model export.</li>
 *     <li>Use the background(listener) method to export it in the background.</li>
 * </ul>
 *
 * @author Aurelien Naldi
 */
public interface ModelExporter extends Task<Boolean> {

    /**
     * Define the destination at which the model will be exported.
     * One of the setDestination methods MUST be called before calling the loader.
     * @param streams a ready-to-use StreamProvider
     */
    void setDestination(StreamProvider streams);

    /**
     * Define the destination at which the model will be exported.
     * A StreamProvider based on this file will be created.
     * One of the setDestination methods MUST be called before calling the loader.
     * @param f the source file
     */
    default void setDestination(File f) {
        setDestination( StreamProvider.create(f));
    }

    /**
     * Define the destination at which the model will be exported.
     * A StreamProvider based on this file name will be created.
     * One of the setDestination methods MUST be called before calling the loader.
     * @param filename the filename of the source
     */
    default void setDestination(String filename) {
        setDestination( StreamProvider.create(filename));
    }

}
