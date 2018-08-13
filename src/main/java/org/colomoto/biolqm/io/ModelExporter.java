package org.colomoto.biolqm.io;

import org.colomoto.common.task.Task;

import java.io.File;

public interface ModelExporter extends Task<Boolean> {

    void setDestination(StreamProvider streams);

    default void setDestination(File f) {
        setDestination( new StreamProviderFileImpl(f));
    }

    default void setDestination(String filename) {
        setDestination( new StreamProviderFileImpl(filename));
    }

}
