package org.colomoto.biolqm.io;

import org.colomoto.biolqm.service.ModelTask;

import java.io.File;

public interface ModelLoader extends ModelTask {

    void setSource(StreamProvider streams);

    default void setSource(File f) {
        setSource( new StreamProviderFileImpl(f));
    }

    default void setSource(String filename) {
        setSource( new StreamProviderFileImpl(filename));
    }
}
