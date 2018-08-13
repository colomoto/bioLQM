package org.colomoto.biolqm.io;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.common.task.AbstractTask;

import java.io.IOException;

/**
 * Base class for model exporters.
 *
 * It stores the model and StreamProvider objects, future versions will add more common methods.
 *
 * @author Aurelien Naldi
 */
public abstract class BaseExporter extends AbstractTask<Boolean> implements ModelExporter {

    protected final StreamProvider streams;
    protected final LogicalModel model;

    public BaseExporter(LogicalModel model, StreamProvider streams) {
        this.model = model;
        this.streams = streams;
    }

    public Boolean performTask() {
        try {
            export();
        } catch (IOException e) {
            return Boolean.FALSE;
        }
        return Boolean.TRUE;
    }

    protected abstract void export() throws IOException;
}
