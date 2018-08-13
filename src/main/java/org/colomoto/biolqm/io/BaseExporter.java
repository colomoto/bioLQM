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

    protected StreamProvider streams = null;
    protected final LogicalModel model;

    public BaseExporter(LogicalModel model) {
        this.model = model;
    }

    public Boolean performTask() {
        try {
            export();
        } catch (IOException e) {
            return Boolean.FALSE;
        }
        return Boolean.TRUE;
    }

    @Override
    public void setDestination(StreamProvider streams) {
        this.streams = streams;
    }

    protected abstract void export() throws IOException;
}
