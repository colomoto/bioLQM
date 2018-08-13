package org.colomoto.biolqm.io;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.common.task.AbstractTask;

/**
 * Base class for model loader.
 *
 * It stores the StreamProvider object, future versions will add more common methods.
 *
 * @author Aurelien Naldi
 */
public abstract class BaseLoader extends AbstractTask<LogicalModel> implements ModelLoader {

    protected StreamProvider streams = null;

    @Override
    public void setSource(StreamProvider streams) {
        this.streams = streams;
    }

}
