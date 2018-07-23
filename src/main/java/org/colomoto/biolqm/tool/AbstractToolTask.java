package org.colomoto.biolqm.tool;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.common.task.AbstractTask;

abstract public class AbstractToolTask<R> extends AbstractTask<R> implements ToolTask<R> {

    public final LogicalModel model;

    public AbstractToolTask(LogicalModel model) {
        this.model = model;
    }
}
