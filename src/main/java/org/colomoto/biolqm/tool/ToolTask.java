package org.colomoto.biolqm.tool;

import org.colomoto.common.task.Task;

public interface ToolTask<R> extends Task<R> {

    void cli();
}
