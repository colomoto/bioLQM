package org.colomoto.biolqm.tool;

import org.colomoto.common.task.Task;

public interface ToolTask<R> extends Task<R> {

    default void setParameters(String[] parameters) {

    }

    default void setParameters(String parameters) {
        String[] t_parameters = parameters.split(" ");
        setParameters(t_parameters);
    }

    void cli();
}
