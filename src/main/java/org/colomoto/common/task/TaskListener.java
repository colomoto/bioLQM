package org.colomoto.common.task;

/**
 * Listen to a background task.
 *
 * @author Aurelien Naldi
 */
public interface TaskListener {

    void taskUpdated(Task task);
}
