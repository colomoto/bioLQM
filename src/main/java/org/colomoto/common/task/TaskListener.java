package org.colomoto.common.task;

/**
 * Listen to a background task.
 *
 * @author Aurelien Naldi
 */
public interface TaskListener {

	/**
	 * Notifies a task update (finished or canceled)
	 * @param task the followed task
	 */
    void taskUpdated(Task task);
}
