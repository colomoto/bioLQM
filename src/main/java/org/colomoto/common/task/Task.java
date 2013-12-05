package org.colomoto.common.task;

import java.util.concurrent.Callable;

/**
 * Simple task definition.
 * A task can either be blocking (it will then return the result directly) or launched in background.
 *
 * @author Aurelien Naldi
 */
public interface Task<T> extends Callable<T> {

    /**
     * Retrieve the result of the task.
     * This will NOT run the task, use <code>call()</code> to run and retrieve.
     *
     * @return the result of the task (null if stopped of canceled).
     */
    T getResult();

    /**
     * Retrieve the status of the task.
     *
     * @return the current status
     */
    TaskStatus getStatus();

    /**
     * Mark the task as canceled.
     * Depending on the implementation,it may not stop the task immediately,
     * but when the task will next check for the canceled status.
     * It will thus not work with stuck tasks.
     */
    void cancel();

    /**
     * Run the task in a separate thread.
     * The listener will be notified when the task is finished.
     *
     * @param listener
     */
    void background(TaskListener listener);
}
