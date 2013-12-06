package org.colomoto.common.task;

/**
 * Helper class implementing the boring part of a Task.
 *
 * Implementors should check the canceled field to detect interruptions.
 *
 * @author Aurelien Naldi
 */
abstract public class AbstractTask<T> extends Thread implements Task<T> {

    protected boolean canceled = false;
    private TaskListener listener = null;

    private TaskStatus status = TaskStatus.STOPPED;

    private T result = null;

    /**
     * Placeholder for the actual implementation.
     *
     * @return the result of the computation
     */
    abstract protected T doGetResult();

    @Override
    public T getResult() {
        return result;
    }

    @Override
    public TaskStatus getStatus() {
        return status;
    }

    @Override
    public synchronized T call() {
        if (this.status == TaskStatus.RUNNING) {
            throw new RuntimeException("Should not be able to call a running task");
        }

        // run the task
        this.result = null;
        this.canceled = false;
        this.status = TaskStatus.RUNNING;

        this.result = doGetResult();
        if (canceled) {
            result = null;
            this.status = TaskStatus.CANCELED;
        } else {
            this.status = TaskStatus.FINISHED;
        }

        return result;
    }

    @Override
    public void cancel() {
        if (status == TaskStatus.RUNNING) {
            this.canceled = true;
        }
    }

    @Override
    public void background(TaskListener listener) {
        this.listener = listener;
        this.start();
    }

    @Override
    public void run() {
        T result = call();

        if (listener != null) {
            listener.taskUpdated(this);
        }
    }
}
