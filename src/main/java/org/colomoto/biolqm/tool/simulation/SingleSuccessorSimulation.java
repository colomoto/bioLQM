package org.colomoto.biolqm.tool.simulation;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * A simple simulation engine for updaters with a single successor.
 * It will stop when reaching a stable state, but has otherwise no memory and will
 * not detect cycles, just stop after reaching a limit on the number of iterations.
 *
 * @author Aurelien Naldi
 */
public class SingleSuccessorSimulation implements Iterable<byte[]> {

    private final DeterministicUpdater updater;
    private final byte[] init;

    private final int max_steps;

    public SingleSuccessorSimulation(DeterministicUpdater updater, byte[] init, int max_steps) {
        this.updater = updater;
        this.init = init;
        this.max_steps = max_steps;
    }

    @Override
    public Iterator<byte[]> iterator() {
        return new StateIterator(init, updater, max_steps);
    }
}

class StateIterator implements Iterator<byte[]> {

    private byte[] state;
    private final DeterministicUpdater updater;
    private int steps;

    public StateIterator(byte[] state, DeterministicUpdater updater, int max_steps) {
        this.state = state;
        this.updater = updater;
        this.steps = max_steps;

    }

    @Override
    public boolean hasNext() {
        return state != null;
    }

    @Override
    public byte[] next() {
        if (state == null) {
            throw new NoSuchElementException();
        }

        byte[] ret = state;
        if (steps < 0) {
            state = null;
        } else {
            state = updater.getSuccessor(state);
        }
        steps--;
        return ret;
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }
}