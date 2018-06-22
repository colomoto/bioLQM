package org.colomoto.biolqm.tool.simulation.deterministic;

import org.colomoto.biolqm.NodeInfo;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * A simple simulation engine for deterministic updaters (with a single successor).
 * It will stop when reaching a stable state, a keeps a partial memory to detect
 * cycles up to the specified length.
 * If no attractor is identified, it stops after reaching the max number of iterations.
 *
 * @author Aurelien Naldi
 */
public class DeterministicSimulation implements Iterable<byte[]> {

    private final DeterministicUpdater updater;
    private final byte[] init;

    private final int max_steps;
    private final int length;

    public DeterministicSimulation(DeterministicUpdater updater, byte[] init) {
        this(updater, init, 10000);
    }
    public DeterministicSimulation(DeterministicUpdater updater, byte[] init, int max_steps) {
        this(updater, init, 100, max_steps);
    }

    public DeterministicSimulation(DeterministicUpdater updater, byte[] init, int length, int max_steps) {
        this.updater = updater;
        this.init = init;
        this.length = length;
        this.max_steps = max_steps;
    }

    public List<NodeInfo> getNodes() {
        return updater.getModel().getComponents();
    }

    @Override
    public Iterator<byte[]> iterator() {
        return new StateIterator(init, updater, length, max_steps);
    }
}

class StateIterator implements Iterator<byte[]> {

    private byte[] state;
    private final byte[][] buffer;
    private int buffer_pos;

    private final DeterministicUpdater updater;
    private int steps;

    public StateIterator(byte[] state, DeterministicUpdater updater, int length, int max_steps) {
        this.state = state;
        this.updater = updater;
        this.steps = max_steps;

        this.buffer = new byte[length][];
        this.buffer_pos = 0;
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

        // Lookup the previous state in the buffer
        for (byte[] prev: buffer) {
            if (prev == null) {
                break;
            }
            if ( Arrays.equals(state,prev) ) {
                byte[] ret = state;
                state = null;
                return ret;
            }
        }

        // add the previous state to the circular buffer
        this.buffer[buffer_pos++] = state;
        if (this.buffer_pos >= this.buffer.length) {
            this.buffer_pos = 0;
        }

        byte[] ret = state;
        if (steps < 1) {
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