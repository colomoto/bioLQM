package org.colomoto.biolqm.tool.simulation.random;

import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.deterministic.DeterministicUpdater;

import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * A simple simulation engine for random updaters.
 * It will stop when reaching a stable state, but has otherwise no memory and will
 * not detect cycles, just stop after reaching a limit on the number of iterations.
 *
 * @author Aurelien Naldi
 */
public class RandomWalkSimulation implements Iterable<byte[]> {

    private final RandomUpdater updater;
    private final byte[] init;

    private final int max_steps;

    public RandomWalkSimulation(RandomUpdater updater, byte[] init, int max_steps) {
        this.updater = updater;
        this.init = init;
        this.max_steps = max_steps;
    }

    public List<NodeInfo> getNodes() {
        return updater.getModel().getComponents();
    }

    @Override
    public Iterator<byte[]> iterator() {
        return new StateIterator(init, updater, max_steps);
    }
}

class StateIterator implements Iterator<byte[]> {

    private byte[] state;
    private final RandomUpdater updater;
    private int steps;

    public StateIterator(byte[] state, RandomUpdater updater, int max_steps) {
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
        if (steps < 1) {
            state = null;
        } else {
            state = updater.pickSuccessor(state);
        }
        steps--;
        return ret;
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }
}