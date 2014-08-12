package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.tool.simulation.LogicalModelUpdater;

import java.util.AbstractList;
import java.util.List;

/**
 * Base class for all updaters.
 *
 * @author Aurelien Naldi
 */
abstract public class BaseUpdater implements LogicalModelUpdater {

    private static final List<byte[]> EMPTYSUCCESSOR = new EmptySuccessorList();

    protected final LogicalModel model;
    protected final int size;

    public BaseUpdater(LogicalModel model) {
        this.model = model;
        this.size = model.getNodeOrder().size();
    }

    /**
     * Test if a component is ready to change its state.
     *
     * @param index the index of the component
     * @return +1, or -1 for pending increase or decrease, 0 if it can not change state.
     */
    protected int nodeChange(byte[] state, int index) {
        byte curState = state[index];
        byte nextState = model.getTargetValue(index, state);

        // now see if the node is willing to change it's state
        if (nextState > curState){
            return 1;
        } else if (nextState < curState){
            return -1;
        }
        return 0;
    }

    public static List<byte[]> getEmptySuccessors() {
        return EMPTYSUCCESSOR;
    }

    public static List<byte[]> getSingleSuccessors(byte[] next) {
        if (next == null) {
            return EMPTYSUCCESSOR;
        }
        return new SingleStateList(next);
    }

    public static List<byte[]> getSuccessors(byte[][] nexts) {
        if (nexts == null || nexts.length == 0) {
            return EMPTYSUCCESSOR;
        }

        if (nexts.length == 1) {
            return new SingleStateList(nexts[0]);
        }

        return new StateList(nexts);
    }

    public LogicalModel getModel() {
        return model;
    }

    /**
     * Create or update the next state.
     *
     * @param state the base state
     * @param idx index of the position to update
     * @param change change for this position
     * @param next a previously created next state (null if it is still unchanged)
     *
     * @return an updated next state, cloned from the reference state if needed
     */
    protected byte[] update(byte[] state, int idx, int change, byte[] next) {
        if (change == 0) {
            return next;
        }

        if (next == null) {
            next = state.clone();
        }
        next[idx] += change;

        return next;
    }

}


/*
 * simple representation of an empty list of successors
 */
class EmptySuccessorList extends AbstractList<byte[]> {

    @Override
    public byte[] get(int i) {
        return null;
    }

    @Override
    public int size() {
        return 0;
    }
}


class SingleStateList extends AbstractList<byte[]> {

    private final byte[] next;

    public SingleStateList(byte[] state) {
        this.next = state;
    }

    @Override
    public byte[] get(int i) {
        return next;
    }

    @Override
    public int size() {
        return 1;
    }
}

class StateList extends AbstractList<byte[]> {

    private final byte[][] states;

    public StateList(byte[][] states) {
        this.states = states;
    }

    @Override
    public byte[] get(int i) {
        return states[i];
    }

    @Override
    public int size() {
        return states.length;
    }
}
