package org.colomoto.biolqm.tool.simulation.updater;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.LogicalModelUpdater;

/**
 * Base class for all updaters.
 *
 * @author Aurelien Naldi
 */
abstract public class BaseUpdater implements LogicalModelUpdater {

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
