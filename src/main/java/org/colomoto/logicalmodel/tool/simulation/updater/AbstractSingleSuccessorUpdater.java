package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.tool.simulation.SingleSuccessorUpdater;

/**
 * Base class to build deterministic updaters
 *
 * @author Aurelien Naldi
 */
public abstract class AbstractSingleSuccessorUpdater implements SingleSuccessorUpdater {

    protected final LogicalModel model;
    protected final int size;


    public AbstractSingleSuccessorUpdater(LogicalModel model) {
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

}
