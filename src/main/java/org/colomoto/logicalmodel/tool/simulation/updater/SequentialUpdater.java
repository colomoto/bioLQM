package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.tool.simulation.DeterministicUpdater;

/**
 * Updater for the sequential scheme: all components are updated one after the other in a single successor.
 * Unlike in the synchronous method, the next updated component takes into account the new value of the previous ones.
 * 
 * @author Aurelien Naldi
 */
public class SequentialUpdater extends AbstractSingleSuccessorUpdater implements DeterministicUpdater {

	private final int[] order;
	
	/**
	 * Create a new sequential updater, using the default order
	 * 
	 * @param model
	 */
	public SequentialUpdater(LogicalModel model) {
		super(model);
		this.order = new int[size];
		for (int i=0 ; i<size ; i++) {
			order[i] = i;
		}
	}
	
	/**
	 * Create a new sequential updater, using a custom order
	 * 
	 * @param model
	 */
	public SequentialUpdater(LogicalModel model, int[] order) {
		super(model);
		this.order = order;
	}

	@Override
    public byte[] getSuccessor(byte[] state) {
		// create the sequential successor
        byte[] refstate = state;
		byte[] nextstate = null;
		for (int idx: order) {
            int change = nodeChange(refstate, idx);
            if (change != 0) {
                nextstate = update(refstate, idx, change, nextstate);
                refstate = nextstate;
			}
		}
		
        return nextstate;
	}

}
