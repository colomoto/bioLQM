package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.LogicalModel;

/**
 * Updater for the sequential scheme: all components are updated one after the other in a single successor.
 * Unlike in the synchronous method, the next updated component takes into account the new value of the previous ones.
 * 
 * @author Aurelien Naldi
 */
public class SequentialUpdater extends AbstractUpdater {

	private final int[] order;
	
	/**
	 * Create a new synchronous updater, using the default order
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
	 * Create a new synchronous updater, using a custom order
	 * 
	 * @param model
	 */
	public SequentialUpdater(LogicalModel model, int[] order) {
		super(model);
		this.order = order;
	}

	@Override
	public byte[] buildNext() {
		// only build one successor
		if (status != 0) {
			return null;
		}
		status = 1;
		
		// create the sequential successor
		byte[] nextstate = state.clone();
		boolean changed = false;
		for (int idx: order) {
			int change = nodeChange(nextstate, idx);
			if (change != 0) {
				nextstate[idx] += change;
				changed = true;
			}
		}
		
		if (changed) {
			return nextstate;
		}
		return null;
	}

}
