package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.LogicalModel;

/**
 * Updater for the synchronous scheme: all possible changes are applied in a single successor
 * 
 * @author Aurelien Naldi
 */
public class SynchronousUpdater extends AbstractUpdater {

	/**
	 * Create a new synchronous updater
	 * 
	 * @param model
	 */
	public SynchronousUpdater(LogicalModel model) {
		super(model);
	}

	@Override
	public byte[] buildNext() {
		byte[] nextstate = null;
		while (status < size) {
			int change = nodeChange(state, status);
			if (change != 0) {
				if (nextstate == null) {
					nextstate = state.clone();
				}
				nextstate[status] += change;
			}
			status++;
		}
		return nextstate;
	}

}
