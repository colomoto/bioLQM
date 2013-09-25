package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.LogicalModel;

/**
 * Updater for the asynchronous scheme: all possible changes are applied separately.
 * It returns all successors according to the order of components in the model.
 * 
 * @author Aurelien Naldi
 */
public class AsynchronousUpdater extends AbstractUpdater {

	/**
	 * Create a new asynchronous updater.
	 * 
	 * @param model
	 */
	public AsynchronousUpdater(LogicalModel model) {
		super(model);
	}

	@Override
	public byte[] buildNext() {
		while (status < size) {
			int change = nodeChange(state, status);
			if (change != 0) {
				byte[] nextstate = state.clone();
				nextstate[status] += change;
				status++;
				return nextstate;
			}
			status++;
		}
		return null;
	}

}
