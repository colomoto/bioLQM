package org.colomoto.biolqm.tool.simulation.updater;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.DeterministicUpdater;

/**
 * Updater for the synchronous scheme: all possible changes are applied in a single successor
 * 
 * @author Aurelien Naldi
 */
public class SynchronousUpdater extends AbstractDeterministicUpdater implements DeterministicUpdater {

	/**
	 * Create a new synchronous updater
	 * 
	 * @param model the model for which the updater is constructed
	 */
	public SynchronousUpdater(LogicalModel model) {
		super(model);
	}

	@Override
	public byte[] getSuccessor(byte[] state) {
		byte[] nextstate = null;
		for (int idx=0 ; idx < size ; idx++) {
			int change = nodeChange(state, idx);
            nextstate = update(state, idx, change, nextstate);
		}

		return nextstate;
	}

}
