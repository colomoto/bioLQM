package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.tool.simulation.DeterministicUpdater;

/**
 * Updater for the synchronous scheme: all possible changes are applied in a single successor
 * 
 * @author Aurelien Naldi
 */
public class SynchronousUpdater extends AbstractSingleSuccessorUpdater implements DeterministicUpdater {

	/**
	 * Create a new synchronous updater
	 * 
	 * @param model
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
