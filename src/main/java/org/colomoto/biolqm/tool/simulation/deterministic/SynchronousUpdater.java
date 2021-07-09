
package org.colomoto.biolqm.tool.simulation.deterministic;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.BaseUpdater;

/**
 * Updater for the synchronous scheme: all possible changes are applied in a single successor
 * 
 * @author Aurelien Naldi
 */
public class SynchronousUpdater extends BaseUpdater implements DeterministicUpdater {

	public static String name = "Synchronous";

	/**
	 * Create a new synchronous updater
	 * 
	 * @param model the model for which the random is constructed
	 */
	public SynchronousUpdater(LogicalModel model) {
		super(model);
	}

	
	@Override
	public byte[] getSuccessor(byte[] state) {
		byte[] nextstate = null;
		for (int idx=0 ; idx < size ; idx++) {
			int change = nodeChange(state, idx);
			// change as -1/+1 ...
            nextstate = update(state, idx, change, nextstate);
		}

		return nextstate;
	}
	
	@Override
	public String getUpdaterName() {
		return getUpdaterClassName();
	}

	public static String getUpdaterClassName() {
		return name;
	}
	
}
