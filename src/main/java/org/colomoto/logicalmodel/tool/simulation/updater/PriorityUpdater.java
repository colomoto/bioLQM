package org.colomoto.logicalmodel.tool.simulation.updater;

import java.util.List;

import org.colomoto.logicalmodel.LogicalModel;

/**
 * Draft for a priority updater: components are grouped in groups, which can be
 * updated synchronously or asynchronously. Only components from the first group
 * with updated components will be taken into account
 *
 * @author Aurelien Naldi
 * @author Pedro T. Monteiro
 */
// April 2015 - Pedro added support for split transitions.
public class PriorityUpdater extends AbstractMultipleSuccessorUpdater {

	private final PriorityClasses priorities;

	public PriorityUpdater(LogicalModel model, PriorityClasses pcs) {
		super(model);
		this.priorities = pcs;
	}

	@Override
	public List<byte[]> getSuccessors(byte[] state) {
		/*
		 * this the state that will be returned here it is initialized to be
		 * current basal state
		 */
		byte[] nextstate = null;
		List<byte[]> successors = null;

		// Iterate over the priority classes
		for (int p = 0; p < this.priorities.size(); p++) {

			// stop if previous block already generated successors
			if (successors != null) {
				break;
			}

			int[] pcVars = this.priorities.getClass(p);
			// Update the nodes in the current priority class
			for (int i = 0; i < pcVars.length; i += 2) {
				int idx = pcVars[i];
				int change = nodeChange(state, idx);
				if (change != 0 && (change == pcVars[i + 1] || pcVars[i+1] == 0)) {
					nextstate = update(state, idx, change, nextstate);
					if (!this.priorities.isSync(p)) {
						successors = addSuccessor(successors, nextstate);
						nextstate = null;
					}
				}
			}
			if (nextstate != null) {
				successors = addSuccessor(successors, nextstate);
				nextstate = null;
			}
		}
		return successors;
	}

}