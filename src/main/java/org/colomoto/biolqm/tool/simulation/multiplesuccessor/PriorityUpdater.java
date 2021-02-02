package org.colomoto.biolqm.tool.simulation.multiplesuccessor;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping.RankedClass;

/**
 * Draft for a priority updater: components are grouped in groups, which can be
 * updated synchronously or asynchronously. Only components from the first group
 * with updated components will be taken into account
 *
 * @author Aurelien Naldi
 * @author Pedro T. Monteiro
 * @author Pedro L. Varela
 */
// April 2015 - Pedro added support for split transitions.
public class PriorityUpdater extends AbstractMultipleSuccessorUpdater {

	private final ModelGrouping pclist; 
	private final boolean isComplete = false;
	private final String name = "Priority Updater";

	public PriorityUpdater(LogicalModel model, String setup) {
		this((setup == null) ? new ModelGrouping(model) : new ModelGrouping(model, setup));
	}

	public PriorityUpdater(ModelGrouping pcs) {
		super(pcs.getModel());
		this.pclist = pcs;
	}

	@Override
	public List<byte[]> getSuccessors(byte[] state) {
		/*
		 * this the state that will be returned here it is initialized to be current
		 * basal state
		 */
		List<byte[]> currStates = addSuccessor(null, state);

		// Iterate over the priority classes
		for (int p = 0; p < this.pclist.size(); p++) {
			RankedClass pc = this.pclist.getClass(p);

			List<byte[]> lTmpSucc = new ArrayList<>();
			for (int g = 0; g < pc.size(); g++) {
				
				// pcVars
				// the individual groups are the classes 
				int[] pcVars = pc.getGroupValues(g);

				if (this.isComplete) {
					lTmpSucc.addAll(this.computeSuccStates(pcVars, lTmpSucc));
				}
				lTmpSucc.addAll(this.computeSuccStates(pcVars, currStates));
			}

			// stop if previous block already generated successors
			if (currStates != null) {
				currStates = lTmpSucc;
				break;
			}
		}
		return currStates;
	}

	private List<byte[]> computeSuccStates(int[] pcVars, List<byte[]> currStates) {
		List<byte[]> lTmp = new ArrayList<>();
		for (byte[] currState : currStates) {
			byte[] succState = null;

			// Update the nodes in the current priority class
			for (int i = 0; i < pcVars.length; i += 2) {
				int idx = pcVars[i];
				int change = nodeChange(currState, idx);
				if (change != 0 && (change == pcVars[i + 1] || pcVars[i + 1] == 0)) {
					succState = update(currState, idx, change, succState);
				}
			}
			lTmp = addSuccessor(lTmp, succState);
		}
		return lTmp;
	}

	@Override
	public String getUpdaterName() {
		return this.name;
	}

}