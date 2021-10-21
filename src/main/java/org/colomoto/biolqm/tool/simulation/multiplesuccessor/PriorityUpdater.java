package org.colomoto.biolqm.tool.simulation.multiplesuccessor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.BaseUpdater;
import org.colomoto.biolqm.tool.simulation.LogicalModelUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.DeterministicUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.grouping.PCRankGroupsVars;
import org.colomoto.biolqm.tool.simulation.grouping.PCRankGroupsVars.RankedClass;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWithRates;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdaterWrapper;

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

	private PCRankGroupsVars pclist; 
//	private final boolean isComplete = false;
	private static final String name = "Priorities";

	public PriorityUpdater(LogicalModel model, String setup) {
		this((setup == null) ? new PCRankGroupsVars(model) : new PCRankGroupsVars(model, setup));
	}

	public PriorityUpdater(PCRankGroupsVars pcs) {
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
				LogicalModelUpdater groupUpdater = this.pclist.getUpdater(p, g);
				
//				if (this.isComplete) {
//					lTmpSucc.addAll(this.computeSuccStates(groupUpdater, lTmpSucc));
//				}
				lTmpSucc.addAll(this.computeSuccStates(groupUpdater, currStates));
			}

			// stop if previous block already generated successors
			if (!lTmpSucc.isEmpty()) {
				currStates = lTmpSucc;
				break;
			}
		}
		return currStates;
	}

	private List<byte[]> computeSuccStates(LogicalModelUpdater groupUpdater, List<byte[]> currStates) {
		List<byte[]> lTmp = new ArrayList<>();
		for (byte[] currState : currStates) {
			List<byte[]> succState = new ArrayList<byte[]>();
			
			if(groupUpdater instanceof AbstractMultipleSuccessorUpdater) {
				succState = ((AbstractMultipleSuccessorUpdater) groupUpdater).getSuccessors(currState);
			} else if (groupUpdater instanceof DeterministicUpdater) {
				byte[] singleSucc = ((DeterministicUpdater) groupUpdater).getSuccessor(currState);
				succState.add(singleSucc);
			} else if (groupUpdater instanceof RandomUpdater) {
				byte[] singleSucc = ((RandomUpdater) groupUpdater).pickSuccessor(currState);
				succState.add(singleSucc);
			}
			
			for (byte[] state : succState)
				lTmp = this.addSuccessor(lTmp, state);
		}
		return lTmp;
	}
	
	public void makeRetroCompatible() {
		this.pclist = this.pclist.cloneRetroCompatible();
	}

	@Override
	public String getUpdaterName() {
		return name;
	}


}