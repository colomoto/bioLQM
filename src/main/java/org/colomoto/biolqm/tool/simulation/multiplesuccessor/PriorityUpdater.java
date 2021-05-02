package org.colomoto.biolqm.tool.simulation.multiplesuccessor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.BaseUpdater;
import org.colomoto.biolqm.tool.simulation.LogicalModelUpdater;
import org.colomoto.biolqm.tool.simulation.deterministic.SynchronousUpdater;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping;
import org.colomoto.biolqm.tool.simulation.grouping.ModelGrouping.RankedClass;
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

	private ModelGrouping pclist; 
	private final boolean isComplete = false;
	private static final String name = "Priorities";

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
//				 the individual groups are the classes 
			
				LogicalModelUpdater groupUpdater = this.pclist.getUpdater(p, g);
				
				if (this.isComplete) {
					lTmpSucc.addAll(this.computeSuccStates(groupUpdater, lTmpSucc));
				}
				lTmpSucc.addAll(this.computeSuccStates(groupUpdater, currStates));
			}

			// stop if previous block already generated successors
			if (currStates != null && !lTmpSucc.isEmpty()) {
				currStates = lTmpSucc;
				break;
			}
		}
		return currStates;
	}

	private List<byte[]> computeSuccStates(LogicalModelUpdater groupUpdater, List<byte[]> currStates) {
		List<byte[]> lTmp = new ArrayList<>();
		for (byte[] currState : currStates) {
			byte[] succState = null;

			if (groupUpdater instanceof RandomUpdaterWithRates) {
				succState = ((RandomUpdaterWithRates) groupUpdater).pickSuccessor(currState);
				
			} else if (groupUpdater instanceof SynchronousUpdater){
				succState = ((SynchronousUpdater) groupUpdater).getSuccessor(currState);

			} else if (groupUpdater instanceof RandomUpdaterWrapper) {
				succState = ((RandomUpdaterWrapper) groupUpdater).pickSuccessor(currState);
			}
			
			lTmp = this.addSuccessor(lTmp, succState);
		}
		return lTmp;
	}
	
	public void makeRetroCompatible() {
		this.pclist = this.pclist.cloneRetroCompatible();
	}

	@Override
	public String getUpdaterName() {
		return getUpdaterClassName();
	}

	public static String getUpdaterClassName() {
		return name;
	}

}