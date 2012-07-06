package org.colomoto.logicalmodel;

import java.util.List;

import org.colomoto.mddlib.MDDManager;

/**
 * Build and consult regulatory relations between variables in a Logical Model.
 * This will assemble for each component, the list of its regulators,
 * and for each core components the list of its targets.
 * 
 * @author Aurelien Naldi
 */
public class ConnectivityMatrix {

	private final int[][] coreRegulators, extraRegulators;
	private final int[][] coreTargets, extraTargets;

	public ConnectivityMatrix(LogicalModel model) {
		MDDManager ddmanager = model.getMDDManager();
		
		// fill in regulators lists
		coreRegulators = fillRegulators(ddmanager, model.getLogicalFunctions());
		extraRegulators = fillRegulators(ddmanager, model.getExtraLogicalFunctions());
		
		// fill target lists
		coreTargets = fillTargets(coreRegulators.length, coreRegulators);
		extraTargets = fillTargets(coreRegulators.length, extraRegulators);
	}
	
	/**
	 * Helper to fill the connectivity matrix to be called by the constructor only.
	 * 
	 * @param ddmanager
	 * @param functions
	 * @param regulators
	 */
	private int[][] fillRegulators(MDDManager ddmanager, int[] functions) {
		int[][] regulators = new int[functions.length][];
		for (int f=0 ; f<functions.length ; f++) {
			int function = functions[f];
			boolean[] varFlags = ddmanager.collectDecisionVariables(function);
			
			int nbRegulators = 0;
			for (boolean b: varFlags) {
				if (b) {
					nbRegulators++;
				}
			}
			
			int[] curRegulators = new int[nbRegulators];
			regulators[f] = curRegulators;
			nbRegulators = 0;
			for (int i=0 ; i<varFlags.length ; i++) {
				if (varFlags[i]) {
					curRegulators[nbRegulators] = i;
					nbRegulators++;
				}
			}
		}
		
		return regulators;
	}

	/**
	 * Helper to fill the list of targets based on the existing lists of regulators.
	 * 
	 * @param coreComponents
	 * @param regulators
	 * @return
	 */
	private int[][] fillTargets(int coreSize, int[][] regulators) {
		
		// count targets
		int[] targetCounts = new int[coreSize];
		for (int[] curRegs: regulators) {
			for (int c: curRegs) {
				targetCounts[c]++;
			}
		}
		
		// create empty lists of the right sizes
		int[][] targets = new int[coreSize][];
		for (int c=0 ; c<targetCounts.length ; c++) {
			targets[c] = new int[targetCounts[c]];
			// reset targetCount for the next step
			targetCounts[c] = 0;
		}
		
		// fill these lists
		for (int t=0 ; t<regulators.length ; t++) {
			int[] curRegs = regulators[t];
			for (int c: curRegs) {
				int idx = targetCounts[c]++;
				targets[c][idx] = t;
			}
		}
		
		return targets;
	}
	
	/**
	 * Get the list of core components regulating a given component.
	 * 
	 * @param idx index of the component
	 * @param extra if true the considered component is an extra component, otherwise it is part of the core
	 * 
	 * @return indices of its regulators (all are core components)
	 */
	public int[] getRegulators(int idx, boolean extra) {
		if (extra) {
			return extraRegulators[idx];
		}
		return coreRegulators[idx];
	}

	/**
	 * Get the list of components regulated by a given core component.
	 * 
	 * @param idx index of the considered core component
	 * @param extra if true, it will give regulated extra components, otherwise core components
	 * 
	 * @return indices of regulated components either in the core or in extra
	 */
	public int[] getCoreTargets(int idx, boolean extra) {
		if (extra) {
			return extraTargets[idx];
		}
		return coreTargets[idx];
	}

}
