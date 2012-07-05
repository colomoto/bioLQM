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
		List<NodeInfo> coreComponents = model.getNodeOrder();
		List<NodeInfo> extraComponents = model.getExtraComponents();
		
		coreTargets= new int[coreComponents.size()][];
		extraTargets = new int[extraComponents.size()][];
		
		MDDManager ddmanager = model.getMDDManager();
		
		// fill in core regulators and targets
		coreRegulators = fillRegulators(ddmanager, model.getLogicalFunctions());

		// fill in extra regulators and targets
		extraRegulators = fillRegulators(ddmanager, model.getExtraLogicalFunctions());
		
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
	
	public int[] getRegulators(int idx, boolean extra) {
		if (extra) {
			return extraRegulators[idx];
		}
		return coreRegulators[idx];
	}
	
	public int[] getCoreTargets(int idx, boolean extra) {
		if (extra) {
			return extraTargets[idx];
		}
		return coreTargets[idx];
	}
	
	
}
