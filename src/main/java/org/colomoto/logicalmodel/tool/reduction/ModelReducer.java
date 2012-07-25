package org.colomoto.logicalmodel.tool.reduction;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.LogicalModelImpl;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;


/**
 * Wrap {@link ReduceOperation} to properly remove variables from models.
 * 
 * @author Aurelien Naldi
 */
public class ModelReducer {

	private final MDDManager ddmanager;
	private final ReduceOperation rop;
	private final List<NodeInfo> allNodes;
	
	private final MDDVariable[] variables;
	private final int[] allFunctions;
	private final Map<MDDVariable, Set<Integer>> targets;
	
	public ModelReducer(LogicalModel model) {
		this.ddmanager = model.getMDDManager();
		this.rop = new ReduceOperation(ddmanager);
		this.variables = ddmanager.getAllVariables();
		
		this.allNodes = new ArrayList<NodeInfo>(model.getNodeOrder());
		allNodes.addAll(model.getExtraComponents());
		
		// copy logical function and create lists of regulators
		int[] functions = model.getLogicalFunctions();
		int[] extraFunctions = model.getExtraLogicalFunctions();
		
		this.targets = new HashMap<MDDVariable, Set<Integer>>();
		this.allFunctions = new int[functions.length + extraFunctions.length];
		int curIdx = 0;
		for (int f: model.getLogicalFunctions()) {
			addRegulators(curIdx, f);
			ddmanager.use(f);
			curIdx++;
		}
		for (int f: extraFunctions) {
			addRegulators(curIdx, f);
			ddmanager.use(f);
			curIdx++;
		}
	}

	/**
	 * Find outputs and pseudo-outputs in the model and remove them all.
	 * @return the number of removed elements (outputs and pseudo-outputs)
	 */
	public int removePseudoOutputs() {
		// count targets of every component and build lists of regulators
		int[] targetCount = new int[allFunctions.length];
		Set<Integer>[] regulators = new Set[allFunctions.length];
		for (int i=0 ; i<targetCount.length ; i++) {
			MDDVariable curVar = variables[i];
			Set<Integer> curTargets = targets.get(curVar);
			if (curTargets == null) {
				targetCount[i] = 0;
			} else {
				targetCount[i] = curTargets.size();
				Integer I = i;
				for (int tgt: curTargets) {
					Set<Integer> curRegs = regulators[tgt];
					if (curRegs == null) {
						curRegs = new HashSet<Integer>();
						regulators[tgt] = curRegs;
					}
					curRegs.add(I);
				}
			}
		}
		
		// actual pseudo-output detection
		Set<Integer> outputs = new HashSet<Integer>();
		for (int i=0 ; i<targetCount.length ; i++) {
			if (targetCount[i] == 0) {
				outputs.add(i);
			}
		}
		
		// expand to pseudo-outputs
		Set<Integer> pseudoOutputs = new HashSet<Integer>();
		for (int idx: outputs) {
			// decrease target count for all of its regulators and detect pseudo-outputs among them
			for (int regIdx: regulators[idx]) {
				targetCount[regIdx]--;
				if (targetCount[regIdx] == 0) {
					pseudoOutputs.add(regIdx);
				}
			}
		}
		
		// actual removal
		for (int idx: pseudoOutputs) {
			remove(idx);
		}
		
		return pseudoOutputs.size();
	}
	
	/**
	 * Remove a selected variable.
	 * 
	 * @param varIdx
	 */
	public void remove(int varIdx) {
		MDDVariable var = variables[varIdx];
		Set<Integer> varTargets = targets.get(var);
		if (varTargets == null) {
			return;
		}
		
		int regulator = allFunctions[varIdx];
		for (int nodeIdx: varTargets) {
			int f = allFunctions[nodeIdx];
			int newFunction = rop.remove(f, regulator, var);
			ddmanager.free(f);
			addRegulators(nodeIdx, newFunction);
		}
		
		// the selected variable should not have any remaining target
		targets.remove(var);
	}
	
	/**
	 * Add the regulators of i
	 * 
	 * @param nodeIdx
	 * @param function
	 */
	private void addRegulators(int nodeIdx, int function) {
		allFunctions[nodeIdx] = function;
		boolean[] regulators = ddmanager.collectDecisionVariables(function);
		for (int j=0 ; j<regulators.length ; j++) {
			if (regulators[j]) {
				// add i to the list of targets of the variable j
				MDDVariable var = variables[j];
				Set<Integer> varTargets = targets.get(var);
				if (varTargets == null) {
					varTargets = new HashSet<Integer>();
					targets.put(var,  varTargets);
				}
				varTargets.add(nodeIdx);
			}
		}
	}
	

	/**
	 * Get a logical model for the modified functions.
	 * 
	 * @return a new LogicalModel
	 */
	public LogicalModel getModel() {
		
		int countCore = 0;
		boolean[] inCore = new boolean[allFunctions.length];
		for (int i=0 ; i<variables.length ; i++) {
			MDDVariable var = variables[i];
			Set<Integer> varTargets = targets.get(var);
			if (varTargets != null && varTargets.size() > 0) {
				inCore[i] = true;
				countCore++;
			}
		}
		
		// prepare the content of the new model
		int[] coreFunctions = new int[countCore];
		int[] extraFunctions = new int[allFunctions.length - countCore];
		List<MDDVariable> newVariables = new ArrayList<MDDVariable>();
		List<NodeInfo> coreNodes = new ArrayList<NodeInfo>(countCore);
		List<NodeInfo> extraNodes = new ArrayList<NodeInfo>(extraFunctions.length);
		
		// add each node/function in core or extra
		for (int i=0 ; i<variables.length ; i++) {
			NodeInfo ni = allNodes.get(i);
			int function = allFunctions[i];
			if (inCore[i]) {
				newVariables.add(variables[i]);
				coreFunctions[coreNodes.size()] = function;
				coreNodes.add(ni);
			} else {
				extraFunctions[extraNodes.size()] = function;
				extraNodes.add(ni);
			}
		}

		MDDManager newDDManager = ddmanager.getManager(newVariables);
		return new LogicalModelImpl(newDDManager, coreNodes, coreFunctions, extraNodes, extraFunctions);
	}
}

