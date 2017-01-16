package org.colomoto.biolqm.modifier.reduction;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
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
    private final int coreSize;
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
		this.coreSize = functions.length;

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
     * This will preserve fixed inputs.
     *
	 * @return the number of removed elements (outputs and pseudo-outputs)
     * @see #removePseudoOutputs(boolean)
	 */
	public int removePseudoOutputs() {
        return this.removePseudoOutputs(true);
    }

    /**
     * Find outputs and pseudo-outputs in the model and remove them all.
     * Fixed inputs can be preserved or removed as well.
     *
     * @param preserveFixedInputs
     * @return the number of removed elements (outputs and pseudo-outputs)
     */
    public int removePseudoOutputs(boolean preserveFixedInputs) {
		// count targets of every component and build lists of regulators
		int[] targetCount = new int[coreSize];
		Set<Integer>[] regulators = new Set[allFunctions.length];
		for (int i=0 ; i<coreSize ; i++) {
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
		
		// detect pure outputs
		Set<Integer> outputs = new HashSet<Integer>();
		for (int i=0 ; i<targetCount.length ; i++) {
			if (targetCount[i] == 0) {
				outputs.add(i);
			}
		}
		
		
		// expand to pseudo-outputs, as long as we found new ones
		Set<Integer> pseudoOutputs = new HashSet<Integer>();
		Set<Integer> newOutputs = outputs;
		while (newOutputs.size() > 0) {
			Set<Integer> curOutputs = newOutputs;
			newOutputs = new HashSet<Integer>();
			for (int idx: curOutputs) {
				// decrease target count for all of its regulators to detect new pseudo-outputs
				Set<Integer> regs = regulators[idx];
				if (regs != null) {
					for (int regIdx: regulators[idx]) {
                        if (preserveFixedInputs && ddmanager.isleaf(allFunctions[regIdx])) {
                            continue;
                        }
						targetCount[regIdx]--;
						if (targetCount[regIdx] == 0) {
							pseudoOutputs.add(regIdx);
							newOutputs.add(regIdx);
						}
					}
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
        return getModel(true);
    }

    public LogicalModel getModel(boolean includeExtra) {
        return getModel(true, includeExtra);
    }

    public LogicalModel getModel(boolean preserveFixedInputs, boolean includeExtra) {
		int countCore = 0;
		boolean[] inCore = new boolean[allFunctions.length];
		for (int i=0 ; i<variables.length ; i++) {
			MDDVariable var = variables[i];
			Set<Integer> varTargets = targets.get(var);
			if (varTargets != null && varTargets.size() > 0) {
				inCore[i] = true;
				countCore++;
			} else if (preserveFixedInputs && ddmanager.isleaf(allFunctions[i])) {
                inCore[i] = true;
                countCore++;
            }
		}
		
		// prepare the content of the new model
		int[] coreFunctions = new int[countCore];
		List<MDDVariable> newVariables = new ArrayList<MDDVariable>();
		List<NodeInfo> coreNodes = new ArrayList<NodeInfo>(countCore);
        int[] extraFunctions = new int[allFunctions.length - countCore];
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
        if (includeExtra) {
            return new LogicalModelImpl(newDDManager, coreNodes, coreFunctions, extraNodes, extraFunctions);
        }
        return new LogicalModelImpl(coreNodes, newDDManager, coreFunctions);
	}
}

