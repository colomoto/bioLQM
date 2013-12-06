package org.colomoto.logicalmodel.tool.reduction;

import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;
import org.colomoto.mddlib.NodeRelation;
import org.colomoto.mddlib.operators.AbstractOperator;

/**
 * Rewrite a logical function to remove a regulator.
 * This is the MDD computation at the core of model reduction.
 * @see ModelReducer
 * 
 * @author Aurelien Naldi
 */
public class ReduceOperation {

	private final MDDManager ddmanager;
	
	public ReduceOperation(MDDManager ddmanager) {
		this.ddmanager = ddmanager;
	}
	
	/**
	 * Remove <code>regulator</code> from its target <code>node</code>.
	 * This is the first part of the algo: we have not yet found the 
	 * regulator in the logical function.
	 * It will be called recursively until we find it (or go too far)
	 * 
	 * @param node
	 * @param regulator
	 * @param rmVar
	 * @return
	 */
	public int remove(int node, int regulator, MDDVariable rmVar) {
		MDDVariable nvar = ddmanager.getNodeVariable(node);
		if (nvar == null || nvar.after(rmVar)) {
			return node;
		}
		
		MDDVariable regVar = ddmanager.getNodeVariable(regulator);
		if (nvar == rmVar) {
			if (regVar == null) {
				return ddmanager.getChild(node, regulator);
			}
			if (regVar == rmVar) {
				throw new RuntimeException("Can not continue the simplification: a circuit would get lost");
			}
			
			return remove(ddmanager.getChildren(node), regulator);
		}
		
		MDDVariable nextVariable;
		int[] next;
		if (regVar == null || regVar.after(nvar)) {
			nextVariable = nvar;
			next = new int[nextVariable.nbval];
			for (int i=0 ; i<next.length ; i++) {
				next[i] = remove(ddmanager.getChild(node,i), regulator, rmVar);
			}
		} else if (nvar.after(regVar)) {
			nextVariable = regVar;
			next = new int[nextVariable.nbval];
			for (int i=0 ; i<next.length ; i++) {
				next[i] = remove(node, ddmanager.getChild(regulator, i), rmVar);
			}
		} else {
			nextVariable = nvar;
			next = new int[nextVariable.nbval];
			for (int i=0 ; i<next.length ; i++) {
				next[i] = remove(ddmanager.getChild(node, i), ddmanager.getChild(regulator, i), rmVar);
			}
		}
		
		return nextVariable.getNode(next);
	}

	/**
	 * Remove <code>regulator</code> from its target <code>node</code>.
	 * This is the second part of the algo: we have found the regulator 
	 * in the logical function.
	 * We must thus follow all branches corresponding to its possible values,
	 * until we can take the final decision.
	 * 
	 * @param t_ori
	 * @param regulator
	 * @return
	 */
	private int remove(int[] t_ori, int regulator) {
		if (ddmanager.isleaf(regulator)) {
			return t_ori[regulator];
		}
		// first, lookup for the best next step
		MDDVariable regVar = ddmanager.getNodeVariable(regulator);
		MDDVariable bestVar = regVar;
		int index = -1;
		for (int i=0 ; i<t_ori.length ; i++) {
			MDDVariable nvar = ddmanager.getNodeVariable(t_ori[i]);
			if (nvar == null || nvar.after(bestVar)) {
                continue;
            }
            // also update when equal to avoid stupid optimisations...
            bestVar = nvar;
            index = i;
		}
		
		int[] next = new int[bestVar.nbval];
		if (index == -1) {
			for (int i=0 ; i<bestVar.nbval ; i++) {
				next[i] = remove(t_ori, ddmanager.getChild(regulator,i));
			}
		} else {
			for (int i=0 ; i<bestVar.nbval ; i++) {
				int[] t_recur = new int[t_ori.length];
				for (int j=0 ; j<t_recur.length ; j++) {
					int node = t_ori[j];
					MDDVariable nvar = ddmanager.getNodeVariable(node);
					if (nvar == null || nvar.after(bestVar)) {
						t_recur[j] = node;
					} else {
						t_recur[j] = ddmanager.getChild(node, i);
					}
				}
				if (regVar == bestVar) {
					next[i] = remove(t_recur, ddmanager.getChild(regulator,i));
				} else {
					next[i] = remove(t_recur, regulator);
				}
			}
		}
		return bestVar.getNode(next);
	}
	
}