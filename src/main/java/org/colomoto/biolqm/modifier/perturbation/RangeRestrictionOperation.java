package org.colomoto.biolqm.modifier.perturbation;

import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;

/**
 * Operation used to restrict the values reached in a MDD into a specific range.
 * 
 * @author Aurelien Naldi
 */
public class RangeRestrictionOperation {

	private final MDDManager manager;
	private final int min;
	private final int max;
	
	public RangeRestrictionOperation(MDDManager manager, int min, int max) {
		this.manager = manager;
		this.min = min;
		this.max = max;
	}
	
	/**
	 * Compute a range-restricted version of a MDD
	 * 
	 * @param node the MDD to restrict
	 * @return the restricted MDD ID
	 */
	public int restrict(int node) {
		if (manager.isleaf(node)) {
			if (node <= min) {
				return min;
			}
			if (node > max) {
				return max;
			}
			return node;
		}
		
		MDDVariable var = manager.getNodeVariable(node);
		if (var.nbval == 2) {
			int f = restrict(manager.getChild(node, 0));
			int t = restrict(manager.getChild(node, 1));
			return var.getNode(f, t);
		}
		
		int[] children = new int[var.nbval];
		for (int i=0 ; i<var.nbval ; i++) {
			children[i] = restrict(manager.getChild(node, i));
		}
		return var.getNode(children);
	}
}
