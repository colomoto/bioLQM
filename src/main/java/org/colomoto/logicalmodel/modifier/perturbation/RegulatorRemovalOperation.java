package org.colomoto.logicalmodel.modifier.perturbation;

import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;

/**
 * Operation used to remove a regulator (by assigning it a fixed value) from a MDD.
 * It can be used by perturbations removing interactions, or to propagate fixed component values to the model.
 * 
 * @author Aurelien Naldi
 */
public class RegulatorRemovalOperation {

	private final MDDManager manager;
	private final MDDVariable regulator;
	private final int value;
	
	public RegulatorRemovalOperation(MDDManager manager, MDDVariable regulator, int value) {
		this.manager = manager;
		this.regulator = regulator;
		this.value = value;
	}
	
	/**
	 * Remove a regulator from a MDD.
	 * It will consider the regulator as having a fixed value and
	 * use the corresponding child instead of the nodes for this regulator.
	 * 
	 * @param node the MDD to restrict
	 * @return the restricted MDD ID
	 */
	public int restrict(int node) {
		if (manager.isleaf(node)) {
			return node;
		}
		
		MDDVariable var = manager.getNodeVariable(node);
		if (var.after(regulator)) {
            manager.use(node);
			return node;
		}
		
		if (var == regulator) {
            int n = manager.getChild(node, value);
            manager.use(n);
			return n;
		}
		
		if (var.nbval == 2) {
			int f = restrict(manager.getChild(node, 0));
			int t = restrict(manager.getChild(node, 1));
			return var.getNodeFree(f, t);
		}
		
		int[] children = new int[var.nbval];
		for (int i=0 ; i<var.nbval ; i++) {
			children[i] = restrict(manager.getChild(node, i));
		}
		return var.getNodeFree(children);
	}
}
