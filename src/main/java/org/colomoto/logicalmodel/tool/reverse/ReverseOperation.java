package org.colomoto.logicalmodel.tool.reverse;

import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;

/**
 * Reverse a node according to a given variable. This allows the construction of
 * a model with the reversed dynamical behaviour, for example to find backward
 * reachable states.
 *
 * @author Aurelien Naldi
 * @author Pedro T. Monteiro
 */
public class ReverseOperation {

	private final MDDManager manager;

	public ReverseOperation(MDDManager manager) {
		this.manager = manager;
	}

	/**
	 * Reverse a node.
	 *
	 * if the node does not contain the target variable, it amounts to a regular
	 * negation. On the target variable, the two children are swapped in
	 * addition to being negated.
	 *
	 * @param var
	 * @param node
	 * @return
	 */
	public int reverse(MDDVariable var, int node) {
		if (manager.isleaf(node))
			return manager.not(node);

		MDDVariable curvar = manager.getNodeVariable(node);
		if (curvar.nbval > 2) {
			throw new RuntimeException(
					"Reverse only applies to Boolean functions");
		}

		int c0 = reverse(var, manager.getChild(node, 0));
		int c1 = reverse(var, manager.getChild(node, 1));
		int newNode = (curvar.equals(var)) ? curvar.getNode(c1, c0) : curvar
				.getNode(c0, c1);
		manager.free(c0);
		manager.free(c1);
		return newNode;
	}
}
