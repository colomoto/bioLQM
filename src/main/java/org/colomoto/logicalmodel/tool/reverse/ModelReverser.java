package org.colomoto.logicalmodel.tool.reverse;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.LogicalModelImpl;
import org.colomoto.logicalmodel.NodeInfo;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;

/**
 * Wrap {@link ReverseOperation} to properly create a new model capable of
 * generating the reverse dynamics.
 * 
 * @author Pedro T. Monteiro
 */
public class ModelReverser {

	private final MDDManager ddmanager;
	private final ReverseOperation revOp;
	private final List<NodeInfo> allNodes;

	private final MDDVariable[] variables;
	private final int[] allFunctions;

	public ModelReverser(LogicalModel model) {
		this.ddmanager = model.getMDDManager();
		this.revOp = new ReverseOperation(ddmanager);
		this.variables = ddmanager.getAllVariables();

		this.allNodes = new ArrayList<NodeInfo>(model.getNodeOrder());
		this.allNodes.addAll(model.getExtraComponents());

		int[] functions = model.getLogicalFunctions();
		int[] extrafunctions = model.getExtraLogicalFunctions();
		this.allFunctions = new int[functions.length + extrafunctions.length];
		for (int i = 0; i < functions.length; i++) {
			this.allFunctions[i] = functions[i];
		}
		for (int i = functions.length; i < (functions.length
				+ extrafunctions.length - 1); i++) {
			this.allFunctions[i] = extrafunctions[i - functions.length];
		}
	}

	/**
	 * Reverse the dynamics of all variables.
	 */
	public void reverse() {
		for (int varIdx = 0; varIdx < variables.length; varIdx++) {
			reverse(varIdx);
		}
	}

	/**
	 * Reverse the dynamics of the selected variable.
	 * 
	 * @param varIdx
	 */
	public void reverse(int varIdx) {
		MDDVariable var = variables[varIdx];
		int f = allFunctions[varIdx];
		int newFunction = revOp.reverse(var, f);
		ddmanager.free(f);
		allFunctions[varIdx] = newFunction;
	}

	/**
	 * Get a logical model with modified functions capable of reversed dynamics.
	 * 
	 * @return a new LogicalModel
	 */
	public LogicalModel getModel() {
		return new LogicalModelImpl(allNodes, ddmanager, allFunctions);
	}
}
