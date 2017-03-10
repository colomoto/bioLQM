package org.colomoto.biolqm.modifier.reverse;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.modifier.ModelModifier;
import org.colomoto.biolqm.modifier.booleanize.Booleanizer;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;

/**
 * Wrap {@link ReverseOperation} to properly create a new model capable of
 * generating the reverse dynamics.
 * 
 * @author Pedro T. Monteiro
 */
public class ModelReverser implements ModelModifier {

	private final MDDManager ddmanager;
	private final ReverseOperation revOp;
	private final List<NodeInfo> allNodes;

	private final MDDVariable[] variables;
	private final int[] allFunctions;

	/**
	 * Prepare a Model modifier for model reversal.
	 * 
	 * @param model The odel to be reversed
	 */
	public ModelReverser(LogicalModel model) {
		// First booleanize the model
		model = Booleanizer.booleanize(model);

		this.ddmanager = model.getMDDManager();
		this.revOp = new ReverseOperation(ddmanager);
		this.variables = ddmanager.getAllVariables();

		this.allNodes = new ArrayList<NodeInfo>(model.getComponents());
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

		this.reverse();
	}

	/**
	 * Reverse the dynamics of all variables.
	 */
	public void reverse() {
		for (int varIdx = 0; varIdx < variables.length; varIdx++) {
			reverse(varIdx);
		}

		// Fix the functions for booleanized models
		Booleanizer.preventForbiddenStates(ddmanager, allNodes, allFunctions);
	}

	/**
	 * Reverse the dynamics of the selected variable.
	 * 
	 * @param varIdx the index of the variable to reverse
	 */
	private void reverse(int varIdx) {
		MDDVariable var = variables[varIdx];
		int f = allFunctions[varIdx];
		int newFunction = revOp.reverse(var, f);
		ddmanager.free(f);
		allFunctions[varIdx] = newFunction;
	}

	@Override
	public LogicalModel getModifiedModel() {
		return new LogicalModelImpl(allNodes, ddmanager, allFunctions);
	}
}
