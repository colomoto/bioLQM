package org.colomoto.biolqm.modifier.reverse;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.LogicalModelImpl;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.modifier.BaseModifier;
import org.colomoto.biolqm.modifier.booleanize.BooleanizeModifier;
import org.colomoto.mddlib.MDDManager;
import org.colomoto.mddlib.MDDVariable;

import java.util.ArrayList;
import java.util.List;

/**
 * Wrap {@link ReverseOperation} to properly create a new model capable of
 * generating the reverse dynamics.
 * 
 * @author Pedro T. Monteiro
 */
public class ReverseModifier extends BaseModifier {

	private final LogicalModel model;

	/**
	 * Prepare a Model modifier for model reversal.
	 * 
	 * @param model The odel to be reversed
	 */
	public ReverseModifier(LogicalModel model) {
		this.model = model;
	}

	@Override
	public LogicalModel performTask() throws Exception {

		// Start by ensuring to work with a Boolean model
		LogicalModel model = new BooleanizeModifier(this.model).call();

		// Retrieve all functions
		MDDManager ddmanager = model.getMDDManager();
		ReverseOperation  revOp = new ReverseOperation(ddmanager);
		MDDVariable[] variables = ddmanager.getAllVariables();

		List<NodeInfo> allNodes = new ArrayList<NodeInfo>(model.getComponents());
		allNodes.addAll(model.getExtraComponents());

		int[] functions = model.getLogicalFunctions();
		int[] extrafunctions = model.getExtraLogicalFunctions();
		int[] allFunctions = new int[functions.length + extrafunctions.length];
		for (int i = 0; i < functions.length; i++) {
			allFunctions[i] = functions[i];
		}
		for (int i = functions.length; i < (functions.length
				+ extrafunctions.length - 1); i++) {
			allFunctions[i] = extrafunctions[i - functions.length];
		}

		// Find reversed functions for all components
		for (int varIdx = 0; varIdx < variables.length; varIdx++) {
			MDDVariable var = variables[varIdx];
			int f = allFunctions[varIdx];
			int newFunction = revOp.reverse(var, f);
			ddmanager.free(f);
			allFunctions[varIdx] = newFunction;
		}

		// Fix the functions for booleanized models
		BooleanizeModifier.preventForbiddenStates(ddmanager, allNodes, allFunctions);

		return new LogicalModelImpl(allNodes, ddmanager, allFunctions);
	}

}
