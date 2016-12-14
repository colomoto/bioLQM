package org.colomoto.logicalmodel.modifier.reduction;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.modifier.ModelModifier;

public class OutputSimplifier implements ModelModifier {

	private final LogicalModel model;

	public OutputSimplifier(LogicalModel model) {
		this.model = model;
	}

	@Override
	public LogicalModel getModifiedModel() {
		ModelReducer reducer = new ModelReducer(model);
		reducer.removePseudoOutputs();
		return reducer.getModel();
	}

}
