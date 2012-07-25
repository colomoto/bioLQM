package org.colomoto.logicalmodel.tool.reduction;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.LogicalModelModifier;

public class OutputSimplifier implements LogicalModelModifier {
	
	@Override
	public LogicalModel apply(LogicalModel model) {
		ModelReducer reducer = new ModelReducer(model);
		reducer.removePseudoOutputs();
		return reducer.getModel();
	}

}
