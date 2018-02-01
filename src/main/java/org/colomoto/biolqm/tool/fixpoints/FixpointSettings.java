package org.colomoto.biolqm.tool.fixpoints;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.ToolSettings;

public class FixpointSettings extends ToolSettings {

	public FixpointMethod method = FixpointMethod.MDD;

	public FixpointSettings(LogicalModel model) {
		super(model);
	}
}

enum FixpointMethod {
	MDD, ASP;
}
