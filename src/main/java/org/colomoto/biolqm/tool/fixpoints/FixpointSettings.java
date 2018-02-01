package org.colomoto.biolqm.tool.fixpoints;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.ToolSettings;

public class FixpointSettings extends ToolSettings {

	public FixpointMethod method = FixpointMethod.MDD;

	public FixpointSettings(LogicalModel model) {
		super(model);
	}

	public void useASP() {
		this.method = FixpointMethod.ASP;
	}

	public void useMDD() {
		this.method = FixpointMethod.MDD;
	}
}

enum FixpointMethod {
	MDD, ASP;
}
