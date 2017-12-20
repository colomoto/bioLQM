package org.colomoto.biolqm.tool.stablestate;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.ToolSettings;

public class StableStateSettings extends ToolSettings {

	public StableStateMethod method = StableStateMethod.MDD;

	public StableStateSettings(LogicalModel model) {
		super(model);
	}
}

enum StableStateMethod {
	MDD, ASP;
}
