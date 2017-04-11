package org.colomoto.biolqm.tool.stablestate;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractTool;
import org.colomoto.biolqm.tool.LogicalModelTool;
import org.mangosdk.spi.ProviderFor;

@ProviderFor(LogicalModelTool.class)
public class StableASPTool extends AbstractTool {

	public StableASPTool() {
		super("stableASP", "Search stable states using ASP", true);
	}

	@Override
	public void run(LogicalModel model) {
		StableASP asp = new StableASP(model);
		asp.run();
	}
}
