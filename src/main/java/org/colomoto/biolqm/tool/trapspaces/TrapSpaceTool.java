package org.colomoto.biolqm.tool.trapspaces;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractTool;
import org.colomoto.biolqm.tool.LogicalModelTool;
import org.mangosdk.spi.ProviderFor;


@ProviderFor(LogicalModelTool.class)
public class TrapSpaceTool extends AbstractTool {

	public TrapSpaceTool() {
		super("trapspace", "Search trap spaces using ASP", true);
	}

	@Override
	public void run(LogicalModel model) {
		TrapSpaceIdentifier identifier = new TrapSpaceIdentifier(model, false);
		identifier.run();
	}
}
