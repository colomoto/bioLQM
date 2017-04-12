package org.colomoto.biolqm.tool.trapspaces;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.AbstractTool;
import org.colomoto.biolqm.tool.LogicalModelTool;
import org.mangosdk.spi.ProviderFor;


@ProviderFor(LogicalModelTool.class)
public class TrapSpacesBDDTool extends AbstractTool {

	public TrapSpacesBDDTool() {
		super("trapspaceBDD", "Alternative search for percolated trap spaces", true);
	}

	@Override
	public void run(LogicalModel model) {
		TrapSpaceIdentifier identifier = new TrapSpaceIdentifier(model, true);
		identifier.run();
	}
}
