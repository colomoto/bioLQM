package org.colomoto.biolqm.tool.trapspaces;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.ToolSettings;

public class TrapSpaceSettings extends ToolSettings {

	public boolean reduce = false;
	
	public boolean percolate = true;
	public boolean bdd = false;
	public boolean showasp = false;
	
	public boolean terminal = false;
	public boolean tree = false;

	public String[] focusComponents = null;

	public TrapSpaceSettings(LogicalModel model) {
		super(model);
	}
}
