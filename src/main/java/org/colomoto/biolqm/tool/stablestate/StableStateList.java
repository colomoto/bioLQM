package org.colomoto.biolqm.tool.stablestate;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;

public class StableStateList extends ArrayList<int[]> {

	public final List<NodeInfo> nodes;

	public StableStateList(LogicalModel model) {
		this.nodes = model.getComponents();
	}
	
}
