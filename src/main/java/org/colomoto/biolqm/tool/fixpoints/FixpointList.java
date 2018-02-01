package org.colomoto.biolqm.tool.fixpoints;

import java.util.ArrayList;
import java.util.List;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;

public class FixpointList extends ArrayList<int[]> {

	public final List<NodeInfo> nodes;

	public FixpointList(LogicalModel model) {
		this.nodes = model.getComponents();
	}
	
}
