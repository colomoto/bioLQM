package org.colomoto.biolqm.metadata;

import org.colomoto.biolqm.NodeInfo;

/**
 * This contains the source node and the target node of an edge
 *
 * @author Martin Boutroux
 */
public class NodeInfoPair {
	
	private final NodeInfo node1;
	private final NodeInfo node2;
	
	public NodeInfoPair(NodeInfo node1, NodeInfo node2) {
		this.node1 = node1;
		this.node2 = node2;
	}
	
	public NodeInfo getNode1() {
		return this.node1;
	}
	
	public NodeInfo getNode2() {
		return this.node2;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof NodeInfoPair) {
			NodeInfoPair nif = (NodeInfoPair) obj;
			return this.node1.equals(nif.getNode1()) && this.node2.equals(nif.getNode2());
		}
		return false;
	}
	
    @Override
	public int hashCode() {
    	return this.node1.hashCode() + this.node2.hashCode();
	}
}
