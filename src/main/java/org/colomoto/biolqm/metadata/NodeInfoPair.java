package org.colomoto.biolqm.metadata;

import org.colomoto.biolqm.NodeInfo;

/**
 * This contains the source node and the target node of an edge
 *
 * @author Martin Boutroux
 */
public class NodeInfoPair {
	
	// variables
	private NodeInfo node1;
	private NodeInfo node2;
	
	// constructors
	public NodeInfoPair(NodeInfo node1, NodeInfo node2) {
		this.node1 = node1;
		this.node2 = node2;
	}
	
	// getters
	public NodeInfo getNode1() {
		return this.node1;
	}
	
	public NodeInfo getNode2() {
		return this.node2;
	}
	
	// functions
	@Override
	public boolean equals(Object obj) {
		
		NodeInfoPair nif = (NodeInfoPair) obj;
		
		if (this.node1.equals(nif.getNode1()) && this.node2.equals(nif.getNode2())) {
			return true;
		}
		
		return false;
	}
	
    @Override
	public int hashCode() {
    	
    	return this.node1.hashCode() + this.node2.hashCode();
	}
}
