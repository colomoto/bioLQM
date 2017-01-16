package org.colomoto.biolqm;


/**
 * Contains the basic informations of a regulatory node (gene) : 
 * 	- nodeID : a unique identifier for the gene
 *  - max : the maximal regulatory level 
 *
 * @author Aurelien Naldi
 */
public class NodeInfo {
	
	public static final byte UNDEFINED_MAX = -1;
	
	private String nodeID;
	private byte max;
	private boolean isInput = false;

	private NodeInfo[] booleanized_group = null;

	public NodeInfo(String name) {
		this(name, (byte)1);
	}
	
	public NodeInfo(String name, byte max) {
		super();
		this.nodeID = name;
		this.max = max;
	}

	public String getNodeID() {
		return nodeID;
	}
	
	public void setNodeID( String id) {
		this.nodeID = id;
	}
	
	public byte getMax() {
		return max;
	}
	
	public void setMax(byte max) {
		this.max = max;
	}
	
	public boolean isInput() {
		return isInput;
	}
	
	public void setInput(boolean isInput) {
		this.isInput = isInput;
	}
	
	/**
	 * Compare the object to the given one. If the given object is a RegulatoryNode, the IDs are compared
	 */
	@Override
	public boolean equals( Object obj) {
		
		if ( obj instanceof NodeInfo) {
			NodeInfo other = (NodeInfo)obj;
			return this.nodeID.equals(other.nodeID) &&
				this.max == other.max &&
				this.isInput == other.isInput;
			
		} else if( obj instanceof NodeInfoHolder) {
			return super.equals(((NodeInfoHolder)obj).getNodeInfo());
		} else {
			return false;
		}
	}
	
	@Override
	public String toString() {
		
		return nodeID;
	}

	public NodeInfo[] getBooleanizedGroup() {
		return booleanized_group;
	}
	public void setBooleanizedGroup(NodeInfo[]booleanized_group) {
		this.booleanized_group = booleanized_group;
	}
}
