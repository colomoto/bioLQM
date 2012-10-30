package org.colomoto.logicalmodel;


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
			return super.equals(obj);
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

}
