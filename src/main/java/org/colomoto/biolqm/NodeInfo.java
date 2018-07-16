package org.colomoto.biolqm;


import org.colomoto.common.xml.XMLWriter;

/**
 * Contains the basic information of a regulatory component :
 * <ul>
 * 	<li>nodeID : a unique identifier for the gene</li>
 * 	<li>display name (optional)</li>
 *  <li>max : the maximal activity level</li>
 *</ul>
 *
 * @author Aurelien Naldi
 */
public class NodeInfo {
	
	public static final byte UNDEFINED_MAX = -1;

	private String nodeID;
	private String name;
	private byte max;
	private boolean isInput = false;
	private int hashcode = -1;

	private NodeInfo[] booleanized_group = null;

	public NodeInfo(String uid) {
		this(uid, null, (byte)1);
	}

	public NodeInfo(String uid, byte max) {
		this(uid, null, max);
	}

	public NodeInfo(String uid, String name, byte max) {
		super();
		setNodeID(uid);
		setName(name);
		this.max = max;
	}

	public String getNodeID() {
		return nodeID;
	}

	public void setNodeID( String id) {
		if (!XMLWriter.isValidId(id)) {
			throw new RuntimeException("Invalid ID: "+ id);
		}
		this.nodeID = id;
	}

	public String getName() {
		return name;
	}

	public void setName( String name) {
		this.name = name;
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
			return equals(((NodeInfoHolder)obj).getNodeInfo());
		} else {
			return false;
		}
	}

	public String getDisplayName() {
		if (name != null && name.length() > 0) {
			return name;
		}
		return nodeID;
	}

	@Override
	public String toString() {
		return nodeID;
	}

	public NodeInfo[] getBooleanizedGroup() {
		return booleanized_group;
	}
	public void setBooleanizedGroup(NodeInfo[] booleanized_group) {
		this.booleanized_group = booleanized_group;
	}
	
	public NodeInfo clone() {
		NodeInfo clone = new NodeInfo(getNodeID(), max);
		clone.isInput = isInput;
		clone.hashcode = this.hashCode();
		if (booleanized_group != null) {
			clone.booleanized_group = new NodeInfo[booleanized_group.length];
			for (int i=0 ; i<booleanized_group.length ; i++) {
				clone.booleanized_group[i] = booleanized_group[i].clone();
			}
		}
		return clone;
	}
	
	@Override
	public int hashCode() {
		if (hashcode <= 0) {
			return super.hashCode();
		}
		return hashcode;
	}

}
