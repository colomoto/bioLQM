package org.colomoto.logicalmodel;

/**
 * An object containing a NodeInfo
 * 
 * @author Aurelien Naldi
 */
public interface NodeInfoHolder {

	/**
	 * Get the internal NodeInfo.
	 * The node info stores the ID and max value of the node. It should NOT be modified directly.
	 * 
	 * @return
	 */
	NodeInfo getNodeInfo();
}
