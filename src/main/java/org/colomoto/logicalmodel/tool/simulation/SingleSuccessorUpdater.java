package org.colomoto.logicalmodel.tool.simulation;

/**
 * General definition of updating methods returning a single successor
 * 
 * @see org.colomoto.logicalmodel.tool.simulation.updater.SynchronousUpdater
 *
 * @author Aurelien Naldi
 */
public interface SingleSuccessorUpdater {

	/**
	 * Get the successor of a state state.
	 * @param state the current state
     * @return the successor state
	 */
	byte[] getSuccessor(byte[] state);
	
}
