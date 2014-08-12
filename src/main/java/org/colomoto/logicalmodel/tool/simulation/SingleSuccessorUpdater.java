package org.colomoto.logicalmodel.tool.simulation;

/**
 * General definition of updating methods returning a single successor.
 * Such an updater can be determinist or stochastic, as denoted by the use of specialised interfaces.
 *
 * @see DeterministicUpdater
 * @see RandomUpdater
 * 
 * @author Aurelien Naldi
 */
public interface SingleSuccessorUpdater extends LogicalModelUpdater {

	/**
	 * Get the successor of a state state.
	 * @param state the current state
     * @return the successor state
	 */
	byte[] getSuccessor(byte[] state);
	
}
