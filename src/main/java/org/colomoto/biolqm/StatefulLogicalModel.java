package org.colomoto.biolqm;

import java.util.List;

/**
 * Extension of a logical model with the possibility to store initial states and oracles
 * 
 * @author Rui Henriques, Pedro Monteiro
 * @version 1.0
 */
public interface StatefulLogicalModel extends LogicalModel {

	/**
	 * Gets the set of initial states defining a portion of the state-space
	 * @return list of initial states
	 */
	List<byte[]> getInitialStates();
	
	/**
	 * Gets the stored oracles 
	 * @return list of oracles where each oracle is defined as a set of state-patterns (null if not available)
	 */
	List<List<byte[]>> getOracles();
	
	/**
	 * Stores a set of oracles within the model
	 * @param o list of oracles, where each oracle is defined as a set of state-patterns
	 */
	void setOracles(List<List<byte[]>> o);
	
	/**
	 * Changes the set of initial states
	 * @param states list of initial states
	 */
	void setInitialStates(List<byte[]> states);
	
	/**
	 * Returns the model's name (by default the associated filename)
	 * @return the name of the model
	 */
	String getName();
}
