package org.colomoto.logicalmodel.tool.simulation;

import org.colomoto.logicalmodel.tool.simulation.updater.AbstractUpdater;
import org.colomoto.logicalmodel.tool.simulation.updater.AsynchronousUpdater;

/**
 * General definition of updating methods: iterator on the successor states.
 * 
 * @see AbstractUpdater
 * @see AsynchronousUpdater
 * 
 * @author Aurelien Naldi
 */
public interface LogicalModelUpdater extends Iterable<byte[]> {

	/**
	 * Set the current state, for which this updater should look for successor(s)
	 * @param state the current state
	 */
	void setState(byte[] state);
	
}
