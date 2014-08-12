package org.colomoto.logicalmodel.tool.simulation;

import org.colomoto.logicalmodel.LogicalModel;

import java.util.List;

/**
 * General definition of updating methods: get a list of successors.
 * Note that this interface should not be directly implemented, consider one of the more specific interfaces.
 *
 * @see MultipleSuccessorsUpdater
 * @see DeterministicUpdater
 * @see RandomUpdater
 *
 * @author Aurelien Naldi
 */
public interface LogicalModelUpdater {

    /**
     * Get all successors for a given state.
     *
     * @param state the state from which to look for successors
     * @return the list of all successors (an empty list denotes that the provided state is stable).
     */
	List<byte[]> getSuccessors(byte[] state);

    LogicalModel getModel();
}
