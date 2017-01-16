package org.colomoto.biolqm.tool.simulation;

import java.util.List;

/**
 * Updaters which can yield multiple successors for a single state.
 *
 * @author Aurelien Naldi
 */
public interface MultipleSuccessorsUpdater extends LogicalModelUpdater {

    /**
     * Get all successors for a given state.
     *
     * @param state the state from which to look for successors
     * @return the list of all successors (an empty list denotes that the provided state is stable).
     */
    List<byte[]> getSuccessors(byte[] state);

}
