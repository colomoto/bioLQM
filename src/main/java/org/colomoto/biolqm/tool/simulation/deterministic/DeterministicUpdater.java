package org.colomoto.biolqm.tool.simulation.deterministic;

import org.colomoto.biolqm.tool.simulation.LogicalModelUpdater;

/**
 * Interface for deterministic updaters which always return the same single successor for a given state.
 *
 * @author Aurelien Naldi
 */
public interface DeterministicUpdater extends LogicalModelUpdater {

    /**
     * Get the unique successor of a state.
     * @param state the current state
     * @return the successor state
     */
    byte[] getSuccessor(byte[] state);

}
