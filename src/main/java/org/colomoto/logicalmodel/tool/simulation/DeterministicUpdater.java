package org.colomoto.logicalmodel.tool.simulation;

/**
 * Interface for deterministic updaters which always return the same single successor for a given state.
 *
 * @author Aurelien Naldi
 */
public interface DeterministicUpdater {

    /**
     * Get the successor of a state state.
     * @param state the current state
     * @return the successor state
     */
    byte[] getSuccessor(byte[] state);

}
