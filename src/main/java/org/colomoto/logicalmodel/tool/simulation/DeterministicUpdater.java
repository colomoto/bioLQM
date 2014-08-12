package org.colomoto.logicalmodel.tool.simulation;

/**
 * Interface for determinist updaters.
 * A determinist updater is an updater which always returns the same single successor for a given state.
 * This interface does not add any method, it only serves to specify that the updater is determinist (vs random).
 *
 * @author Aurelien Naldi
 */
public interface DeterministicUpdater extends SingleSuccessorUpdater {

}
