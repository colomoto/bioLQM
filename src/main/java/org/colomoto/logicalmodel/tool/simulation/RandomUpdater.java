package org.colomoto.logicalmodel.tool.simulation;

/**
 * Interface for random (stochastic) updaters.
 * A determinist updater is an updater which returns a single successor for a given state,
 * but this successor may change when calling it again on the same state.
 *
 * @author Aurelien Naldi
 */
public interface RandomUpdater extends SingleSuccessorUpdater {

}
