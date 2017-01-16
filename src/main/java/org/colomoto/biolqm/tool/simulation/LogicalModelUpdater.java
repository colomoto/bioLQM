package org.colomoto.biolqm.tool.simulation;

import org.colomoto.biolqm.LogicalModel;

/**
 * Common interface for all updaters.
 * Note that this interface is useless by itself and should not be directly implemented.
 *
 * @see MultipleSuccessorsUpdater
 * @see DeterministicUpdater
 * @see RandomUpdater
 *
 * @author Aurelien Naldi
 */
public interface LogicalModelUpdater {

    LogicalModel getModel();
}
