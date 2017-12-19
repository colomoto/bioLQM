package org.colomoto.biolqm.tool.simulation;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.deterministic.DeterministicUpdater;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.MultipleSuccessorsUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdater;

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
