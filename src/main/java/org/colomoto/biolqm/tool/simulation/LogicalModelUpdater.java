package org.colomoto.biolqm.tool.simulation;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.NodeInfo;
import org.colomoto.biolqm.tool.simulation.deterministic.DeterministicUpdater;
import org.colomoto.biolqm.tool.simulation.grouping.SplittingType;
import org.colomoto.biolqm.tool.simulation.multiplesuccessor.MultipleSuccessorsUpdater;
import org.colomoto.biolqm.tool.simulation.random.RandomUpdater;
import java.util.Map;

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

    void setFilter(Map<NodeInfo, SplittingType> filter);
}
