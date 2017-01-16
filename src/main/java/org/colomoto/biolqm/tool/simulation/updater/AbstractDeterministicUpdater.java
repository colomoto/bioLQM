package org.colomoto.biolqm.tool.simulation.updater;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.DeterministicUpdater;

/**
 * Base class to build updaters with a single successor.
 *
 * @author Aurelien Naldi
 */
public abstract class AbstractDeterministicUpdater extends BaseUpdater implements DeterministicUpdater {

    public AbstractDeterministicUpdater(LogicalModel model) {
        super(model);
    }

}
