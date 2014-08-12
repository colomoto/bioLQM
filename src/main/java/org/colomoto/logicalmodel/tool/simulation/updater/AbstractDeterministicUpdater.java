package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.tool.simulation.DeterministicUpdater;

import java.util.List;

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
