package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.tool.simulation.RandomUpdater;

import java.util.List;
import java.util.Random;

/**
 * Base class for random updaters.
 *
 * @author Aurelien Naldi
 */
public abstract class AbstractRandomUpdater extends BaseUpdater implements RandomUpdater {

    // random generator
    protected final Random random = new Random();

    public AbstractRandomUpdater(LogicalModel model) {
        super(model);
    }

    @Override
    public void setSeed(long seed) {
        random.setSeed(seed);
    }
}
