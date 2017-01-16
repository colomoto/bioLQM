package org.colomoto.biolqm.tool.simulation.updater;

import org.colomoto.biolqm.LogicalModel;
import org.colomoto.biolqm.tool.simulation.RandomUpdater;

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
