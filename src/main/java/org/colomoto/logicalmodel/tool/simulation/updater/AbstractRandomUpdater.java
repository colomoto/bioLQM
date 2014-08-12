package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.tool.simulation.RandomUpdater;

import java.util.Random;

/**
 * Base class for random updaters.
 *
 * @author Aurelien Naldi
 */
public abstract class AbstractRandomUpdater extends AbstractSingleSuccessorUpdater implements RandomUpdater {

    // random generator
    private final Random random = new Random();

    public AbstractRandomUpdater(LogicalModel model) {
        super(model);
    }

    /**
     * Set the seed for the random generator (for reproducibility in tests, NOT for real use)
     *
     * @param seed
     */
    public void setSeed(long seed) {
        random.setSeed(seed);
    }

    protected int getRandomInt(int max) {
        return random.nextInt();
    }
}
