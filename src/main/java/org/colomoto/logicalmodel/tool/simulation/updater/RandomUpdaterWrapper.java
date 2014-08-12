package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.tool.simulation.MultipleSuccessorsUpdater;
import org.colomoto.logicalmodel.tool.simulation.RandomUpdater;

import java.util.List;
import java.util.Random;

/**
 * Random updater which wraps a MultipleSuccessorUpdater and picks one of the successors.
 *
 * @author Aurelien Naldi
 */
public class RandomUpdaterWrapper implements RandomUpdater {

    MultipleSuccessorsUpdater updater;
    Random random = new Random();

    public RandomUpdaterWrapper(MultipleSuccessorsUpdater updater) {
        this.updater = updater;
    }

    @Override
    public byte[] pickSuccessor(byte[] state) {
        List<byte[]> successors = updater.getSuccessors(state);

        if (successors == null || successors.size() == 0) {
            return null;
        }

        int l = successors.size();
        if (l == 1) {
            return successors.get(0);
        }

        return successors.get( random.nextInt(l) );
    }

    @Override
    public void setSeed(long seed) {
        random.setSeed(seed);
    }
}
