package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.tool.simulation.MultipleSuccessorsUpdater;

import java.util.List;

/**
 * Random updater which wraps a MultipleSuccessorUpdater and picks one of the successors.
 *
 * @author Aurelien Naldi
 */
public class RandomUpdaterWrapper extends AbstractRandomUpdater {

    MultipleSuccessorsUpdater updater;

    public RandomUpdaterWrapper(MultipleSuccessorsUpdater updater) {
        super(updater.getModel());
        this.updater = updater;
    }

    @Override
    public byte[] getSuccessor(byte[] state) {
        List<byte[]> successors = updater.getSuccessors(state);

        if (successors == null || successors.size() == 0) {
            return null;
        }

        int l = successors.size();
        if (l == 1) {
            return successors.get(0);
        }

        return successors.get( getRandomInt(l) );
    }

}
