package org.colomoto.logicalmodel.tool.simulation.updater;

import org.colomoto.logicalmodel.LogicalModel;
import org.colomoto.logicalmodel.tool.simulation.SingleSuccessorUpdater;

import java.util.List;

/**
 * Base class to build updaters with a single successor.
 *
 * @author Aurelien Naldi
 */
public abstract class AbstractSingleSuccessorUpdater extends BaseUpdater implements SingleSuccessorUpdater {


    public AbstractSingleSuccessorUpdater(LogicalModel model) {
        super(model);
    }

    public List<byte[]> getSuccessors(byte[] state) {
        byte[] next = getSuccessor(state);
        if (next == null) {
            return BaseUpdater.getEmptySuccessors();
        }

        return BaseUpdater.getSingleSuccessors(next);
    }

}
